-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of Michael and Scott's lock-free queue.
--  Copyright (C) 2004 - 2006  Anders Gidenstam
--
--  This program is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the Free Software
--  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : example_queue.adb
--  Description     : Simple example ADT for lock-free garbage reclamation
--                    schemes.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $Id: example_queue.adb,v 1.2 2006/12/01 10:40:10 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Example_Queue is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function Create_Queue_Node is new LFMR_Ops.Create (New_Queue_Node_Access);

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node) is
      use LFMR_Ops;
   begin
      Store (Node.Next'Access, Null_Reference);
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Free (Node : access Queue_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Queue_Node,
                                    New_Queue_Node_Access);
      function To_New_Queue_Node_Access is new
        Ada.Unchecked_Conversion (LFMR_Ops.Node_Access,
                                  New_Queue_Node_Access);

      X : New_Queue_Node_Access :=
        To_New_Queue_Node_Access (LFMR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Object_Value type from the Object_Value_Access2 pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   function All_References (Node : access Queue_Node)
                           return LFMR.Reference_Set is
      type Link_Access is access all Queue_Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  LFMR.Shared_Reference_Base_Access);

      Link : constant Link_Access :=
        Node.Next'Access;
      Result : constant LFMR.Reference_Set (1 .. 1) :=
        (1 => To_Shared_Reference_Access (Link));
   begin
      return Result;
   end All_References;

   ----------------------------------------------------------------------------
   procedure Init (Queue : in out Queue_Type) is
      use LFMR_Ops;
      Node : constant Queue_Node_Access := Create_Queue_Node;
   begin
      Store (Queue.Head'Access, Node);
      Store (Queue.Tail'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   function  Dequeue (Queue : access Queue_Type) return Value_Type is
      use LFMR_Ops;
      Node : Queue_Node_Access;
      Next : Queue_Node_Access;
      Res  : Value_Type;
   begin
      loop
         Node := Dereference (Queue.Head'Access);
         Next := Dereference ("+"(Node).Next'Access);
         if Next = Null_Reference then
            Release (Node);
            raise Queue_Empty;
         end if;
         exit when Compare_And_Swap (Link      => Queue.Head'Access,
                                     Old_Value => Node,
                                     New_Value => Next);
         Release (Node);
         Release (Next);
      end loop;
      Release (Node);
      Res := "+"(Next).Value;
      Release (Next);
      return Res;
   end Dequeue;

   ----------------------------------------------------------------------------
   procedure Enqueue (Queue : in out Queue_Type;
                      Value : in     Value_Type) is
      use LFMR_Ops;
      Node : constant Queue_Node_Access := Create_Queue_Node;
      Old, Prev, Prev2 : Queue_Node_Access;
   begin
      "+"(Node).Value := Value;
      Old  := Dereference (Queue.Tail'Access);
      Prev := Old;
      loop
         loop
            Prev2 := Dereference ("+"(Prev).Next'Access);
            exit when Prev2 = Null_Reference;

            if Old /= Prev then
               Release (Prev);
            end if;

            Prev := Prev2;
         end loop;

         exit when Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                     Old_Value => Null_Reference,
                                     New_Value => Node);
      end loop;

      Compare_And_Swap (Link      => Queue.Tail'Access,
                        Old_Value => Old,
                        New_Value => Node);

      if Old /= Prev then
         Release (Prev);
      end if;

      Release (Old);
      Release (Node);
   end Enqueue;

end Example_Queue;
