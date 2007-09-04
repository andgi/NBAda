-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
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
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : example_queue.adb
--  Description     : Simple example ADT for lock-free garbage reclamation
--                    scheme.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $Id: example_queue.adb,v 1.10 2007/09/04 12:03:59 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Example_Queue is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : NBAda.Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function Create_Queue_Node is new LFMR_Ops.Create (New_Queue_Node_Access);

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      use LFMR_Ops;
      Next : Queue_Node_Access;
   begin
      if not Concurrent then
         Store (Node.Next'Access, Null_Reference);
      else
         loop
            Next := Dereference (Node.Next'Access);
            exit when Compare_And_Swap (Link      => Node.Next'Access,
                                        Old_Value => Next,
                                        New_Value => Null_Reference);
            Release (Next);
         end loop;
         Release (Next);
      end if;
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Clean_Up (Node : access Queue_Node) is
      use LFMR_Ops;
      Node1, Node2 : Queue_Node_Access;
   begin
      loop
         Node1 := Dereference (Node.Next'Access);
         if Node1 /= Null_Reference and then Is_Deleted (+Node1) then
            Node2 := Dereference ("+"(Node1).Next'Access);
            if Compare_And_Swap (Link      => Node.Next'Access,
                                 Old_Value => Node1,
                                 New_Value => Node2)
            then
               null;
            end if;
            Release (Node2);
            Release (Node1);
         else
            Release (Node1);
            exit;
         end if;
      end loop;
   end Clean_Up;

   -------------------------------------------------------------------------
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
      Delete (Node);
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
