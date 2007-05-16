-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of Michael and Scott's lock-free queue.
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--  Filename        : lock_free_queues.adb
--  Description     : An Ada implementation of Michael and Scott's
--                    lock-free queue algorithm.
--  Author          : Anders Gidenstam
--  Created On      : Tue Nov 28 14:35:35 2006
--  $Id: nbada-lock_free_queues.adb,v 1.3 2007/05/16 12:28:51 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Lock_Free_Growing_Storage_Pools;

package body Lock_Free_Queues is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function New_Node is new MR_Ops.Create
     (User_Node_Access => New_Queue_Node_Access);

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Queue : in out Queue_Type) is
      use MR_Ops;
      Node :  constant Private_Reference := New_Node;
   begin
      Store (Queue.Head'Access, Node);
      Store (Queue.Tail'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   function  Dequeue (From : access Queue_Type) return Element_Type is
      use MR_Ops;
   begin
      loop
         declare
            Head : constant Private_Reference :=
              Dereference (From.Head'Access);
            Tail : constant Private_Reference :=
              Dereference (From.Tail'Access);
            Next : constant Private_Reference :=
              Dereference ("+" (Head).Next'Access);
         begin
            if Head = From.Head then
               if Head = Tail then
                  if Next = Null_Reference then
                     Release (Next);
                     Release (Tail);
                     Release (Head);
                     raise Queue_Empty;
                  end if;

                  Void_Compare_And_Swap (Link      => From.Tail'Access,
                                         Old_Value => Tail,
                                         New_Value => Next);
               else
                  declare
                     Result : constant Element_Type := "+" (Next).Element;
                  begin
                     if
                       Boolean_Compare_And_Swap (Link      => From.Head'Access,
                                                 Old_Value => Head,
                                                 New_Value => Next)
                     then
                        Release (Next);
                        Release (Tail);
                        Delete  (Head);
                        return Result;
                     end if;
                  end;
               end if;
            end if;

            Release (Next);
            Release (Tail);
            Release (Head);
         end;
      end loop;
   end Dequeue;

   ----------------------------------------------------------------------------
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type) is
      use MR_Ops;
      Node :  constant Private_Reference := New_Node;
   begin
      "+" (Node).Element := Element;

      loop
         declare
            Tail : constant Private_Reference :=
              Dereference (On.Tail'Access);
            Next : constant Private_Reference :=
              Dereference ("+" (Tail).Next'Access);
         begin
            if Tail = On.Tail then
               if Next = Null_Reference then
                  if Boolean_Compare_And_Swap
                    (Link      => "+" (Tail).Next'Access,
                     Old_Value => Next,
                     New_Value => Node)
                  then
                     Void_Compare_And_Swap (Link      => On.Tail'Access,
                                            Old_Value => Tail,
                                            New_Value => Node);
                     Release (Next);
                     Release (Tail);
                     Release (Node);
                     return;
                  end if;
               else
                  Void_Compare_And_Swap (Link      => On.Tail'Access,
                                         Old_Value => Tail,
                                         New_Value => Next);
               end if;
            end if;

            Release (Next);
            Release (Tail);
         end;
      end loop;
   end Enqueue;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Free (Node : access Queue_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Queue_Node,
                                    New_Queue_Node_Access);
      function To_New_Queue_Node_Access is new
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_Queue_Node_Access);
      X : New_Queue_Node_Access :=
        To_New_Queue_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Queue_Node type from the New_Queue_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end Lock_Free_Queues;
