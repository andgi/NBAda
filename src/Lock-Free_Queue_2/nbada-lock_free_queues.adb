-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of  M. Hoffman, O. Shalev and
--                    N. Shavit's lock-free queue algorithm.
--  Copyright (C) 2008  Anders Gidenstam
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
--  Filename        : nbada-lock_free_queues.adb
--  Description     : A lock-free queue algorithm based on
--                    M. Hoffman, O. Shalev and N. Shavit,
--                    "The Baskets Queue", The 11th International Conference
--                    On the Principles Of Distributed Systems (OPODIS'07),
--                    LNCS 4878, pp. 401-414, 2007.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jan 10 17:16:33 2008
--  $Id: nbada-lock_free_queues.adb,v 1.1 2008/01/10 19:38:44 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;

package body NBAda.Lock_Free_Queues is

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
            Next : Private_Reference :=
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
                  --  From.Tail is lagging. Help updating it.
                  while Tail = From.Tail loop
                     declare
                        Next_Next : constant Private_Reference :=
                          Dereference ("+" (Next).Next'Access);
                     begin
                        if Next_Next /= Null_Reference then
                           Release (Next);
                           Next := Next_Next;
                        else
                           Release (Next_Next);
                           exit;
                        end if;
                     end;
                  end loop;

                  if  Tail = From.Tail then
                     Compare_And_Swap (Link      => From.Tail'Access,
                                       Old_Value => Tail,
                                       New_Value => Next);
                  end if;

               else

                  --  Attempt to dequeue a node.
                  declare
                     Node : Private_Reference := Copy (Head);
                  begin
                     while
                       Is_Marked (Next) and Node /= Tail and Head = From.Head
                     loop
                        Release (Node);
                        Node := Next;
                        Next := Dereference ("+" (Node).Next'Access);
                     end loop;

                     if Head = From.Head then
                        if Node = Tail then
                           --  Update From.Head.
                           Compare_And_Swap (Link      => From.Head'Access,
                                             Old_Value => Head,
                                             New_Value => Node);
                        else
                           declare
                              Result : constant Element_Type :=
                                "+" (Node).Element;
                           begin
                              if
                                Compare_And_Swap
                                (Link      => "+" (Node).Next'Access,
                                 Old_Value => Next,
                                 New_Value => Mark (Next))
                              then
                                 Release (Next);
                                 Release (Tail);
                                 --  Update From.Head.
                                 Compare_And_Swap
                                   (Link      => From.Head'Access,
                                    Old_Value => Head,
                                    New_Value => Node);
                                 Release (Head);
                                 Delete  (Node);
                                 return Result;
                              end if;
                           end;
                        end if;
                     end if;
                     Release (Node);
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
            Next : Private_Reference :=
              Dereference ("+" (Tail).Next'Access);
         begin
            if Tail = On.Tail then
               if Next = Null_Reference then
                  --  Attempt standard enqueue.
                  if Compare_And_Swap
                    (Link      => "+" (Tail).Next'Access,
                     Old_Value => Null_Reference,
                     New_Value => Node)
                  then
                     Compare_And_Swap (Link      => On.Tail'Access,
                                       Old_Value => Tail,
                                       New_Value => Node);
                     Release (Next);
                     Release (Tail);
                     Release (Node);
                     return;
                  else
                     --  Foiled by concurrent operation. Attempt to go
                     --  into the basket.
                     while not Is_Marked ("+" (Tail).Next) loop
                        --  Back-off.

                        Store ("+" (Node).Next'Access, Next);
                        if Compare_And_Swap
                          (Link      => "+" (Tail).Next'Access,
                           Old_Value => Next,
                           New_Value => Node)
                        then
                           Release (Next);
                           Release (Tail);
                           Release (Node);
                           return;
                        end if;

                        Release (Next);
                        Next := Dereference ("+" (Tail).Next'Access);
                     end loop;
                     --  The basket got dequeued. Retry from start.
                  end if;
               else
                  --  On.Tail is lagging. Help updating it.
                  while  Tail = On.Tail loop
                     declare
                        Next_Next : constant Private_Reference :=
                          Dereference ("+" (Next).Next'Access);
                     begin
                        if Next_Next /= Null_Reference then
                           Release (Next);
                           Next := Next_Next;
                        else
                           Release (Next_Next);
                           exit;
                        end if;
                     end;
                  end loop;

                  if  Tail = On.Tail then
                     Compare_And_Swap (Link      => On.Tail'Access,
                                       Old_Value => Tail,
                                       New_Value => Next);
                  end if;
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
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      use MR_Ops;
   begin
      if not Concurrent then
         Store (Node.Next'Access, Null_Reference);
      else
         declare
            Tmp : MR_Ops.Unsafe_Reference_Value;
         begin
            loop
               Tmp := Unsafe_Read (Node.Next'Access);
               exit when Compare_And_Swap (Link      => Node.Next'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Clean_Up (Node : access Queue_Node) is
   begin
      null;
   end Clean_Up;

   ----------------------------------------------------------------------------
   function All_References (Node : access Queue_Node)
                           return MR.Reference_Set is
      type Link_Access is access all Queue_Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  MR.Shared_Reference_Base_Access);

      Result : MR.Reference_Set (1 .. 1);

   begin
      Result (Integer (1)) :=
        To_Shared_Reference_Access (Node.Next'Access);

      return Result;
   end All_References;

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

end NBAda.Lock_Free_Queues;
