-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of  M. Hoffman, O. Shalev and
--                    N. Shavit's lock-free queue algorithm.
--  Copyright (C) 2008 - 2011  Anders Gidenstam
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
--  Filename        : nbada-lock_free_queues.adb
--  Description     : A lock-free queue algorithm based on
--                    M. Hoffman, O. Shalev and N. Shavit,
--                    "The Baskets Queue", The 11th International Conference
--                    On the Principles Of Distributed Systems (OPODIS'07),
--                    LNCS 4878, pp. 401-414, 2007.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jan 10 17:16:33 2008
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

with Ada.Text_IO;

package body NBAda.Lock_Free_Queues is

   -------------------------------------------------------------------------
   --  NOTE: The original Baskets Queue algorithm used tags and double
   --  width CAS to avoid ABA problems w.r.t. the nodes. The
   --  implementation here uses a lock-free memory reclamation scheme
   --  for the nodes instead. This leads to some changes in the algorithm.
   --
   --------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function New_Node is new MR_Ops.Create
     (User_Node_Access => New_Queue_Node_Access);

   ----------------------------------------------------------------------------
   package Reference_Marks is
      new MR_Ops.Basic_Reference_Operations.Reference_Mark_Operations
          (MR_Ops.Private_Reference);
   use Reference_Marks;

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Queue : in out Queue_Type) is
      use MR_Ops;
      Node :  constant Private_Reference := New_Node (Queue.MM);
   begin
      Store ("+" (Node).Next'Access, Mark (Null_Reference));
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
              Dereference (From.MM,
                           From.Head'Access);
            Tail : constant Private_Reference :=
              Dereference (From.MM,
                           From.Tail'Access);
            Next : Private_Reference :=
              Dereference (From.MM,
                           "+" (Head).Next'Access);
         begin
            if Head = From.Head then
               if Head = Tail then
                  if "+" (Next) = null then
                     Release (Next);
                     Release (Tail);
                     Release (Head);
                     raise Queue_Empty;
                  end if;
                  --  From.Tail is lagging. Help updating it.
                  while Tail = From.Tail loop
                     declare
                        Next_Next : constant Private_Reference :=
                          Dereference (From.MM,
                                       "+" (Next).Next'Access);
                     begin
                        if "+" (Next_Next) /= null then
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
                     --  Search for the first unmarked node.
                     while
                       "+" (Next) /= null and Is_Marked (Next) and
                       Head = From.Head
                     loop
                        Release (Node);
                        Node := Next;
                        Next := Dereference (From.MM,
                                             "+" (Node).Next'Access);
                     end loop;

                     if Head = From.Head then
                        if Is_Marked (Next) then
                           --  Update From.Head.
                           Compare_And_Swap (Link      => From.Head'Access,
                                             Old_Value => Head,
                                             New_Value => Unmark (Node));
                        else
                           --  Node is an active node.
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
                                    New_Value => Unmark (Node));
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
      Node :  constant Private_Reference := New_Node (On.MM);
   begin
      "+" (Node).Element := Element;

      loop
         declare
            Tail : constant Private_Reference :=
              Dereference (On.MM,
                           On.Tail'Access);
            Next : Private_Reference :=
              Dereference (On.MM,
                           "+" (Tail).Next'Access);
         begin
            if Tail = On.Tail then
               if "+" (Next) = null then
                  --  Attempt standard enqueue.
                  --  Preserve any deletion mark since the front dummy
                  --  node should always be marked.
                  declare
                     New_Value : Private_Reference;
                  begin
                     if Is_Marked (Next) then
                        New_Value := Mark (Node);
                     else
                        New_Value := Node;
                     end if;

                     if Compare_And_Swap
                       (Link      => "+" (Tail).Next'Access,
                        Old_Value => Next,
                        New_Value => New_Value)
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
                        while not Is_Marked ("+" (Tail).Next'Access) loop
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
                           Next := Dereference (On.MM,
                                                "+" (Tail).Next'Access);
                        end loop;
                        --  The basket got dequeued. Retry from start.
                     end if;
                  end;
               else
                  --  On.Tail is lagging. Help updating it.
                  while  Tail = On.Tail loop
                     declare
                        Next_Next : constant Private_Reference :=
                          Dereference (On.MM,
                                       "+" (Next).Next'Access);
                     begin
                        if "+" (Next_Next) /= null then
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
                                       New_Value => Unmark (Next));
                  end if;
               end if;
            end if;

            Release (Next);
            Release (Tail);
         end;
      end loop;
   end Enqueue;

   ----------------------------------------------------------------------------
   procedure Verify (Queue : in out Queue_Type;
                     Print : in     Boolean := False) is
      use MR_Ops;

      function Image (Link : Queue_Node_Reference) return String;
      function Image (Node : Private_Reference) return String;

      -----------------------------------------------------------------
      function Image (Link : Queue_Node_Reference) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (Queue_Node_Reference,
                                          NBAda.Primitives.Standard_Unsigned);
         use NBAda.Primitives;
      begin
         return Standard_Unsigned'Image (To_Unsigned (Link));
      end Image;

      -----------------------------------------------------------------
      function Image (Node : Private_Reference) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                          NBAda.Primitives.Standard_Unsigned);
         use NBAda.Primitives;
      begin
         declare
            Text : constant String :=
              Standard_Unsigned'Image (To_Unsigned ("+" (Node))) &
              "@(" &
              "Next = " &
              Image ("+" (Node).Next) &
              ")";
         begin
            return
              Text;
         end;
      end Image;


      Node : Private_Reference := Dereference (Queue.MM,
                                               Queue.Head'Access);
   begin
      if Print then
         Ada.Text_IO.Put_Line ("Head = " & Image (Queue.Head));
         Ada.Text_IO.Put_Line ("Tail = " & Image (Queue.Tail));
      end if;
      loop
         if Print then
            Ada.Text_IO.Put_Line ("  " & Image (Node));
         end if;
         declare
            Next : constant Private_Reference :=
              Dereference (Queue.MM,
                           "+" (Node).Next'Access);
         begin
            exit when "+" (Next) = null;

            Release (Node);
            Node := Next;
         end;
      end loop;
      Release (Node);
   end Verify;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
   begin
      if not Concurrent then
         Store (Node.Next'Access, Null_Reference);
      else
         declare
            Tmp : BRO.Unsafe_Reference_Value;
         begin
            loop
               Tmp := BRO.Unsafe_Read (Node.Next'Access);
               exit when Compare_And_Swap (Link      => Node.Next'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Clean_Up (MM   : in     MR.Memory_Manager_Base'Class;
                       Node : access Queue_Node) is
      use MR_Ops;
      RMM             : MR_Ops.Memory_Manager
        renames MR_Ops.Memory_Manager (MM);
      Next, Next_Next : Private_Reference;
   begin
      loop
         Next := Dereference (RMM,
                              Node.Next'Access);
         if "+" (Next) /= null and then Is_Deleted ("+" (Next)) then
            Next_Next := Dereference (RMM,
                                      "+" (Next).Next'Access);
            if Compare_And_Swap (Link      => Node.Next'Access,
                                 Old_Value => Next,
                                 New_Value => Mark (Next_Next))
            then
               null;
            end if;
            Release (Next_Next);
            Release (Next);
         else
            Release (Next);
            exit;
         end if;
      end loop;
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
