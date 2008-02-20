-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - Based on the the lock-free set algorithm by
--                              M. Michael.
--
--  Copyright (C) 2006 - 2008  Anders Gidenstam
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_priority_queues.adb
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 12:23:47 2006
--  $Id: nbada-lock_free_priority_queues.adb,v 1.1 2008/02/20 20:55:38 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body NBAda.Lock_Free_Priority_Queues is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => List_Node'Max_Size_In_Storage_Elements);

   type New_List_Node_Access is access List_Node;
   for New_List_Node_Access'Storage_Pool use Node_Pool;

   function Create_List_Node is new MRS_Ops.Create (New_List_Node_Access);

   procedure Find (Queue           : in     Priority_Queue_Type;
                   Key             : in     Element_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out List_Node_Access);
   --  Note: Prev, Cur, Next are Null_Reference or valid references that
   --        must be released. It is safe to Release a Null_Reference.

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Initialize (Queue : in out Priority_Queue_Type) is
      use MRS_Ops;
      Node : constant List_Node_Access := Create_List_Node;
   begin
      Store (Queue.Head'Access, Node);
      Release (Node);
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Insert  (Into    : in out Priority_Queue_Type;
                      Element : in     Element_Type) is
      use MRS_Ops;
      Node : constant List_Node_Access := Create_List_Node;
   begin
      "+"(Node).Value := Element;
      loop
         declare
            Prev, Cur, Next : List_Node_Access;
            Found           : Boolean;
         begin
            begin
               Find (Into, Element,
                     Found,
                     Prev, Cur, Next);

            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Lock_Free_Priority_Queue.Insert: " &
                                        "Release exception in " &
                                        "Find call!");
                  raise;
            end;
            if Found then
               declare
                  Step : Natural := 0;
               begin
                  Release (Prev);
                  Step := Step + 1;
                  Release (Cur);
                  Step := Step + 1;
                  Release (Next);
                  Step := Step + 1;
                  Delete  (Node);

               exception
                  when Constraint_Error =>
                     Ada.Text_IO.Put_Line
                       ("Lock_Free_Priority_Queue.Insert: " &
                        "Release exception at " &
                        "Already_Present! (Step " &
                        Natural'Image (Step) & ")");
                     raise;
               end;
               raise Already_Present;
            end if;
            Store ("+"(Node).Next'Access, Unmark (Cur));
            if
              Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                Old_Value => Unmark (Cur),
                                New_Value => Node)
            then
               declare
                  Step : Natural := 0;
               begin
                  Release (Prev);
                  Step := Step + 1;
                  Release (Next);
                  Step := Step + 1;
                  Release (Cur);
                  Step := Step + 1;
                  Release (Node);

               exception
                  when Constraint_Error =>
                     Ada.Text_IO.Put_Line
                       ("Lock_Free_Priority_Queue.Insert: " &
                        "Release exception at " &
                        "done! (Step " &
                        Natural'Image (Step) & ")");
                     raise;
               end;
               return;
            else
               begin
                  Release (Prev);
                  Release (Cur);
                  Release (Next);

               exception
                  when Constraint_Error =>
                     Ada.Text_IO.Put_Line
                       ("Lock_Free_Priority_Queue.Insert: " &
                        "Release exception in " &
                        "loop iteration!");
                     raise;
               end;
            end if;
         end;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete_Min (From    : in out Priority_Queue_Type;
                         Element :    out Element_Type) is
   begin
      Element := Delete_Min (From.Mutable.Self);
   end Delete_Min;

   ----------------------------------------------------------------------------
   function  Delete_Min (From : in Priority_Queue_Type)
                        return Element_Type is
   begin
      return Delete_Min (From.Mutable.Self);
   end Delete_Min;

   ----------------------------------------------------------------------------
   function  Delete_Min (From : access Priority_Queue_Type)
                        return Element_Type is

      use MRS_Ops;
      Head : constant List_Node_Access := Dereference (From.Head'Access);
   begin
      loop
         declare
            Node : constant List_Node_Access :=
              Dereference ("+" (Head).Next'Access);
         begin
            if "+" (Node) = null then
               Release (Node);
               Release (Head);
               raise Queue_Empty;
            end if;

            declare
               Node_Next : constant List_Node_Access :=
                 Dereference ("+" (Node).Next'Access);
               --  Note: An Unsafe_Read would be safe here.
               Value     : constant Element_Type := "+" (Node).Value;
            begin
               if not Is_Marked (Node_Next) then
                  if
                    Compare_And_Swap (Link      => "+" (Node).Next'Access,
                                      Old_Value => Node_Next,
                                      New_Value => Mark (Node_Next))
                  then
                     --  We marked the node. Now dechain it.
                     Compare_And_Swap (Link      => "+" (Head).Next'Access,
                                       Old_Value => Node,
                                       New_Value => Node_Next);
                     Release (Node_Next);
                     Delete  (Node);
                     Release (Head);
                     return Value;
                  end if;

               else
                  --  Help dechain the deleted node.
                  Compare_And_Swap
                    (Link      => "+" (Head).Next'Access,
                     Old_Value => Node,
                     New_Value => Unmark (Node_Next));
               end if;

               Release (Node_Next);
            end;
            Release (Node);
         end;
      end loop;
   end Delete_Min;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Verify (Queue : in out Priority_Queue_Type;
                     Print : in     Boolean := False) is
   begin
      null;
   end Verify;

   ----------------------------------------------------------------------------
   procedure Free     (Node : access List_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (List_Node,
                                    New_List_Node_Access);
      function To_New_List_Node_Access is new
        Ada.Unchecked_Conversion (MRS_Ops.Node_Access,
                                  New_List_Node_Access);

      X : New_List_Node_Access :=
        To_New_List_Node_Access (MRS_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  List_Node type from the New_List_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   procedure Find (Queue           : in     Priority_Queue_Type;
                   Key             : in     Element_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out List_Node_Access) is
      use MRS_Ops;
   begin
      Prev := Null_Reference;
      Cur  := Null_Reference;
      Next := Null_Reference;

      Start_Over :
      loop

         declare
            Previous : List_Node_Access :=
              Dereference (Queue.Mutable.Self.Head'Access);
            Current  : List_Node_Access := Null_Reference;
         begin
            --  Previous is a dummy node always present at the head of the
            --  list.
            Current  := Dereference ("+"(Previous).Next'Access);

            Traverse :
            loop
               if "+"(Current) = null then
                  Found := False;
                  Prev  := Previous;
                  Cur   := Null_Reference;
                  Next  := Null_Reference;
                  --  Prev is a valid reference. Cur and Next are null.
                  return;
               end if;

               Next := Dereference ("+"(Current).Next'Access);
               if "+"(Previous).Next /= Unmark (Current) then
                  --  Retry from the list head.
                  exit Traverse;
               else
                  if not Is_Marked (Next) then
                     if not ("+"(Current).Value < Key) then
                        Found :=  "+"(Current).Value = Key;
                        --  Prev is a valid reference. Cur and Next are valid
                        --  references or null.
                        Prev := Previous;
                        Cur  := Current;
                        return;
                     else
                        declare
                           Tmp : constant Private_Reference := Previous;
                        begin
                           Previous := Current;
                           Current  := Null_Reference;
                           Release (Tmp);
                        end;
                     end if;
                  else
                     --  Current.Next is marked, i.e. Current is logically
                     --  deleted.
                     if
                       Compare_And_Swap
                       (Link      => "+"(Previous).Next'Access,
                        Old_Value => Unmark (Current),
                        New_Value => Unmark (Next))
                     then
                        --  Successfully dechained Current. I.e. we are still
                        --  in the active part.
                        Release (Current);
                        Current := Null_Reference;
                     else
                        --  Could have lost the active part of the structure.
                        --  Retry from the list head.
                        exit Traverse;
                     end if;
                  end if;
               end if;
               Release (Current);
               Current := Next;
               Next    := Null_Reference;
            end loop Traverse;

            --  Clean-up before next iteration.
            Release (Previous);
            Release (Current);
            Release (Next);
            Next := Null_Reference;
         end;
      end loop Start_Over;
   end Find;

end NBAda.Lock_Free_Priority_Queues;
