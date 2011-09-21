-------------------------------------------------------------------------------
--  Lock-Free Sets - An implementation of the lock-free set algorithm by
--                   M. Michael.
--
--  Copyright (C) 2006 - 2011  Anders Gidenstam
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
--  Filename        : nbada-lock_free_sets.adb
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 12:23:47 2006
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body NBAda.Lock_Free_Sets is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => List_Node'Max_Size_In_Storage_Elements);

   type New_List_Node_Access is access List_Node;
   for New_List_Node_Access'Storage_Pool use Node_Pool;

   function Create_List_Node is new MR_Ops.Create (New_List_Node_Access);

   ----------------------------------------------------------------------------
   package Reference_Marks is
      new MR_Ops.Basic_Reference_Operations.Reference_Mark_Operations
     (MR_Ops.Private_Reference);
   use Reference_Marks;

   subtype Private_Reference is MR_Ops.Private_Reference;

   procedure Find (Set             : in     Set_Type;
                   Key             : in     Key_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out Private_Reference);
   --  Note: Prev, Cur, Next are Null_Reference or valid references that
   --        must be released. It is safe to Release a Null_Reference.

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Set : in out Set_Type) is
      use MR_Ops;
      Node : constant Private_Reference := Create_List_Node (Set.MM);
   begin
      Store (Set.Head'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Set_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is
      use MR_Ops;
      Node : constant Private_Reference := Create_List_Node (Into.MM);
   begin
      "+"(Node).Key   := Key;
      "+"(Node).Value := Value;
      loop
         declare
            Prev, Cur, Next : Private_Reference;
            Found           : Boolean;
         begin
            begin
               Find (Into, Key,
                     Found,
                     Prev, Cur, Next);

            exception
               when Constraint_Error =>
                  Ada.Text_IO.Put_Line ("Lock_Free_Set.Insert: " &
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
                     Ada.Text_IO.Put_Line ("Lock_Free_Set.Insert: " &
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
                     Ada.Text_IO.Put_Line ("Lock_Free_Set.Insert: " &
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
                     Ada.Text_IO.Put_Line ("Lock_Free_Set.Insert: " &
                                           "Release exception in " &
                                           "loop iteration!");
                     raise;
               end;
            end if;
         end;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From : in out Set_Type;
                      Key  : in     Key_Type) is
      use MR_Ops;
   begin
      loop
         declare
            Prev, Cur, Next : Private_Reference;
            Found           : Boolean;
         begin
            Find (From, Key,
                  Found,
                  Prev, Cur, Next);
            if not Found then
               Release (Prev);
               Release (Cur);
               Release (Next);
               raise Not_Found;
            end if;

            if
              Compare_And_Swap (Link      => "+"(Cur).Next'Access,
                                Old_Value => Unmark (Next),
                                New_Value => Mark (Next))
            then
               --  Dechain the logically deleted node.
               Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                 Old_Value => Unmark (Cur),
                                 New_Value => Unmark (Next));
               Delete (Cur);
               Release (Prev);
               Release (Next);
               return;
            end if;
            Release (Prev);
            Release (Cur);
            Release (Next);
         end;
      end loop;
   end Delete;

   ----------------------------------------------------------------------------
   function  Find    (In_Set : in Set_Type;
                      Key    : in Key_Type) return Value_Type is
      use MR_Ops;
      Prev, Cur, Next : Private_Reference;
      Found           : Boolean;
   begin
      Find (In_Set, Key,
            Found,
            Prev, Cur, Next);
      if Found then
         declare
            Value : constant Value_Type := "+"(Cur).Value;
         begin
            Release (Prev);
            Release (Cur);
            Release (Next);
            return Value;
         end;
      else
         Release (Prev);
         Release (Cur);
         Release (Next);
         raise Not_Found;
      end if;
   end Find;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Free     (Node : access List_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (List_Node,
                                    New_List_Node_Access);
      function To_New_List_Node_Access is new
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_List_Node_Access);

      X : New_List_Node_Access :=
        To_New_List_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  List_Node type from the New_List_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   procedure Find (Set             : in     Set_Type;
                   Key             : in     Key_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out Private_Reference) is
      use MR_Ops;

      type Mutable_Access is access all List_Node_Reference;
      type Immutable_Access is access constant List_Node_Reference;
      function To_Mutable is new Ada.Unchecked_Conversion
        (Immutable_Access, Mutable_Access);

      Mutable_Set_Head : constant Mutable_Access :=
        To_Mutable (Set.Head'Access);
   begin
      Prev := Null_Reference;
      Cur  := Null_Reference;
      Next := Null_Reference;
      loop

         declare
            Previous : Private_Reference := Dereference (Set.MM,
                                                         Mutable_Set_Head);
            Current  : Private_Reference := Null_Reference;
         begin
            --  Previous is a dummy node always present at the head of the
            --  list.
            if "+"(Previous) = null then
               --  This should not happen!
               raise Constraint_Error;
            end if;
            Current  := Dereference (Set.MM,
                                     "+"(Previous).Next'Access);

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

               Next := Dereference (Set.MM,
                                    "+"(Current).Next'Access);
               if "+"(Previous).Next /= Unmark (Current) then
                  --  Retry from the list head.
                  exit Traverse;
               else
                  if not Is_Marked (Next) then
                     if not ("+"(Current).Key < Key) then
                        Found :=  "+"(Current).Key = Key;
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
                     --  deleted. Help dechain Current.
                     --  Note: The node that marked Current as logically
                     --  deleted is responsible for reclaiming it.
                     if
                       Compare_And_Swap
                       (Link      => "+"(Previous).Next'Access,
                        Old_Value => Unmark (Current),
                        New_Value => Unmark (Next))
                     then
                        Release (Current);
                        Current := Null_Reference;
                     else
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
      end loop;
   end Find;

end NBAda.Lock_Free_Sets;
