-------------------------------------------------------------------------------
--  Lock-Free Simple Trees - An implementation of a lock-free simple
--                           tree algorithm by A. Gidenstam.
--
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_simple_trees.adb
--  Description     : Lock-free algorithm for simple binary trees by
--                    Anders Gidenstam.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 21 23:22:26 2008
--  $Id: nbada-lock_free_simple_trees.adb,v 1.4 2008/03/14 16:24:15 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body NBAda.Lock_Free_Simple_Trees is

   ----------------------------------------------------------------------------
   --  Storage pools for the nodes.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------
   Node_State_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Node_State'Max_Size_In_Storage_Elements);

   type New_Node_State_Access is access Node_State;
   for New_Node_State_Access'Storage_Pool use Node_State_Pool;

   function Create_Node_State is
      new State_Ops.Create (New_Node_State_Access);

   ----------------------------------------------------------------------
   Tree_Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
       (Block_Size => Tree_Node'Max_Size_In_Storage_Elements);

   type New_Tree_Node_Access is access Tree_Node;
   for New_Tree_Node_Access'Storage_Pool use Tree_Node_Pool;

   function Create_Tree_Node is
      new Node_Ops.Create (New_Tree_Node_Access);

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   function New_Copy (State : State_Ops.Private_Reference)
                     return State_Ops.Private_Reference;
   --  Create a copy of State. Both State and the new copy must be released.

   function  Find_Parent (Root : access Node_Reference;
                          Node : in     Node_Ops.Private_Reference;
                          Key  : in     Key_Type)
                         return Node_Ops.Private_Reference;
   --  Root is released. Node is not released.
   --  The returned private reference must be released.

   procedure Remove_Node (Root      : access Node_Reference;
                          Node      : in     Node_Ops.Private_Reference;
                          Old_State : in     State_Ops.Private_Reference);
   --  Dechain and delete a marked node.
   --  Node and Old_State are both released.

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Dictionary : in out Dictionary_Type) is
   begin
      null;
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is

      procedure Traverse_And_Insert (Root : in     Node_Ops.Private_Reference;
                                     Done :    out Boolean);
      --  Note:
      --    Root      is always released.
      --    New_Node  is released or deleted if Done = True;
      --    New_State is released or deleted if Done = True;

      use Node_Ops, State_Ops;
      New_Node  : constant Node_Ops.Private_Reference  := Create_Tree_Node;
      New_State : constant State_Ops.Private_Reference := Create_Node_State;

      ----------------------------------------------------------------------
      procedure Traverse_And_Insert (Root : in     Node_Ops.Private_Reference;
                                     Done :    out Boolean) is
         Current        : Node_Ops.Private_Reference := Root;
         Current_State  : State_Ops.Private_Reference;
         Next           : Node_Ops.Private_Reference;
         Null_Reference : Node_Ops.Private_Reference
           renames Node_Ops.Null_Reference;
      begin
         Done := False;
         Find_Leaf : loop
            Current_State := Dereference ("+" (Current).State'Access);
            if Is_Marked (Current_State) then
               --  Help removing this logically deleted node.
               if  "+" (Current_State) /= null then
                  Remove_Node (Into.Root'Access,
                               Current, Current_State);
               else
                  --  If the node state is null it has already been removed.
                  Release (Current_State);
                  Release (Current);
               end if;
               Current := Node_Ops.Null_Reference;
               exit Find_Leaf;
            end if;
            if Key < "+" (Current_State).Key then
               Next :=
                 Dereference ("+" (Current).Left'Access);
            elsif "+" (Current_State).Key < Key then
               Next :=
                 Dereference ("+" (Current).Right'Access);
            else
               --  Current.Key = Key. Update value.
               Store ("+" (New_Node).State'Access, State_Ops.Null_Reference);
               Delete (New_Node);
               if
                 Compare_And_Swap (Link      =>
                                     "+" (Current).State'Access,
                                   Old_Value => Current_State,
                                   New_Value => New_State)
               then
                  Delete  (Current_State);
                  Release (New_State);
               else
                  --  The relevant node's state was concurrently changed.
                  --  Consider this update overwritten. Is this really OK?!
                  Release (Current_State);
                  Delete  (New_State);
               end if;
               Release (Current);
               Done := True;
               return;
            end if;

            exit Find_Leaf when "+" (Next) = null;

            Release (Current_State);
            Release (Current);
            Current := Next;
            Next    := Null_Reference;
         end loop Find_Leaf;
         Release (Next); --  Next should be null here.

         if "+" (Current) /= null then
            declare
               Success : Boolean := False;
            begin
               --  Attempt to add New_Node below Current.
               if Key < "+" (Current_State).Key then
                  Success :=
                    Compare_And_Swap (Link => "+" (Current).Left'Access,
                                      Old_Value => Null_Reference,
                                      New_Value => New_Node);
               elsif "+" (Current_State).Key < Key then
                  Success :=
                    Compare_And_Swap (Link => "+" (Current).Right'Access,
                                      Old_Value => Null_Reference,
                                      New_Value => New_Node);
               end if;
               Release (Current_State);
               Release (Current);

               if Success then
                  Release (New_State);
                  Release (New_Node);
                  Done := True;
               end if;
               --  Insertion of New_Node failed due to concurrent operations.
               --  Retry from the root. Can we do better?
            end;
         end if;
      end Traverse_And_Insert;
      ----------------------------------------------------------------------

   begin
      --  Prepare the new node.
      "+" (New_State).Key   := Key;
      "+" (New_State).Value := Value;
      Store ("+" (New_Node).State'Access, New_State);

      loop
         declare
            Root : constant Node_Ops.Private_Reference :=
              Dereference (Into.Root'Access);
            Done : Boolean;
         begin
            if "+" (Root) = null then
               if
                 Compare_And_Swap (Link      => Into.Root'Access,
                                   Old_Value => Node_Ops.Null_Reference,
                                   New_Value => New_Node)
               then
                  Release (Root);
                  Release (New_State);
                  Release (New_Node);
                  return;
               end if;
            else
               --  Traverse to a leaf.
               Traverse_And_Insert (Root, Done);
               if Done then
                  return;
               end if;
            end if;
         end;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type) is
      Dummy : Value_Type;
   begin
      Dummy := Delete (From, Key);
   end Delete;

   ----------------------------------------------------------------------------
   function  Delete (From  : in Dictionary_Type;
                     Key   : in Key_Type)
                    return Value_Type is
      use Node_Ops, State_Ops;

      ----------------------------------------------------------------------
      Root  : constant Node_Ops.Private_Reference :=
        Dereference (From.Mutable.Self.Root'Access);
   begin
      if "+" (Root) = null then
         Release (Root);
         raise Not_Found;
      else
         --  Traverse the tree.
         declare
            Current       : Node_Ops.Private_Reference := Root;
            Current_State : State_Ops.Private_Reference;
            Next          : Node_Ops.Private_Reference;
         begin
            Traverse : loop
               Current_State := Dereference ("+" (Current).State'Access);
               if Key < "+" (Current_State).Key then
                  Next :=
                    Dereference ("+" (Current).Left'Access);
               elsif "+" (Current_State).Key < Key then
                  Next :=
                    Dereference ("+" (Current).Right'Access);
               else
                  --  Current.Key = Key. Set the deletion mark.
                  Mark : loop
                     if Is_Marked (Current_State) then
                        Release (Current_State);
                        Release (Current);
                        raise Not_Found;
                     end if;

                     if
                       Compare_And_Swap (Link    =>
                                           "+" (Current).State'Access,
                                         Old_Value => Current_State,
                                         New_Value =>
                                           State_Ops.Mark (Current_State))
                     then
                        declare
                           Value : constant Value_Type :=
                             "+" (Current_State).Value;
                        begin
                           Remove_Node (From.Mutable.Self.Root'Access,
                                        Current, Current_State);
                           return Value;
                        end;
                     else
                        Release (Current_State);
                        Current_State :=
                          Dereference ("+" (Current).State'Access);
                     end if;
                  end loop Mark;
               end if;

               Release (Current_State);
               Release (Current);

               if "+" (Next) = null then
                  --  Not found.
                  raise Not_Found;
               end if;

               Current := Next;
               Next    := Node_Ops.Null_Reference;
            end loop Traverse;
         end;
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function Delete_Min (From : in Dictionary_Type)
                       return Value_Type is
      Result : constant Pair_Type := Delete_Min (From);
   begin
      return Result.Value;
   end Delete_Min;

   ----------------------------------------------------------------------------
   function Delete_Min (From : in Dictionary_Type)
                       return Pair_Type is
      use Node_Ops, State_Ops;
   begin
      loop
         declare
            Root : constant Node_Ops.Private_Reference :=
              Dereference (From.Mutable.Self.Root'Access);
         begin
            if "+" (Root) = null then
               Release (Root);
               raise Not_Found;
            else
               --  Traverse the tree looking for the leftmost node.
               declare
                  Current       : Node_Ops.Private_Reference := Root;
                  Current_State : State_Ops.Private_Reference;
                  Next          : Node_Ops.Private_Reference;
               begin
                  Traverse : loop
                     Next := Dereference ("+" (Current).Left'Access);

                     if "+" (Next) = null then
                        Current_State :=
                          Dereference ("+" (Current).State'Access);
                        if "+" (Current).Left /= Next then
                           --  Abort this attempt. We could do better.
                           Release (Current_State);
                           Release (Current);
                           Current := Node_Ops.Null_Reference;
                        end if;
                        exit Traverse;
                     end if;

                     Release (Current);
                     Current := Next;
                     Next    := Node_Ops.Null_Reference;
                  end loop Traverse;

                  if "+" (Current) /= null then
                     --  Set the deletion mark.
                     Mark : loop
                        if Is_Marked (Current_State) then
                           --  Retry. Uhu.. live lock..
                           Release (Current_State);
                           Release (Current);
                           raise Not_Found; --  Let's break linearizability instead...
                           exit Mark;
                        end if;

                        if
                          Compare_And_Swap (Link    =>
                                              "+" (Current).State'Access,
                                            Old_Value => Current_State,
                                            New_Value =>
                                              State_Ops.Mark (Current_State))
                        then
                           declare
                              Value : constant Pair_Type :=
                                ("+" (Current_State).Key,
                                 "+" (Current_State).Value);
                           begin
                              Remove_Node (From.Mutable.Self.Root'Access,
                                           Current, Current_State);
                              return Value;
                           end;
                        else
                           Release (Current_State);
                           Current_State :=
                             Dereference ("+" (Current).State'Access);
                        end if;
                     end loop Mark;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Delete_Min;

   ----------------------------------------------------------------------------
   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type is
      use Node_Ops, State_Ops;
      Root : constant Node_Ops.Private_Reference :=
        Dereference (From.Mutable.Self.Root'Access);
   begin
      if "+" (Root) = null then
         Release (Root);
         raise Not_Found;
      else
         --  Traverse the tree.
         declare
            Current       : Node_Ops.Private_Reference := Root;
            Current_State : State_Ops.Private_Reference;
            Next          : Node_Ops.Private_Reference;
         begin
            Traverse : loop
               Current_State := Dereference ("+" (Current).State'Access);
               if Key < "+" (Current_State).Key then
                  Next :=
                    Dereference ("+" (Current).Left'Access);
               elsif "+" (Current_State).Key < Key then
                  Next :=
                    Dereference ("+" (Current).Right'Access);
               else
                  --  Current.Key = Key
                  Release (Current);
                  if not Is_Marked (Current_State) then
                     declare
                        Value : constant Value_Type :=
                          "+" (Current_State).Value;
                     begin
                        Release (Current_State);
                        return Value;
                     end;
                  else
                     Release (Current_State);
                     raise Not_Found;
                  end if;
               end if;

               if "+" (Next) = null then
                  --  Not found.
                  Release (Current_State);
                  Release (Current);
                  raise Not_Found;
               end if;

               Release (Current_State);
               Release (Current);
               Current := Next;
               Next    := Node_Ops.Null_Reference;
            end loop Traverse;
         end;
      end if;
   end Lookup;

   ----------------------------------------------------------------------------
   procedure Verify (Tree  : in out Dictionary_Type;
                     Print : in     Boolean := False) is
      use Node_Ops;

      procedure Dump (Node  : in Node_Ops.Private_Reference;
                      Level : in Natural);

      -----------------------------------------------------------------
      procedure Dump (Node  : in Node_Ops.Private_Reference;
                      Level : in Natural) is
      begin
         for I in 1 .. 2*Level loop
            Ada.Text_IO.Put ("  ");
         end loop;
         Ada.Text_IO.Put (Natural'Image (Level) & ".");
         if "+" (Node) = null then
            Ada.Text_IO.Put_Line ("-");
         else
            declare
               use State_Ops;
               Left  : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (Node).Left'Access);
               Right : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (Node).Right'Access);
               State : constant State_Ops.Private_Reference :=
                 Dereference ("+" (Node).State'Access);
            begin
               if Is_Marked (State) then
                  Ada.Text_IO.Put_Line ("Node (" &
                                        Image ("+" (State).Key) & ")");
               else
                  Ada.Text_IO.Put_Line ("Node " & Image ("+" (State).Key));
               end if;
               Release (State);
               Release (Node);
               Dump (Left, Level + 1);
               Dump (Right, Level + 1);
            end;
         end if;
      end Dump;

      Root  : constant Node_Ops.Private_Reference :=
        Dereference (Tree.Mutable.Self.Root'Access);
   begin
      if Print then
         Dump (Root, 0);
         Ada.Text_IO.Put_Line ("Tree_Node MR:");
         Node_MR.Print_Statistics;
         Ada.Text_IO.Put_Line ("Tree_Node_State MR:");
         State_MR.Print_Statistics;
      end if;
   end Verify;


   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function New_Copy (State : State_Ops.Private_Reference)
                     return State_Ops.Private_Reference is
      use State_Ops;
      New_State : constant State_Ops.Private_Reference := Create_Node_State;
   begin
      "+" (New_State).Key     := "+" (State).Key;
      "+" (New_State).Value   := "+" (State).Value;
      return New_State;
   end New_Copy;

   ----------------------------------------------------------------------------
   function  Find_Parent (Root : access Node_Reference;
                          Node : in     Node_Ops.Private_Reference;
                          Key  : in     Key_Type)
                         return Node_Ops.Private_Reference is
      use Node_Ops, State_Ops;
      Current        : Node_Ops.Private_Reference :=
        Dereference (Root);
      Current_State  : State_Ops.Private_Reference;
      Next           : Node_Ops.Private_Reference;
   begin
      loop
         if "+" (Current) = null then
            --  Node isn't in the tree any more.
            Release (Current);
            return Node_Ops.Null_Reference;
         end if;

         Current_State := Dereference ("+" (Current).State'Access);

         if "+" (Current_State) = null then
            --  Current has been removed. Node isn't in the tree any more.
            Release (Current_State);
            Release (Current);
            return Node_Ops.Null_Reference;
         end if;

         if Key < "+" (Current_State).Key then
            Next :=
              Dereference ("+" (Current).Left'Access);
         elsif "+" (Current_State).Key < Key then
            Next :=
              Dereference ("+" (Current).Right'Access);
         else
            --  Current.Key = Key.
            --  Node must be gone already!
            Release (Current_State);
            Release (Current);
            return Node_Ops.Null_Reference;
         end if;
         Release (Current_State);

         if "+" (Next) = "+" (Node) then
            Release (Next);
            return Current;
         end if;

         Release (Current);
         Current := Next;
         Next    := Node_Ops.Null_Reference;
      end loop;
   end Find_Parent;

   ----------------------------------------------------------------------------
   procedure Remove_Node (Root      : access Node_Reference;
                          Node      : in     Node_Ops.Private_Reference;
                          Old_State : in     State_Ops.Private_Reference) is
      use Node_Ops, State_Ops;
      Marked_Left, Marked_Right : Boolean := False;
   begin
      --  Try to mark both child pointers.
      Marked_Left :=
        Compare_And_Swap (Link      => "+" (Node).Left'Access,
                          Old_Value => Node_Ops.Null_Reference,
                          New_Value => Mark (Node_Ops.Null_Reference)) or
        else Is_Marked ("+" (Node).Left);
      Marked_Right :=
        Compare_And_Swap (Link      => "+" (Node).Right'Access,
                          Old_Value => Node_Ops.Null_Reference,
                          New_Value => Mark (Node_Ops.Null_Reference)) or
        else Is_Marked ("+" (Node).Right);

      if not (Marked_Left or Marked_Right) then
         --  Two subtrees.
         --  For now: Cannot remove the node.
         Release (Old_State);
         Release (Node);
         return;
      end if;

      declare
         New_Child : Node_Ops.Private_Reference :=
           Node_Ops.Null_Reference;
         Parent    : constant Node_Ops.Private_Reference :=
           Find_Parent (Root,
                        Node,
                        "+" (Old_State).Key);
         Success   : Boolean := False;
      begin
         if "+" (Parent) /= null then
            --  Check if there is a subtree to move up.
            if not Marked_Left and Marked_Right then
               New_Child := Dereference ("+" (Node).Left'Access);
            elsif Marked_Left and not Marked_Right then
               New_Child := Dereference ("+" (Node).Right'Access);
            end if;

            if "+" (Parent).Left = Node then
               Success := Compare_And_Swap
                 (Link      => "+" (Parent).Left'Access,
                  Old_Value => Node,
                  New_Value => New_Child);
            elsif "+" (Parent).Right = Node then
               Success := Compare_And_Swap
                 (Link      => "+" (Parent).Right'Access,
                  Old_Value => Node,
                  New_Value => New_Child);
            end if;
            Release (New_Child);
            if Success then
               if
                 Compare_And_Swap (Link      => "+" (Node).State'Access,
                                   Old_Value => Mark (Old_State),
                                   New_Value =>
                                     Mark (State_Ops.Null_Reference))
               then
                  Delete (Old_State);
               else
                  Release (Old_State);
               end if;

               Delete  (Node);
            else
               Release (Node);
            end if;
         else
            --  The Parent is no longer in the tree.
            Release (Old_State);
            Release (Node);
         end if;
      end;
   end Remove_Node;

   ----------------------------------------------------------------------------
   procedure Free (State : access Node_State) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Node_State,
                                    New_Node_State_Access);

      function To_New_Node_State_Access is new
        Ada.Unchecked_Conversion (State_Ops.Node_Access,
                                  New_Node_State_Access);

      X : New_Node_State_Access :=
        To_New_Node_State_Access (State_Ops.Node_Access (State));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   function All_References (Node : access Tree_Node)
                           return Node_MR.Reference_Set is
      type Link_Access is access all Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  Node_MR.Shared_Reference_Base_Access);
      use Node_Ops;

      Result : constant Node_MR.Reference_Set (1 .. 2) :=
        (To_Shared_Reference_Access (Node.Left'Access),
         To_Shared_Reference_Access (Node.Right'Access));
   begin
      return Result;
   end All_References;

   ----------------------------------------------------------------------------
   procedure Free (Node : access Tree_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Tree_Node,
                                    New_Tree_Node_Access);

      function To_New_Tree_Node_Access is new
        Ada.Unchecked_Conversion (Node_Ops.Node_Access,
                                  New_Tree_Node_Access);

      X : New_Tree_Node_Access :=
        To_New_Tree_Node_Access (Node_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Lock_Free_Simple_Trees;

