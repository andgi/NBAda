-------------------------------------------------------------------------------
--  Lock-Free Red Black Trees - An implementation of the lock-free red black
--                              tree algorithm by A. Gidenstam.
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
--  Filename        : nbada-lock_free_red_black_trees.adb
--  Description     : Lock-free algorithm for Red-Black trees by
--                    Anders Gidenstam. Based on the left leaning
--                    red-black trees by ...
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 21 23:22:26 2008
--  $Id: nbada-lock_free_red_black_trees.adb,v 1.1 2008/02/26 16:55:14 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body NBAda.Lock_Free_Red_Black_Trees is

   ----------------------------------------------------------------------------
   --  Storage pools for the nodes.
   ----------------------------------------------------------------------------

   Tree_Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
       (Block_Size => Tree_Node'Max_Size_In_Storage_Elements);

   type New_Tree_Node_Access is access Tree_Node;
   for New_Tree_Node_Access'Storage_Pool use Tree_Node_Pool;

   function Create_Tree_Node is
      new Node_Ops.Create (New_Tree_Node_Access);

   Tree_Node_State_Pool :
     Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Tree_Node_State'Max_Size_In_Storage_Elements);

   type New_Tree_Node_State_Access is access Tree_Node_State;
   for New_Tree_Node_State_Access'Storage_Pool use Tree_Node_State_Pool;

   function Create_Tree_Node_State is
      new Node_State_Ops.Create (New_Tree_Node_State_Access);

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

--   procedure Rotate_Left ();

--   procedure Announce (Parent : in    Tree_Node_Ops.Private_Reference;
--                       Info   : in    Operation_Info);
--   procedure Update_Child       (Info : in Operation_Info);
--   procedure Update_Grand_Child (Info : in Operation_Info);

   function New_Copy (State : Node_State_Ops.Private_Reference)
                     return Node_State_Ops.Private_Reference;

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
      use Node_Ops;
      use Node_State_Ops;
      Node : constant Node_Ops.Private_Reference := Create_Tree_Node;
   begin
      "+" (Node).Key   := Key;
      "+" (Node).Value := Value;
      declare
         State : constant Node_State_Ops.Private_Reference :=
           Create_Tree_Node_State;
      begin
         Store ("+" (Node).State'Access, State);
         Release (State);
      end;

      loop
         declare
            Root : constant Node_Ops.Private_Reference :=
              Dereference (Into.Root'Access);
         begin
            if "+" (Root) = null then
               if
                 Compare_And_Swap (Link      => Into.Root'Access,
                                   Old_Value => Node_Ops.Null_Reference,
                                   New_Value => Node)
               then
                  Release (Root);
                  Release (Node);
                  return;
               end if;
            else
               --  Traverse to a leaf.
               declare
                  Current : Node_Ops.Private_Reference := Root;
                  Next    : Node_Ops.Private_Reference;
                  Current_State : Node_State_Ops.Private_Reference;
               begin
                  Find_Leaf : loop
                     Current_State :=
                       Dereference ("+" (Current).State'Access);
                     if Key < "+" (Current).Key then
                        Next :=
                          Dereference ("+" (Current_State).Left'Access);
                     elsif "+" (Current).Key < Key then
                        Next :=
                          Dereference ("+" (Current_State).Right'Access);
                     else
                        --  Current.Key = Key.
                        --  NOTE: Currently a key can only be inserted once.
                        Release (Current_State);
                        Release (Current);
                        raise Already_Present;
                     end if;

                     exit Find_Leaf when "+" (Next) = null;

                     Release (Current_State);
                     Current_State := Node_State_Ops.Null_Reference;
                     Release (Current);
                     Current := Next;
                     Next    := Node_Ops.Null_Reference;
                  end loop Find_Leaf;
                  Release (Next);

                  declare
                     New_State : constant Node_State_Ops.Private_Reference :=
                       New_Copy (Current_State);
                  begin
                     if Key < "+" (Current).Key then
                        Store ("+" (New_State).Left'Access, Node);
                     elsif "+" (Current).Key < Key then
                        Store ("+" (New_State).Right'Access, Node);
                     end if;
                     if
                       Compare_And_Swap
                       (Link      => "+" (Current).State'Access,
                        Old_Value => Current_State,
                        New_Value => New_State)
                     then
                        Release (New_State);
                        Delete  (Current_State);
                        Release (Current);
                        Release (Node);
                        return;
                     end if;
                     Delete (New_State);
                     Release (Current_State);
                  end;
                  Release (Current);
               end;
            end if;
         end;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type) is
      use Node_Ops;
      use Node_State_Ops;
      Root  : constant Node_Ops.Private_Reference :=
        Dereference (From.Mutable.Self.Root'Access);
   begin
      if "+" (Root) = null then
         Release (Root);
         raise Not_Found;
      else
         --  Traverse the tree.
         declare
            Current : Node_Ops.Private_Reference := Root;
            Next    : Node_Ops.Private_Reference;
            Current_State : Node_State_Ops.Private_Reference;
         begin
            loop
               Current_State :=
                 Dereference ("+" (Current).State'Access);
               if Key < "+" (Current).Key then
                  Next :=
                    Dereference ("+" (Current_State).Left'Access);
               elsif "+" (Current).Key < Key then
                  Next :=
                    Dereference ("+" (Current_State).Right'Access);
               else
                  --  Current.Key = Key. Set the deletion mark.
                  loop
                     if "+" (Current_State).Deleted then
                        Release (Current_State);
                        Release (Current);
                        raise Not_Found;
                     end if;

                     declare
                        New_State : constant
                          Node_State_Ops.Private_Reference :=
                          New_Copy (Current_State);
                     begin
                        "+" (New_State).Deleted := True;

                        if
                          Compare_And_Swap (Link      =>
                                              "+" (Current).State'Access,
                                            Old_Value => Current_State,
                                            New_Value => New_State)
                        then
                           Release (New_State);
                           Delete  (Current_State);
                           Release (Current);
                           return;
                        end if;

                        Release (New_State);
                        Release (Current_State);
                        Current_State :=
                          Dereference ("+" (Current).State'Access);
                     end;
                  end loop;
               end if;

               if "+" (Next) = null then
                  --  Not found.
                  Release (Current_State);
                  Release (Current);
                  raise Not_Found;
               end if;

               Release (Current_State);
               Current_State := Node_State_Ops.Null_Reference;
               Release (Current);
               Current := Next;
               Next    := Node_Ops.Null_Reference;
            end loop;
         end;
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type is
      use Node_Ops;
      use Node_State_Ops;
      Root  : constant Node_Ops.Private_Reference :=
        Dereference (From.Mutable.Self.Root'Access);
   begin
      if "+" (Root) = null then
         Release (Root);
         raise Not_Found;
      else
         --  Traverse the tree.
         declare
            Current : Node_Ops.Private_Reference := Root;
            Next    : Node_Ops.Private_Reference;
            Current_State : Node_State_Ops.Private_Reference;
         begin
            loop
               Current_State :=
                 Dereference ("+" (Current).State'Access);
               if Key < "+" (Current).Key then
                  Next :=
                    Dereference ("+" (Current_State).Left'Access);
               elsif "+" (Current).Key < Key then
                  Next :=
                    Dereference ("+" (Current_State).Right'Access);
               else
                  --  Current.Key = Key
                  declare
                     Value   : constant Value_Type := "+" (Current).Value;
                     Deleted : constant Boolean := "+" (Current_State).Deleted;
                  begin

                     Release (Current_State);
                     Release (Current);
                     if not Deleted then
                        return Value;
                     else
                        raise Not_Found;
                     end if;
                  end;
               end if;

               if "+" (Next) = null then
                  --  Not found.
                  Release (Current_State);
                  Release (Current);
                  raise Not_Found;
               end if;

               Release (Current_State);
               Current_State := Node_State_Ops.Null_Reference;
               Release (Current);
               Current := Next;
               Next    := Node_Ops.Null_Reference;
            end loop;
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
         use Node_State_Ops;
      begin
         for I in 1 .. 2*Level loop
            Ada.Text_IO.Put ("  ");
         end loop;
         Ada.Text_IO.Put (Natural'Image (Level) & ".");
         if "+" (Node) = null then
            Ada.Text_IO.Put_Line ("-");
         else
            declare
               State : constant Node_State_Ops.Private_Reference :=
                 Dereference ("+" (Node).State'Access);
               Left  : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (State).Left'Access);
               Right : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (State).Right'Access);
            begin
               Ada.Text_IO.Put_Line ("Node " & Image ("+" (Node).Key));
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
      Dump (Root, 0);
   end Verify;


   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function New_Copy (State : Node_State_Ops.Private_Reference)
                     return Node_State_Ops.Private_Reference is
      use Node_Ops;
      use Node_State_Ops;
      New_State : constant Node_State_Ops.Private_Reference :=
        Create_Tree_Node_State;
      --  Note: Unsafe_Read would be safe here.
      Left      : constant Node_Ops.Private_Reference :=
        Dereference ("+" (State).Left'Access);
      Right     : constant Node_Ops.Private_Reference :=
        Dereference ("+" (State).Right'Access);
   begin
      "+" (New_State).Deleted := "+" (State).Deleted;
      Store ("+" (New_State).Left'Access, Left);
      Store ("+" (New_State).Right'Access, Right);
      Release (Left);
      Release (Right);
      return New_State;
   end New_Copy;

   ----------------------------------------------------------------------------
   function All_References (Node : access Tree_Node)
                           return MR.Reference_Set is
      type Link_Access is access all Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  MR.Shared_Reference_Base_Access);
      use Node_Ops;
      use Node_State_Ops;

      State  : constant Node_State_Ops.Private_Reference :=
        Dereference (Node.State'Access);
      Result : constant MR.Reference_Set (1 .. 2) :=
        (To_Shared_Reference_Access ("+" (State).Left'Access),
         To_Shared_Reference_Access ("+" (State).Right'Access));
   begin
      Release (State);
      Store (Node.State'Access, Node_State_Ops.Null_Reference);
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

   ----------------------------------------------------------------------------
   function All_References (Node : access Tree_Node_State)
                           return MR.Reference_Set is
      Result : constant MR.Reference_Set (1 .. 0) := (others => null);
   begin
      return Result;
   end All_References;

   ----------------------------------------------------------------------------
   procedure Free (Node : access Tree_Node_State) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Tree_Node_State,
                                    New_Tree_Node_State_Access);

      function To_New_Tree_Node_State_Access is new
        Ada.Unchecked_Conversion (Node_State_Ops.Node_Access,
                                  New_Tree_Node_State_Access);

      X : New_Tree_Node_State_Access :=
        To_New_Tree_Node_State_Access (Node_State_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Lock_Free_Red_Black_Trees;

