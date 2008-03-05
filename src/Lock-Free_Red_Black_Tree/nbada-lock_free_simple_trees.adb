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
--  $Id: nbada-lock_free_simple_trees.adb,v 1.1 2008/03/05 11:10:14 andersg Exp $
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

   Tree_Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
       (Block_Size => Tree_Node'Max_Size_In_Storage_Elements);

   type New_Tree_Node_Access is access Tree_Node;
   for New_Tree_Node_Access'Storage_Pool use Tree_Node_Pool;

   function Create_Tree_Node is
      new Node_Ops.Create (New_Tree_Node_Access);

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   function Compare_And_Swap is
      new NBAda.Primitives.Boolean_Compare_And_Swap_32 (Atomic_Boolean);

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
      Node : constant Node_Ops.Private_Reference := Create_Tree_Node;
   begin
      "+" (Node).Key   := Key;
      "+" (Node).Value := Value;

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
                  Current : Private_Reference := Root;
                  Next    : Private_Reference;
               begin
                  Find_Leaf : loop
                     if Key < "+" (Current).Key then
                        Next :=
                          Dereference ("+" (Current).Left'Access);
                     elsif "+" (Current).Key < Key then
                        Next :=
                          Dereference ("+" (Current).Right'Access);
                     else
                        --  Current.Key = Key.
                        --  NOTE: Currently a key can only be inserted once.
                        Release (Current);
                        Release (Node);
                        raise Already_Present;
                     end if;

                     exit Find_Leaf when "+" (Next) = null;

                     Release (Current);
                     Current := Next;
                     Next    := Null_Reference;
                  end loop Find_Leaf;
                  Release (Next);

                  declare
                     Success : Boolean := False;
                  begin
                     if Key < "+" (Current).Key then
                        Success :=
                          Compare_And_Swap (Link => "+" (Current).Left'Access,
                                            Old_Value => Null_Reference,
                                            New_Value => Node);
                     elsif "+" (Current).Key < Key then
                        Success :=
                          Compare_And_Swap (Link => "+" (Current).Right'Access,
                                            Old_Value => Null_Reference,
                                            New_Value => Node);
                     end if;
                     if Success then
                        Release (Current);
                        Release (Node);
                        return;
                     end if;
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
         begin
            loop
               if Key < "+" (Current).Key then
                  Next :=
                    Dereference ("+" (Current).Left'Access);
               elsif "+" (Current).Key < Key then
                  Next :=
                    Dereference ("+" (Current).Right'Access);
               else
                  --  Current.Key = Key. Set the deletion mark.
                  loop
                     if "+" (Current).Deleted then
                        Release (Current);
                        raise Not_Found;
                     end if;

                     if
                       Compare_And_Swap (Target    =>
                                           "+" (Current).Deleted'Access,
                                         Old_Value => False,
                                         New_Value => True)
                     then
                        Release (Current);
                        return;
                     end if;
                  end loop;
               end if;

               if "+" (Next) = null then
                  --  Not found.
                  Release (Current);
                  raise Not_Found;
               end if;

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
      Root : constant Node_Ops.Private_Reference :=
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
         begin
            loop
               if Key < "+" (Current).Key then
                  Next :=
                    Dereference ("+" (Current).Left'Access);
               elsif "+" (Current).Key < Key then
                  Next :=
                    Dereference ("+" (Current).Right'Access);
               else
                  --  Current.Key = Key
                  declare
                     Value   : constant Value_Type := "+" (Current).Value;
                     Deleted : constant Boolean :=
                       Boolean ("+" (Current).Deleted);
                  begin
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
                  Release (Current);
                  raise Not_Found;
               end if;

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
      begin
         for I in 1 .. 2*Level loop
            Ada.Text_IO.Put ("  ");
         end loop;
         Ada.Text_IO.Put (Natural'Image (Level) & ".");
         if "+" (Node) = null then
            Ada.Text_IO.Put_Line ("-");
         else
            declare
               Left  : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (Node).Left'Access);
               Right : constant Node_Ops.Private_Reference :=
                 Dereference ("+" (Node).Right'Access);
            begin
               if "+" (Node).Deleted then
                  Ada.Text_IO.Put_Line ("Node (" &
                                        Image ("+" (Node).Key) & ")");
               else
                  Ada.Text_IO.Put_Line ("Node " & Image ("+" (Node).Key));
               end if;
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
   function All_References (Node : access Tree_Node)
                           return MR.Reference_Set is
      type Link_Access is access all Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  MR.Shared_Reference_Base_Access);
      use Node_Ops;

      Result : constant MR.Reference_Set (1 .. 2) :=
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

