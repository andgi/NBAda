-------------------------------------------------------------------------------
--  Lock-Free Dicitionary Test - Test benchmark for lock-free dictionaries.
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
--                              -*- Mode: Ada -*-
--  Filename        : red_black_trees.adb
--  Description     : Left-leaning red-black trees based on Robert Sedgewick's
--                    presentation at Dagstuhl, 2008-02-18.
--  Author          : Anders Gidenstam
--  Created On      : Tue Feb 26 18:56:37 2008
--  $Id: red_black_trees.adb,v 1.1 2008/02/27 17:27:27 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;

with Ada.Text_IO;

package body Red_Black_Trees is

   procedure Reclaim is new Ada.Unchecked_Deallocation (Tree_Node,
                                                        Tree_Node_Access);

   function Is_Red (Node : Tree_Node_Access) return Boolean;

   procedure Lean_Left  (Parent : in out Tree_Node_Access);
   procedure Lean_Right (Parent : in out Tree_Node_Access);
   procedure Split_Four_Node (Parent : in out Tree_Node_Access);
   procedure Move_Red_Left  (Parent : in out Tree_Node_Access);
   procedure Move_Red_Right (Parent : in out Tree_Node_Access);

   procedure Rotate_Left  (Parent : in out Tree_Node_Access);
   procedure Rotate_Right (Parent : in out Tree_Node_Access);

   procedure Delete_Min (Node  : in out Tree_Node_Access;
                         Key   :    out Key_Type;
                         Value :    out Value_Type);

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Dictionary : in out Dictionary_Type) is

      procedure Delete_All (Node : in out Tree_Node_Access);

      procedure Delete_All (Node : in out Tree_Node_Access) is
      begin
         if Node /= null then
            Delete_All (Node.Left);
            Delete_All (Node.Right);
            Reclaim (Node);
            Node := null;
         end if;
      end Delete_All;

   begin
      Delete_All (Dictionary.Root);
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is

      procedure Insert (Node : in out Tree_Node_Access);

      procedure Insert (Node : in out Tree_Node_Access) is
      begin
         if Node = null then
            Node := new Tree_Node'(Key   => Key,
                                   Value => Value,
                                   Color => Red,
                                   Left  => null,
                                   Right => null);
         else
            if Is_Red (Node.Left) and then Is_Red (Node.Left.Left) then
               Ada.Text_IO.Put_Line ("Splitting 4-node!");
               Split_Four_Node (Node);
            end if;

            if Key < Node.Key then
               Insert (Node.Left);
            elsif Node.Key < Key then
               Insert (Node.Right);
            else
               --  Node.Key = Key;
               --  This is stupid and dangerous!
               raise Already_Present;
            end if;

            if Is_Red (Node.Right) then
               Lean_Left (Node);
            end if;
         end if;
      end Insert;

   begin
      Verify (Into, True);
      Ada.Text_IO.Put_Line ("Insert " & Image (Key));
      Insert (Into.Root);
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type) is
      Tmp : Value_Type;
   begin
      Tmp := Delete (From, Key);
   end Delete;

   ----------------------------------------------------------------------------
   function  Delete (From  : in Dictionary_Type;
                     Key   : in Key_Type)
                    return Value_Type is

      procedure Delete (Node : in out Tree_Node_Access);

      Result : Value_Type;

      -----------------------------------------------------------------
      procedure Delete (Node : in out Tree_Node_Access) is
      begin
         if Node = null then
            raise Not_Found;
         else
            if Key < Node.Key then
               if not Is_Red (Node.Left) and not Is_Red (Node.Left.Left) then
                  Move_Red_Left (Node);
               end if;
               Delete (Node.Left);
            else
               if Is_Red (Node.Left) then
                  Lean_Right (Node);
               end if;

               if Node.Key = Key and Node.Right = null then
                  Result := Node.Value;
                  Reclaim (Node);
                  Node   := null;
                  return;
               end if;

               if not Is_Red (Node.Right) and not Is_Red (Node.Right.Left) then
                  Move_Red_Right (Node);
               end if;

               if Node.Key = Key then
                  declare
                     Key   : Key_Type;
                     Value : Value_Type;
                  begin
                     Delete_Min (Node.Right, Key, Value);
                     Node.Key   := Key;
                     Node.Value := Value;
                  end;
               else
                  --  Node.Key < Key.
                  Delete (Node.Right);
               end if;
            end if;

            if Is_Red (Node.Right) then
               Lean_Left (Node);
            end if;
         end if;
      end Delete;

   begin
      Delete (From.Mutable.Self.Root);
      return Result;
   end Delete;

   ----------------------------------------------------------------------------
   function Delete_Min (From : in Dictionary_Type)
                       return Value_Type is

      Min_Key : Key_Type;
      Result  : Value_Type;
   begin
      Verify (From.Mutable.Self.all, False);
      Delete_Min (From.Mutable.Self.Root, Min_Key, Result);
      return Result;
   end Delete_Min;

   ----------------------------------------------------------------------------
   function Delete_Max (From : in Dictionary_Type)
                       return Value_Type is

      procedure Delete_Max (Node : in out Tree_Node_Access);

      Result : Value_Type;

      procedure Delete_Max (Node : in out Tree_Node_Access) is
      begin
         if Node = null then
            raise Not_Found;
         end if;
         if Node.Right = null then
            --  Remove this node.
            if Node.Left /= null then
               Node.Left.Color := Black;
            end if;
            declare
               Tmp : Tree_Node_Access := Node;
            begin
               Result := Node.Value;
               Node   := Node.Left;
               Reclaim (Tmp);
            end;
         else
            if Is_Red (Node.Left) then
               Lean_Right (Node);
            end if;
            if not Is_Red (Node.Right) and not Is_Red (Node.Right.Left) then
               Move_Red_Right (Node);
            end if;

            Delete_Max (Node.Right);

            if Is_Red (Node.Right) then
               Lean_Left (Node);
            end if;
         end if;
      end Delete_Max;

   begin
      Delete_Max (From.Mutable.Self.Root);
      return Result;
   end Delete_Max;

   ----------------------------------------------------------------------------
   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type is
      function Lookup (Node : Tree_Node_Access)
                      return Value_Type;

      function Lookup (Node : Tree_Node_Access)
                      return Value_Type is
      begin
         if Node /= null then
            if Key < Node.Key then
               return Lookup (Node.Left);
            elsif Node.Key < Key then
               return Lookup (Node.Right);
            else
               --  Node.Key = Key;
               return Node.Value;
            end if;
         else
            raise Not_Found;
         end if;
      end Lookup;

   begin
      return Lookup (From.Root);
   end Lookup;

   ----------------------------------------------------------------------------
   procedure Verify (Tree  : in out Dictionary_Type;
                     Print : in     Boolean := False) is
      procedure Verify (Node  : in Tree_Node_Access;
                        Level : in Natural);

      procedure Verify (Node  : in Tree_Node_Access;
                        Level : in Natural) is
      begin
         if Node /= null and then Is_Red (Node.Right) then
            Ada.Text_IO.Put_Line ("Error: Right leaning red edge!");
         end if;
         if Print then
            for I in 1 .. 2*Level loop
               Ada.Text_IO.Put ("  ");
            end loop;
            Ada.Text_IO.Put (Natural'Image (Level) & ".");
            if Node = null then
               Ada.Text_IO.Put_Line (" -");
            else
               if Node.Color = Red then
                  Ada.Text_IO.Put_Line ("*Node " & Image (Node.Key));
               else
                  Ada.Text_IO.Put_Line (" Node " & Image (Node.Key));
               end if;
               Verify (Node.Left, Level + 1);
               Verify (Node.Right, Level + 1);
            end if;
         end if;
      end Verify;

   begin
      Verify (Tree.Root, 0);
   end Verify;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Is_Red (Node : Tree_Node_Access) return Boolean is
   begin
      if Node /= null then
         return Node.Color = Red;
      else
         return False;
      end if;
   end Is_Red;

   ----------------------------------------------------------------------------
   procedure Lean_Left (Parent : in out Tree_Node_Access) is
   begin
      Rotate_Left (Parent);
      Parent.Color      := Parent.Left.Color;
      Parent.Left.Color := Red;
   end Lean_Left;

   ----------------------------------------------------------------------------
   procedure Lean_Right (Parent : in out Tree_Node_Access) is
      --  This is a guess. Sedgewick didn't include this one.
   begin
      Rotate_Right (Parent);
      Parent.Color       := Parent.Right.Color;
      Parent.Right.Color := Red;
   end Lean_Right;

   ----------------------------------------------------------------------------
   procedure Split_Four_Node (Parent : in out Tree_Node_Access) is
   begin
      Rotate_Right (Parent);
      Parent.Left.Color := Black;
   end Split_Four_Node;

   ----------------------------------------------------------------------------
   procedure Move_Red_Left (Parent : in out Tree_Node_Access) is
   begin
      Parent.Color      := Black;
      Parent.Left.Color := Red;
      if Is_Red (Parent.Right.Left) then
         Rotate_Right (Parent.Right);
         Rotate_Left (Parent);
      else
         Parent.Right.Color := Red;
      end if;
   end Move_Red_Left;

   ----------------------------------------------------------------------------
   procedure Move_Red_Right (Parent : in out Tree_Node_Access) is
   begin
      Parent.Color       := Black;
      Parent.Right.Color := Red;
      if Is_Red (Parent.Left.Left) then
         Rotate_Right (Parent);
         Parent.Color      := Red;
         Parent.Left.Color := Black;
      else
         Parent.Left.Color := Red;
      end if;
   end Move_Red_Right;

   ----------------------------------------------------------------------------
   procedure Rotate_Left  (Parent : in out Tree_Node_Access) is
      Child             : constant Tree_Node_Access := Parent;
      Grand_Child       : constant Tree_Node_Access := Child.Right;
      Grand_Child_Left  : constant Tree_Node_Access := Grand_Child.Left;
   begin
      Parent           := Grand_Child;
      Grand_Child.Left := Child;
      Child.Right      := Grand_Child_Left;
      --  Experimental:
      if Child.Right /= null then
         Child.Right.Color := Black;
      end if;
   end Rotate_Left;

   ----------------------------------------------------------------------------
   procedure Rotate_Right (Parent : in out Tree_Node_Access) is
      Child             : constant Tree_Node_Access := Parent;
      Grand_Child       : constant Tree_Node_Access := Child.Left;
      Grand_Child_Right : constant Tree_Node_Access := Grand_Child.Right;
   begin
      Parent            := Grand_Child;
      Grand_Child.Right := Child;
      Child.Left        := Grand_Child_Right;
   end Rotate_Right;

   ----------------------------------------------------------------------------
   procedure Delete_Min (Node  : in out Tree_Node_Access;
                         Key   :    out Key_Type;
                         Value :    out Value_Type) is
   begin
      if Node = null then
         raise Not_Found;
      end if;
      if Node.Left = null then
         --  Remove this node.
         Key   := Node.Key;
         Value := Node.Value;
         Reclaim (Node);
         Node := null;
      else
         if not Is_Red (Node.Left) and (Node.Left /= null and then
                                        not Is_Red (Node.Left.Left)) then
            Move_Red_Left (Node);
         end if;

         Delete_Min (Node.Left, Key, Value);

         if Is_Red (Node.Right) then
            Lean_Left (Node);
         end if;
      end if;
   end Delete_Min;

end Red_Black_Trees;

