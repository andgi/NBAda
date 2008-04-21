-------------------------------------------------------------------------------
--  Lock-free Flat-sets - An implementation of A. Gidenstam et al.'s
--                        atomic move algorithm.
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
--  Filename        : nbada-atomic_move.adb
--  Description     : Based on A. Gidenstam,
--                    M. Papatriantafilou and P. Tsigas,
--                    "Allocating memory in a lock-free manner",
--                    The 13th Annual European Symposium on Algorithms
--                    (ESA 2005), LNCS 3669, pages 329 - 242, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Jan 16 11:46:57 2008
--  $Id: nbada-atomic_move.adb,v 1.15 2008/04/21 11:34:29 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;

with NBAda.Lock_Free_Growing_Storage_Pools;

package body NBAda.Atomic_Move is

   -------------------------------------------------------------------------
   --  Storage pool for the Move_Info nodes.
   -------------------------------------------------------------------------

   Move_Info_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Move_Info_Record'Max_Size_In_Storage_Elements);

   type New_Move_Info_Access is access Move_Info_Record;
   for New_Move_Info_Access'Storage_Pool use Move_Info_Pool;

   function New_Move_Info is new Move_Info_MR_Ops.Create
     (User_Node_Access => New_Move_Info_Access);

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap is
      new Primitives.Standard_Compare_And_Swap (Shared_Location);
   procedure Void_Compare_And_Swap is
      new Primitives.Standard_Void_Compare_And_Swap (Shared_Location);
   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap (Shared_Location);

   procedure Compare_And_Swap is
      new Primitives.Void_Compare_And_Swap_32 (Move_Status);

   procedure Help_Move (Element   : in out Private_Reference;
                        Operation : in     Move_Info_Reference;
                        Result    :    out Move_Status);
   --  NOTE: Operation must be a valid private reference.
   --        Operation will be released before Help_Move returns.

   function Image (Ref : Node_Ref) return String;

   function To_Node_Access (X : Node_Access_Impl) return Node_Access;
   function To_Node_Access_Impl (X : Node_Access) return Node_Access_Impl;

   subtype Processes is Process_Ids.Process_ID_Type;

   --  Per-task shared locations used by Create and Delete.
   Tmp_Location : array (Processes) of aliased Shared_Location;

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Dereference (Location : access Shared_Location)
                        return Private_Reference is
      type Location_Access is access all Shared_Location;
      function To_Shared_Location_Access is
         new Ada.Unchecked_Conversion (Location_Access,
                                       Shared_Location_Access);
      Result : Private_Reference;
   begin
      loop
         declare
            Ref  : constant Node_Ref := Node_Ref (Location.all);
            Node : constant Node_Access := To_Node_Access (Ref.Node);
         begin
            Primitives.Membar;

            Result.Ref      := Ref;
            Result.Location :=
              To_Shared_Location_Access (Location_Access (Location));

            if Node = null then
               return Result;
            end if;

            declare
               use Move_Info_MR_Ops;
               Status : constant Move_Info_Reference :=
                 Dereference (Node.Status'Access);
            begin
               if
                 "+" (Status).New_Pos = null and then
--                  "+" (Status).Old_Pos = Location) and then
                 Node_Ref (Location.all) = Ref
               then
                  Release (Status);
                  return Result;
               end if;

               if "+" (Status).New_Pos /= null then
                  declare
                     Tmp : Move_Status;
                  begin
                     --  The linked load of Node.Status above should
                     --  remain valid until the end of Help_Move.
                     Help_Move (Element   => Result,
                                Operation => Status,
                                Result    => Tmp);
                  end;
               else
                  Release (Status);
               end if;
            end;
         end;
      end loop;
   end Dereference;

   ----------------------------------------------------------------------------
   procedure Move (To      : access Shared_Location;
                   Element : in out Private_Reference;
                   Result  :    out Move_Status) is
      type Location_Access is access all Shared_Location;
      function To_Shared_Location_Access is
         new Ada.Unchecked_Conversion (Location_Access,
                                       Shared_Location_Access);
   begin
      if To = Element.Location then
         Result := Moved_Ok;
         return;
      end if;

      --  Step 1. Initiate move operation.
      loop
         declare
            use Move_Info_MR_Ops;
            Old_From  : Node_Ref;
            Old_To    : Node_Ref;
            Node      : constant Node_Access :=
              To_Node_Access (Element.Ref.Node);
            Operation : constant Move_Info_Reference :=
              Dereference (Node.Status'Access);
            --  Dereference Node.Status. This node should remain valid until
            --  the end of the move.
         begin
            --  Read current value of From.
            Old_From := Node_Ref (Element.Location.all);
            Primitives.Membar;

            if Old_From /= Element.Ref then
               --  The Node is no longer present in Element.Location.
               Result := Moved_Away;
               Release (Operation);
               return;
            elsif "+" (Operation).New_Pos = null then
               --  No current operation.

               --  Dereference To and stop early if we'd fail anyway. This
               --  is a significant saving.
               Old_To := Dereference (To).Ref;
               if Old_To.Node /= 0 then
                  --  To is occupied. Thanks to Dereference this
                  --  should linearize just fine.
                  if Node_Ref (Element.Location.all) = Old_From then
                     Result := Not_Moved;
                  else
                     --  We might have seen this node in To..
                     Result := Moved_Away;
                  end if;
                  Release (Operation);
                  return;
               end if;

               declare
                  use Move_Info_MR_Ops;
                  New_Op  : constant Move_Info_Reference := New_Move_Info;

                  Old_Pos : constant Shared_Location_Access :=
                    Element.Location;
                  New_Pos : constant Shared_Location_Access :=
                    To_Shared_Location_Access (Location_Access (To));
               begin
                  "+" (New_Op).Old_Pos := Old_Pos;
                  "+" (New_Op).New_Pos := New_Pos;
                  "+" (New_Op).New_Pos_Value := Old_To;
                  "+" (New_Op).Old_Pos_Value := Old_From;

                  if
                    Compare_And_Swap (Link      => Node.Status'Access,
                                      Old_Value => Operation,
                                      New_Value => New_Op)
                  then
                     Delete (Operation);
                     --  NOTE: Help_Move requires a linked load of the
                     --        current value of Node.Status.
                     Help_Move (Element   => Element,
                                Operation => New_Op,
                                Result    => Result);
                     return;
                  else
                     Delete  (New_Op);
                     Release (Operation);
                  end if;
               end;
            else
               --  Help current operation.
               --  NOTE: For efficiency one might consider just returning
               --        Moved_Away, but that isn't certain - since that
               --        move might result in Not_Moved.
               declare
                  Tmp  : Move_Status;
                  Elem : Private_Reference :=
                    (Ref      =>
                       (Element.Ref.Node,
                        "+" (Operation).Old_Pos_Value.Version),
                     Location => "+" (Operation).Old_Pos);
               begin
                  Help_Move (Element   => Elem,
                             Operation => Operation,
                             Result    => Tmp);
               end;
            end if;
         end;
      end loop;
   end Move;

   ----------------------------------------------------------------------------
   function Create (Element : Element_Type) return Private_Reference is
      use Move_Info_MR_Ops;
      ID       : constant Processes := Process_Ids.Process_ID;
      Tmp      : Private_Reference;
      New_Node : constant Node_Access         := new Node;
      New_MI   : constant Move_Info_Reference := New_Move_Info;
   begin
      Tmp.Ref.Node      := To_Node_Access_Impl (New_Node);
      New_Node.Element  := Element;
      Tmp.Location      := Tmp_Location (ID)'Access;
      Tmp_Location (ID) := (Tmp.Ref.Node, 0);
      "+" (New_MI).New_Pos := null;
      "+" (New_MI).Old_Pos := Tmp.Location;
      Store (New_Node.Status'Access, New_MI);
      Release (New_MI);
      return Tmp;
   end Create;

   ----------------------------------------------------------------------------
   procedure Delete (Element : in Private_Reference) is
      ID     : constant Processes := Process_Ids.Process_ID;
      Node   : Private_Reference := Element;
      Result : Move_Status;
   begin
      loop
         Move (Element => Node,
               To      => Tmp_Location (ID)'Access,
               Result  => Result);
         exit when Result /= Not_Moved;
      end loop;
      if Result = Moved_Ok then
         --  The node is now ready for deletion. However, the memory cannot be
         --  freed safely without the help of a memory reclamation algorithm.
         --  For now the memory is just leaked..
         null;
--        elsif
--          Result = Dunno and
--          Dereference (Tmp_Location (ID)'Access).Ref.Node = Node.Ref.Node
--        then
--           --  The move must have succeded after all.
--           null;
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function "=" (Left, Right : Private_Reference) return Boolean is
   begin
      return Left.Ref.Node = Right.Ref.Node;
   end "=";

   ----------------------------------------------------------------------------
   function Image (Ref : Private_Reference) return String is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Shared_Location_Access,
                                       Primitives.Standard_Unsigned);
   begin
      return Image (Ref.Ref) & "@" &
        Primitives.Standard_Unsigned'Image (To_Unsigned (Ref.Location));
   end Image;

   ----------------------------------------------------------------------------
   function Image (Location : Shared_Location) return String is
      use Primitives;
   begin
      if Location.Node = 0 then
         return "(null, " & Version_ID'Image (Location.Version) & ")";
      else
         return "(" &
           Standard_Unsigned'Image
           (Standard_Unsigned (Location.Node) * Node'Alignment) &
           ", " & Version_ID'Image (Location.Version) & ")" &
           "  " &
           Unsigned_32'Image (To_Node_Access (Location.Node).Step_Count (2)) &
           " " &
           Unsigned_32'Image (To_Node_Access (Location.Node).Step_Count (3));
      end if;
   end Image;

   ----------------------------------------------------------------------------
   function "+" (Ref : Private_Reference) return Element_Access is
      Node : constant Node_Access := To_Node_Access (Ref.Ref.Node);
   begin
      return Node.Element'Access;
   end "+";


   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   procedure Help_Move (Element   : in out Private_Reference;
                        Operation : in     Move_Info_Reference;
                        Result    :    out Move_Status) is

      procedure Give_Up (Node      : in     Node_Access;
                         Operation : in     Move_Info_Reference;
                         Result    :    out Move_Status);
      procedure Give_Up (Node      : in     Node_Access;
                         Operation : in     Move_Info_Reference;
                         Result    :    out Move_Status) is
         use Move_Info_MR_Ops;
      begin
         Primitives.Membar;
         if Operation = Node.Status then
            --  We know this operation will result in Not_Moved. Verify!
            Compare_And_Swap (Target    => "+" (Operation).Result'Access,
                              Old_Value => Dunno,
                              New_Value => Not_Moved);

            declare
               New_Status : constant Move_Info_Reference := New_Move_Info;
            begin
               "+" (New_Status).Old_Pos := Element.Location;
               "+" (New_Status).New_Pos := null;
               if
                 Compare_And_Swap (Link      => Node.Status'Access,
                                   Old_Value => Operation,
                                   New_Value => New_Status)
               then
                  Release (New_Status);
                  Delete  (Operation);
                  --  We completed the operation.
                  Result := Not_Moved;
               else
                  Delete  (New_Status);
                  Release (Operation);
                  Result := Not_Moved; --  This is right, isn't it?
               end if;
            end;
         else
            Result := "+" (Operation).Result;
            Release (Operation);
--            Result := Dunno; -- ?! Can we be more specific?
         end if;
      end Give_Up;

      To   : constant Shared_Location_Access :=
        Move_Info_MR_Ops."+" (Operation).New_Pos;
      From : constant Shared_Location_Access :=
        Move_Info_MR_Ops."+" (Operation).Old_Pos;
   begin
      --  Step 2. Update To.
      Primitives.Membar;
      declare
         use Move_Info_MR_Ops;
         Old_To : constant Node_Ref := Node_Ref (To.all);
         Node   : constant Node_Access := To_Node_Access (Element.Ref.Node);
      begin
         Primitives.Membar;
         if Operation /= Node.Status then
            Primitives.Membar;
            Result := "+" (Operation).Result;
            Release (Operation);
            return;
         end if;

         if Old_To = "+" (Operation).New_Pos_Value then
            --  Attempt to do Step 2.
            declare
               Res : Node_Ref := (Element.Ref.Node,
                                  Old_To.Version + 1);
            begin
               Compare_And_Swap (Target    => To,
                                 Old_Value => Shared_Location (Old_To),
                                 New_Value => Shared_Location (Res));
               if Res /= Old_To then
                  --  To was occupied.
                  if Res.Node /= Element.Ref.Node then
                     --  This should be safe now.
                     Give_Up (Node, Operation, Result);
                     return;
                  end if;
                  --  Step 2 was done be someone else. Carry on.
               else
                  --  Step 2 done.
                  Primitives.Fetch_And_Add_32 (Node.Step_Count (2)'Access, 1);
               end if;
               --  Step 2 done. Carry on.
            end;
         else
            if Old_To.Node /= Element.Ref.Node then
               --  What if somebody else succeded? Should be safe now.
               Give_Up (Node, Operation, Result);
               return;
            end if;
            --  Step 2 was done be someone else. Carry on.
         end if;
      end;

      --  Step 3. Clear From.
      --  We know this operation will result in Moved_Ok. Verify!
      Compare_And_Swap (Target    =>
                          Move_Info_MR_Ops."+" (Operation).Result'Access,
                        Old_Value => Dunno,
                        New_Value => Moved_Ok);
      Result := Moved_Ok;

      declare
         use Move_Info_MR_Ops;
         Node     : constant Node_Access := To_Node_Access (Element.Ref.Node);
         Old_From : constant Node_Ref    := "+" (Operation).Old_Pos_Value;
      begin
         Primitives.Membar;
         if Operation = Node.Status then
            if
              Compare_And_Swap (Target    => From,
                                Old_Value => Shared_Location (Old_From),
                                New_Value => (0, Old_From.Version + 1))
            then
               Primitives.Fetch_And_Add_32 (Node.Step_Count (3)'Access, 1);
            end if;
         else
            Release (Operation);
            return;
         end if;
      end;

      --  Step 4. Remove the operation information from Node.
      declare
         use Move_Info_MR_Ops;
         Node    : constant Node_Access
           := To_Node_Access (Element.Ref.Node);
         New_Op  : constant Move_Info_Reference := New_Move_Info;
         Version : constant Version_ID :=
           "+" (Operation).New_Pos_Value.Version + 1;
      begin
         "+" (New_Op).Old_Pos := To;
         "+" (New_Op).New_Pos := null;
         if
           Compare_And_Swap (Link      => Node.Status'Access,
                             Old_Value => Operation,
                             New_Value => New_Op)
         then
            Release (New_Op);
            Delete  (Operation);
         else
            Delete  (New_Op);
            Release (Operation);
         end if;
         --  Update the Element reference to reflect the new position.
         Element.Ref.Version := Version;
         Element.Location    := To;
      end;
   end Help_Move;

   ----------------------------------------------------------------------------
   function Image (Ref : Node_Ref) return String is
      use Primitives;
   begin
      if Ref.Node = 0 then
         return "(null, " & Version_ID'Image (Ref.Version) & ")";
      else
         return
           "("  &
           Standard_Unsigned'Image
           (Standard_Unsigned (Ref.Node) * Node'Alignment) &
           ", " & Version_ID'Image (Ref.Version) & ")";
      end if;
   end Image;

   ----------------------------------------------------------------------------
   function To_Node_Access (X : Node_Access_Impl) return Node_Access is
      use type Primitives.Standard_Unsigned;
      function To_Access is
         new Ada.Unchecked_Conversion (Primitives.Standard_Unsigned,
                                       Node_Access);
      X_Unsigned : constant Primitives.Standard_Unsigned :=
        Primitives.Standard_Unsigned (X);
   begin
      return To_Access (X_Unsigned * Node'Alignment);
   end To_Node_Access;

   ----------------------------------------------------------------------------
   function To_Node_Access_Impl (X : Node_Access) return Node_Access_Impl is
      use type Primitives.Standard_Unsigned;
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Primitives.Standard_Unsigned);
      X_Unsigned : constant Primitives.Standard_Unsigned :=
        To_Unsigned (X);
   begin
      if X_Unsigned mod Node'Alignment /= 0 then
         raise Constraint_Error;
      else
         return Node_Access_Impl (X_Unsigned / Node'Alignment);
      end if;
   end To_Node_Access_Impl;

   ----------------------------------------------------------------------------
   procedure Free (Object : access Move_Info_Record) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Move_Info_Record,
                                    New_Move_Info_Access);
      function To_New_Move_Info_Access is new
        Ada.Unchecked_Conversion (Move_Info_MR_Ops.Node_Access,
                                  New_Move_Info_Access);
      X : New_Move_Info_Access :=
        To_New_Move_Info_Access (Move_Info_MR_Ops.Node_Access (Object));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Queue_Node type from the New_Queue_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Atomic_Move;
