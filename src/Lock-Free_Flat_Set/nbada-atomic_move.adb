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
--  $Id: nbada-atomic_move.adb,v 1.4 2008/01/22 15:09:30 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Conversion;
with Ada.Exceptions;

package body NBAda.Atomic_Move is

   procedure Compare_And_Swap is
      new Primitives.Standard_Compare_And_Swap (Shared_Location);
   procedure Void_Compare_And_Swap is
      new Primitives.Standard_Void_Compare_And_Swap (Shared_Location);

   procedure Help_Move (Element : in out Private_Reference;
                        From    : in     Shared_Location_Access;
                        To      : in     Shared_Location_Access;
                        Result  :    out Move_Status);

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
               use Move_Info_LL_SC;
               Status : constant Move_Info := Load_Linked (Node.Status);
            begin
               if
                 Status.New_Pos = null and then
                 Node_Ref (Location.all) = Ref then
                  return Result;
               end if;

               if Status.New_Pos /= null then
                  declare
                     Tmp : Move_Status;
                  begin
                     Help_Move (Element => Result,
                                From    => Status.Old_Pos,
                                To      => Status.New_Pos,
                                Result  => Tmp);
                  end;
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
      Status : Move_Info;
   begin
      if To = Element.Location then
         Result := Moved_Ok;
         return;
      end if;

      --  Step 1. Initiate move operation.
      loop
         declare
            use Move_Info_LL_SC;
            Old_From : constant Node_Ref := Node_Ref (Element.Location.all);
            Node     : constant Node_Access :=
              To_Node_Access (Element.Ref.Node);
         begin
            Status := Load_Linked (Node.Status);
            if Old_From.Node /= Element.Ref.Node then
               Result := Moved_Away;
               return;
            elsif Status.New_Pos = null then
               --  No current operation.
               Element.Ref.Version := Old_From.Version;
               Status := (Current => Old_From.Version,
                          Old_Pos => Element.Location,
                          New_pos =>
                            To_Shared_Location_Access (Location_Access (To)));
               if Store_Conditional (Node.Status'Access, Status) then
                  Help_Move
                    (Element => Element,
                     From    => Status.Old_Pos,
                     To      => Status.New_Pos,
                     Result  => Result);
                  return;
               end if;
            else
               --  Help current operation.
               --  NOTE: For efficiency one might consider just returning
               --        Moved_Away, but that isn't certain - since that
               --        move might result in Not_Moved.
               declare
                  Tmp  : Move_Status;
                  Elem : Private_Reference :=
                    (Ref      => (Element.Ref.Node, Status.Current),
                     Location => Status.Old_Pos);
               begin
                  Help_Move
                    (Element => Elem,
                     From    => Status.Old_Pos,
                     To      => Status.New_Pos,
                     Result  => Tmp);
               end;
            end if;
         end;
      end loop;
   end Move;

   ----------------------------------------------------------------------------
   function Create (Element : Element_Type) return Private_Reference is
      use Move_Info_LL_SC;
      ID       : constant Processes := Process_Ids.Process_ID;
      Tmp      : Private_Reference;
      New_Node : constant Node_Access := new Node;
   begin
      Tmp.Ref.Node      := To_Node_Access_Impl (New_Node);
      New_Node.Element  := Element;
      Tmp.Location      := Tmp_Location (ID)'Access;
      Tmp_Location (ID) := (Tmp.Ref.Node, 0);
      Initialize (New_Node.Status,
                  (0,
                   New_Pos => null,
                   Old_Pos => Tmp.Location));
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
   begin
      if Location.Node = 0 then
         return "(null, " & Version_ID'Image (Location.Version) & ")";
      else
         return "(" &
           Node_Access_Impl'Image (Location.Node * Node'Alignment) &
           ", " & Version_ID'Image (Location.Version) & ")";
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

   procedure Help_Move (Element : in out Private_Reference;
                        From    : in     Shared_Location_Access;
                        To      : in     Shared_Location_Access;
                        Result  :    out Move_Status) is

      procedure Give_Up;

      procedure Give_Up is
         use Move_Info_LL_SC;
         Node : constant Node_Access := To_Node_Access (Element.Ref.Node);
         Status : Move_Info := Load_Linked (Node.Status);
      begin
         if
           Status = (Current => Element.Ref.Version,
                     Old_Pos => Element.Location,
                     New_pos => To)
         then
            Status.Current := 0;
            Status.New_Pos := null;
            Store_Conditional (Node.Status'Access,
                               Status);
         end if;
      end Give_Up;

   begin
      --  Step 2. Update To.
      declare
         use Move_Info_LL_SC;
         Old_To : constant Node_Ref := Node_Ref (To.all);
         Node   : constant Node_Access := To_Node_Access (Element.Ref.Node);
         Status : Move_Info := Load_Linked (Node.Status);
      begin
         if
           Status /= (Current => Element.Ref.Version,
                      Old_Pos => Element.Location,
                      New_pos => To)
         then
            --  Note: We really don't know how the operation ended.
            Result := Helped;
            return;
         end if;

         if Old_To.Node /= 0 then
            if Old_To.Node /= Element.Ref.Node then
               Give_Up;
               Result := Not_Moved;
               --  NOTE: The Not_Move result isn't 100% certain.
               return;
            end if;
            --  Step 2 was done be someone else. Carry on.
         else
            declare
               Res : Node_Ref := (Element.Ref.Node,
                                  Old_To.Version + 1);
            begin
               Compare_And_Swap (Target    => To,
                                 Old_Value => (0, Old_To.Version),
                                 New_Value => Shared_Location (Res));
               if Res /=  (0, Old_To.Version) then
                  --  To was occupied.
                  if Res.Node /= Element.Ref.Node then
                     Give_Up;
                     Result := Not_Moved;
                     return;
                  end if;
                  --  Step 2 was done be someone else. Carry on.
               end if;
               --  Step 2 done. Carry on.
            end;
         end if;
      end;

      --  Step 3. Clear From.
      Void_Compare_And_Swap (Target    => From,
                             Old_Value => Shared_Location (Element.Ref),
                             New_Value => (0, Element.Ref.Version + 1));

      --  Step 4. Remove the operation information from Node.
      declare
         use Move_Info_LL_SC;
         Node   : constant Node_Access := To_Node_Access (Element.Ref.Node);
         Status : Move_Info := Load_Linked (Node.Status);
      begin
         if
           Status = (Current => Element.Ref.Version,
                     Old_Pos => Element.Location,
                     New_pos => To)
         then
            Status.Current := 0;
            Status.Old_Pos := To;
            Status.New_Pos := null;
            Store_Conditional (Node.Status'Access,
                               Status);
         end if;

         Result := Moved_Ok;
      end;

   end Help_Move;

   ----------------------------------------------------------------------------
   function Image (Ref : Node_Ref) return String is
   begin
      if Ref.Node = 0 then
         return "(null, " & Version_ID'Image (Ref.Version) & ")";
      else
         return
           "("  & Node_Access_Impl'Image (Ref.Node * Node'Alignment) &
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

end NBAda.Atomic_Move;
