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
--  $Id: nbada-atomic_move.adb,v 1.1 2008/01/17 18:54:29 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

package body NBAda.Atomic_Move is

   procedure Compare_And_Swap is
      new Primitives.Compare_And_Swap_64 (Shared_Location);
   procedure Void_Compare_And_Swap is
      new Primitives.Void_Compare_And_Swap_64 (Shared_Location);

   procedure Help_Move (Element : in out Private_Reference;
                        From    : in     Shared_Location_Access;
                        To      : in     Shared_Location_Access;
                        Result  :    out Move_Status);

   subtype Processes is Process_Ids.Process_ID_Type;

   --  Per-task shared locations used by Create and Delete.
   Tmp_Location : array (Processes) of aliased Shared_Location;

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Dereference (Location : access Shared_Location)
                        return Private_Reference is
      Result : Private_Reference;
   begin
      loop
         declare
            Ref : constant Node_Ref := Node_Ref (Location.all);
         begin
            Primitives.Membar;

            Result.Ref      := Ref;
            Result.Location := Shared_Location_Access (Location);

            if Ref.Node = null then
               return Result;
            end if;

            declare
               use Move_Info_LL_SC;
               Status : constant Move_Info := Load_Linked (Ref.Node.Status);
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
      Dummy  : aliased Shared_Location;
      Status : Move_Info;
   begin
      --  Step 1. Initiate move operation.
      loop
         declare
            use Move_Info_LL_SC;
            Old_From : constant Node_Ref := Node_Ref (Element.Location.all);
         begin
            Status := Load_Linked (Element.Ref.Node.Status);
            if Old_From.Node /= Element.Ref.Node then
               Result := Moved_Away;
               return;
            elsif Status.New_Pos = null then
               --  No current operation.
               Element.Ref.Version := Old_From.Version;
               Status := (Current => Old_From.Version,
                          Old_Pos => Element.Location,
                          New_pos => Shared_Location_Access (To));
               if Store_Conditional (Element.Ref.Node.Status'Access,
                                     Status) then
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
      ID  : constant Processes := Process_Ids.Process_ID;
      Tmp : Private_Reference;
   begin
      Tmp.Ref.Node         := new Node;
      Tmp.Ref.Node.Element := Element;
      Tmp.Location         := Tmp_Location (ID)'Access;
      Tmp_Location (ID)    := (Tmp.Ref.Node, 0);
      Initialize (Tmp.Ref.Node.Status, (0,
                                        New_Pos => Null,
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
   begin
      if Ref.Ref.Node = null then
         return "(null)";
      else
         return "(X)";
      end if;
   end Image;

   ----------------------------------------------------------------------------
   function Image (Location : Shared_Location) return String is
   begin
      if Location.Node = null then
         return "(null, " & Version_ID'Image (Location.Version) & ")";
      else
         return "(X, " & Version_ID'Image (Location.Version) & ")";
      end if;
   end Image;

   ----------------------------------------------------------------------------
   function "+" (Ref : Private_Reference) return Element_Access is
   begin
      return Ref.Ref.Node.Element'Access;
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
         Status : Move_Info := Load_Linked (Element.Ref.Node.Status);
      begin
         if
           Status = (Current => Element.Ref.Version,
                     Old_Pos => Element.Location,
                     New_pos => To)
         then
            Status.Current := 0;
            Status.New_Pos := null;
            Store_Conditional (Element.Ref.Node.Status'Access,
                               Status);
         end if;
      end Give_Up;

   begin
      --  Step 2. Update To.
      declare
         use Move_Info_LL_SC;
         Old_To : constant Node_Ref := Node_Ref (To.all);
         Status : Move_Info := Load_Linked (Element.Ref.Node.Status);
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

         if Old_To.Node /= null then
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
                                 Old_Value => (null, Old_To.Version),
                                 New_Value => Shared_Location (Res));
               if Res /=  (null, Old_To.Version) then
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
                             New_Value => (null, Element.Ref.Version + 1));

      --  Step 4. Remove the operation information from Node.
      declare
         use Move_Info_LL_SC;
         Status : Move_Info := Load_Linked (Element.Ref.Node.Status);
      begin
         if
           Status = (Current => Element.Ref.Version,
                     Old_Pos => Element.Location,
                     New_pos => To)
         then
            Status.Current := 0;
            Status.Old_Pos := To;
            Status.New_Pos := null;
            Store_Conditional (Element.Ref.Node.Status'Access,
                               Status);
         end if;

         Result := Moved_Ok;
      end;

   end Help_Move;

end NBAda.Atomic_Move;
