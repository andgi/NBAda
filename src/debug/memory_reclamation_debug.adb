-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
--
--  Copyright (C) 2007  Anders Gidenstam
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
--  Filename        : memory_reclamation_debug.adb
--  Description     : Track local references.
--  Author          : Anders Gidenstam
--  Created On      : Tue Oct 30 10:41:12 2007
--  $Id: memory_reclamation_debug.adb,v 1.2 2008/05/13 12:59:27 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Strings.Fixed;

package body Memory_Reclamation_Debug is

   type Reference_Info is
      record
         Node  : Private_Reference;
         Where : String (1 .. 64) := (others => ' ');
         Used  : Boolean := False;
         Level : Natural := 0;
      end record;
   type Reference_Info_Array is
     array (Positive range <>) of Reference_Info;

   function Image (References : Reference_Array) return String;

   Local_References :
     array (Process_Ids.Process_ID_Type) of
     Reference_Info_Array (1 .. Max_Number_Of_Dereferences);
   pragma Volatile_Components (Local_References);


   ----------------------------------------------------------------------------
   procedure Add_Reference (Node  : Private_Reference;
                            Where : String := "") is
      ID  : constant Process_Ids.Process_ID_Type := Process_Ids.Process_ID;
   begin
      for I in Local_References (ID)'Range loop
         if not Local_References (ID)(I).Used then
            Local_References (ID)(I).Node  := Node;
            Local_References (ID)(I).Where := (others => ' ');
            Local_References (ID)(I).Where (1 .. Where'Length) := Where;
            Local_References (ID)(I).Used  := True;
            Local_References (ID)(I).Level := 0;
            return;
         end if;
      end loop;
      Debug_IO.Put_Line
        ("lock-free-deques.adb:@Task(" &
         Process_Ids.Process_ID_Type'Image (ID) & "): " &
         "Maximum number of local dereferences exceeded at " &
         Where & "!");
      Dump_Local_References (Where);
      raise Reference_Error;
   end Add_Reference;

   ----------------------------------------------------------------------------
   procedure Remove_Reference (Node  : Private_Reference;
                               Where : String := "") is
      use type Private_Reference;
      ID  : constant Process_Ids.Process_ID_Type :=
        Process_Ids.Process_ID;
   begin
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used and
           Local_References (ID)(I).Node = Node
         then
            Local_References (ID)(I).Used := False;
            return;
         end if;
      end loop;
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used and
           Same_Node (Local_References (ID)(I).Node, Node)
         then
            Local_References (ID)(I).Used := False;
            return;
         end if;
      end loop;
      Debug_IO.Put_Line
        ("lock-free-deques.adb@Task(" &
         Process_Ids.Process_ID_Type'Image (ID) & "): " &
         "Unknown references released at " & Where &
         ": " & Image (Node));
      Dump_Local_References (Where);
      raise Reference_Error;
   end Remove_Reference;

   ----------------------------------------------------------------------------
   procedure Verify_Quiescent (Where : String := "") is
      ID  : constant Process_Ids.Process_ID_Type :=
        Process_Ids.Process_ID;
   begin
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used
         then
            Debug_IO.Put_Line
              ("lock-free-deques.adb@Task(" &
               Process_Ids.Process_ID_Type'Image (ID) & ") " &
               "Dangling local references at " & Where);
            Dump_Local_References (Where);
            raise Reference_Error;
         end if;
      end loop;
   end Verify_Quiescent;

   ----------------------------------------------------------------------------
   procedure Enter is
      ID : constant Process_Ids.Process_ID_Type :=
        Process_Ids.Process_ID;
   begin
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used
         then
            Local_References (ID)(I).Level :=
              Local_References (ID)(I).Level + 1;
         end if;
      end loop;
   end Enter;

   ----------------------------------------------------------------------------
   procedure Exit_Quiescent (Where  : String := "") is
      None : Reference_Array (1 .. 0);
   begin
      Exit_Quiescent (Where, None);
   end Exit_Quiescent;

   ----------------------------------------------------------------------------
   procedure Exit_Quiescent (Where  : String := "";
                             Except : Reference_Array) is
      ID : constant Process_Ids.Process_ID_Type :=
        Process_Ids.Process_ID;
      Active : array (Except'Range) of Boolean := (others => True);
      Bad    : Boolean;
   begin
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used and
           Local_References (ID)(I).Level = 0
         then
            Bad := True;
            for J in Except'Range loop
               if
                 Active (J) and then
                 Same_Node (Except (J), Local_References (ID)(I).Node)
               then
                  Active (J) := False;
                  --  Adjust level. This reference now belongs to the parent.
                  Local_References (ID)(I).Level := 1;
                  --  This reference is accounted for.
                  Bad := False;
                  exit;
               end if;
            end loop;

            if Bad then
               Debug_IO.Put_Line
                 ("lock-free-deques.adb@Task(" &
                  Process_Ids.Process_ID_Type'Image (ID) & ") " &
                  "Dangling local reference (" &
                  Image (Local_References (ID)(I).Node) &
                  ") at " & Where & ". " &
                  "Except = " & Image (Except));
               Dump_Local_References (Where);
               raise Reference_Error;
            end if;
         end if;
      end loop;
      for I in Local_References (ID)'Range loop
         if
           Local_References (ID)(I).Used and
           Local_References (ID)(I).Level > 0
         then
            Local_References (ID)(I).Level :=
              Local_References (ID)(I).Level - 1;
         end if;
      end loop;
   end Exit_Quiescent;

   ----------------------------------------------------------------------------
   procedure Dump_Local_References (Where : String := "") is
      ID  : constant Process_Ids.Process_ID_Type :=
        Process_Ids.Process_ID;

      function Image (R : Reference_Info) return String;
      function Image (R : Reference_Info_Array) return String;

      ------------------------------------------------------------
      function Image (R : Reference_Info) return String is
      begin
         return Image (R.Node) & ": " &
           Ada.Strings.Fixed.Trim (R.Where, Side => Ada.Strings.Right) &
           " " & Boolean'Image (R.Used) & Integer'Image (R.Level);
      end Image;

      ------------------------------------------------------------
      function Image (R : Reference_Info_Array) return String is
      begin
         if R'Length = 0 then
            return "";
         elsif R'Length = 1 then
            return Image (R (R'First));
         else
            return Image (R (R'First)) & ", " &
              Image (R (R'First + 1 .. R'Last));
         end if;
      end Image;

   begin
      Debug_IO.Put_Line (Where & ": [" & Image (Local_References (ID)) & "]");
   end Dump_Local_References;

   ----------------------------------------------------------------------------
   function Image (References : Reference_Array) return String is

      function Image_Rec (R : Reference_Array) return String;

      function Image_Rec (R : Reference_Array) return String is
      begin
         if R'Length = 0 then
            return "";
         elsif R'Length = 1 then
            return Image (R (R'First));
         else
            return Image (R (R'First)) & ", " &
              Image_Rec (R (R'First + 1 .. R'Last));
         end if;
      end Image_Rec;

   begin
      return "[" & Image_Rec (References) & "]";
   end Image;


   ----------------------------------------------------------------------------
   package body Debug_IO is

      ----------------------------------------------------------------------
      protected Debug_IO is
         procedure Put_Line (Text : in String);
      end Debug_IO;

      ----------------------------------------------------------------------
      procedure Put_Line (Text : in String) is
      begin
         Debug_IO.Put_Line (Text);
      end Put_Line;

      ----------------------------------------------------------------------
      protected body Debug_IO is
         procedure Put_Line (Text : in String) is
         begin
            Ada.Text_IO.Put_Line (Text);
         end Put_Line;
      end Debug_IO;

   end Debug_IO;

end Memory_Reclamation_Debug;
