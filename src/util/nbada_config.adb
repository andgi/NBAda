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
--  Filename        : nbada_config.adb
--  Description     : NBAda build config.
--  Author          : Anders Gidenstam
--  Created On      : Thu Aug 30 11:18:46 2007
-- $Id: nbada_config.adb,v 1.2 2007/08/30 14:35:43 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Text_IO;

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

procedure NBAda_Config is

   use Ada.Strings.Unbounded;

   function "+" (Source : in String) return Unbounded_String renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   --  Types.
   type Target is (PRIMITIVES, LF_POOLS, EBMR, HAZARD_POINTERS);
   type Architecture is (IA32, SPARCV8PLUS, SPARCV9, MIPSN32);
   type Target_Array is array (Target) of Boolean;

   ----------------------------------------------------------------------
   --  Configuration.

   --  NBAda source code base directory.
   Install_Base : constant String :=
     "/home/andersg/projects/Ada/Non-Blocking/src";

   --  Architecture dependent compiler flags.
   Compiler_Flags : constant array (Architecture) of Unbounded_String :=
     (IA32        => "+" ("-I" & Install_Base & "/Primitives/IA32"),
      SPARCV8PLUS => "+" ("-I" & Install_Base & "/Primitives/SPARCv8plus " &
                          "-cargs -Wa,-xarch=v8plus"),
      SPARCV9     => "+" ("-I" & Install_Base & "/Primitives/SPARCv9 " &
                          "-m64 --RTS=m64 -cargs -Wa,-xarch=v9"),
      MIPSN32     => "+" ("-I" & Install_Base & "/Primitives/MIPSN32"));

   --  Component include directories.
   Include : constant array (Target) of Unbounded_String :=
     (PRIMITIVES      => "+" ("-I" & Install_Base & "/Primitives"),
      LF_POOLS        =>
        "+" ("-I" & Install_Base & "/Lock-Free_Storage_Pools"),
      EBMR            =>
        "+" ("-I" & Install_Base & "/Epoch-Based_Memory_Reclamation"),
      HAZARD_POINTERS =>
        "+" ("-I" & Install_Base & "/Hazard_Pointers")
      );

   --  Component dependencies.
   --  NOTE: All dependencies except must be explicit.
   Depends : constant array (Target) of Target_Array :=
     (PRIMITIVES      => (others => False),
      LF_POOLS        => (PRIMITIVES => True, others => False),
      EBMR            => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False),
      HAZARD_POINTERS => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False)
      );

   ----------------------------------------------------------------------
   type Config_State is
      record
         ISA    : Architecture  := IA32;
         Target : Target_Array := (PRIMITIVES => True, others => False);
      end record;

   ----------------------------------------------------------------------
   procedure Print_Usage;
   procedure Parse_Arguments (State : in out Config_State);
   function  Compiler_Arguments (State : in Config_State) return String;

   ----------------------------------------------------------------------
   procedure Print_Usage is
      package TIO renames Ada.Text_IO;
      package CL  renames Ada.Command_Line;
   begin
      TIO.Put_Line (TIO.Standard_Error,
                    "Usage: " & CL.Command_Name & " [OPTIONS] [LIBRARIES]");

      TIO.Put_Line (TIO.Standard_Error,
                    "Options:");

      TIO.Put      (TIO.Standard_Error,
                    "        [--isa=<");
      for I in Architecture loop
         TIO.Put (TIO.Standard_Error,
                  Architecture'Image (I));
         if not (I = Architecture'Last) then
            TIO.Put (TIO.Standard_Error,
                     "|");
         end if;
      end loop;
      TIO.Put_Line (TIO.Standard_Error,
                    ">]");

      TIO.Put_Line (TIO.Standard_Error,
                    "        [--help]");

      TIO.Put_Line (TIO.Standard_Error,
                    "Libraries:");

      for L in Target'Range loop
         if L = Target'First then
            TIO.Put_Line (TIO.Standard_Error,
                          "        " & Target'Image (L) & "    (default)");
         else
            TIO.Put_Line (TIO.Standard_Error,
                          "        " & Target'Image (L));
         end if;
      end loop;
   end Print_Usage;

   ----------------------------------------------------------------------
   procedure Parse_Arguments (State : in out Config_State) is
      package CL  renames Ada.Command_Line;
      use Ada.Strings.Fixed;

      Next_Arg : Natural := 1;
   begin
      --  Parse options.
      while Next_Arg <= CL.Argument_Count loop

         declare
            Arg : constant String := CL.Argument (Next_Arg);
         begin
            if Arg = "--help" then
               raise Constraint_Error;

            elsif Index (Arg, "--isa=") /= 0 then
               State.ISA := Architecture'Value (Arg (7 .. Arg'Last));

            else
               exit;

            end if;
         end;

         Next_Arg := Next_Arg + 1;
      end loop;

      --  Parse libraries.
      while Next_Arg <= CL.Argument_Count loop
         declare
            Arg : constant String := CL.Argument (Next_Arg);
            T   : constant Target := Target'Value (Arg);
         begin
            State.Target (T) := True;
            State.Target := State.Target or Depends (T);
         end;

         Next_Arg := Next_Arg + 1;
      end loop;

   end Parse_Arguments;

   ----------------------------------------------------------------------
   function  Compiler_Arguments (State : in Config_State) return String is
      Result : Unbounded_String;
   begin
      --  Base include.
      Append (Result, "+" ("-I" & Install_Base & "/common "));

      --  Component includes.
      for L in Target'Range loop
         if State.Target (L) then
            Append (Result, Include (L));
            Append (Result, ' ');
         end if;
      end loop;

      Append (Result, Compiler_Flags (State.ISA));

      return To_String (Result);
   end Compiler_Arguments;


   ----------------------------------------------------------------------------
   State  : Config_State;
begin
   Parse_Arguments (State);

   Ada.Text_IO.Put (Compiler_Arguments (State));

exception
   when Constraint_Error =>
      Print_Usage;
end NBAda_Config;
