-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
--
--  Copyright (C) 2007 - 2011  Anders Gidenstam
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
--  $Id: nbada_config.adb,v 1.20 2008/06/26 09:20:30 andersg Exp $
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
   type Target is
     (
      --  Support packages.
      PRIMITIVES,
      LF_POOLS,
      --  Memory reclamation schemes.
      --    Non-traversable.
      EBMR,
      HPMR,
      PTB,
      --    Traversable.
      LFRC,
      LFMR,
      --  Algorithms and data structures.
      --    Released.
      SW_LL_SC,
      LF_STACKS_EBMR,
      LF_STACKS_HPMR,
      LF_QUEUES_BOUNDED,
      LF_QUEUES_EBMR,
      LF_QUEUES_HPMR,
      LF_QUEUES_LFMR,
      LF_QUEUES_LFRC,
      LF_QUEUES_LFMR_2,
      LF_QUEUES_LFRC_2,
      LF_DEQUES_LFMR,
      LF_DEQUES_LFRC,
      LF_PRIORITY_QUEUES_EBMR,
      LF_PRIORITY_QUEUES_HPMR,
      LF_SETS_EBMR,
      LF_SETS_HPMR,
      LF_DICTIONARIES_EBMR,
      LF_DICTIONARIES_HPMR,
      LF_UBTREES_LFMR,
      LF_UBTREES_LFRC
      );
   type Architecture is (IA32, SPARCV8PLUS, SPARCV9, MIPSN32);
   type Target_Array is array (Target) of Boolean;

   ----------------------------------------------------------------------
   --  Configuration.

   --  NBAda source code base directory.
   Install_Base : constant String :=
     "/home/anders/projects/Ada/Non-Blocking/NBAda.git/src";
   --  Default architecture.
   Default_Architecture : constant Architecture := IA32;

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
     (PRIMITIVES =>
        "+" ("-I" & Install_Base & "/Primitives"),
      LF_POOLS =>
        "+" ("-I" & Install_Base & "/Lock-Free_Storage_Pools"),
      --  Memory reclamation schemes.
      EBMR =>
        "+" ("-I" & Install_Base & "/Epoch-Based_Memory_Reclamation"),
      HPMR =>
        "+" ("-I" & Install_Base & "/Hazard_Pointers"),
      PTB =>
        "+" ("-I" & Install_Base & "/Pass_The_Buck"),
      LFRC =>
        "+" ("-I" & Install_Base & "/Lock-Free_Reference_Counting"),
      LFMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Memory_Reclamation " &
             "-I" & Install_Base &
             "/Lock-Free_Memory_Reclamation/uncontrolled"),
      --  Algorithms and data structures.
      SW_LL_SC =>
        "+" ("-I" & Install_Base & "/Lock-Free_LL_SC"),
      LF_STACKS_EBMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Stack " &
             "-I" & Install_Base & "/Lock-Free_Stack/EBMR"),
      LF_STACKS_HPMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Stack " &
             "-I" & Install_Base & "/Lock-Free_Stack/HPMR"),
      LF_QUEUES_BOUNDED =>
        "+" ("-I" & Install_Base & "/Lock-Free_Bounded_Queue"),
      LF_QUEUES_EBMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue " &
             "-I" & Install_Base & "/Lock-Free_Queue/EBMR"),
      LF_QUEUES_HPMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue " &
             "-I" & Install_Base & "/Lock-Free_Queue/HPMR"),
      LF_QUEUES_LFMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue_2 " &
             "-I" & Install_Base & "/Lock-Free_Queue_2/LFMR"),
      LF_QUEUES_LFRC =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue_2 " &
             "-I" & Install_Base & "/Lock-Free_Queue_2/LFRC"),
      LF_QUEUES_LFMR_2 =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue_3 " &
             "-I" & Install_Base & "/Lock-Free_Queue_3/LFMR"),
      LF_QUEUES_LFRC_2 =>
        "+" ("-I" & Install_Base & "/Lock-Free_Queue_3 " &
             "-I" & Install_Base & "/Lock-Free_Queue_3/LFRC"),
      LF_DEQUES_LFMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Deque " &
             "-I" & Install_Base & "/Lock-Free_Deque/LFMR"),
      LF_DEQUES_LFRC =>
        "+" ("-I" & Install_Base & "/Lock-Free_Deque " &
             "-I" & Install_Base & "/Lock-Free_Deque/LFRC"),
      LF_PRIORITY_QUEUES_EBMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Priority_Queue " &
             "-I" & Install_Base & "/Lock-Free_Priority_Queue/EBMR"),
      LF_PRIORITY_QUEUES_HPMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Priority_Queue " &
             "-I" & Install_Base & "/Lock-Free_Priority_Queue/HPMR"),
      LF_SETS_EBMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Sets " &
             "-I" & Install_Base & "/Lock-Free_Sets/EBMR"),
      LF_SETS_HPMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Sets " &
             "-I" & Install_Base & "/Lock-Free_Sets/HPMR"),
      LF_DICTIONARIES_EBMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Dictionary"),
      LF_DICTIONARIES_HPMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Dictionary"),
      LF_UBTREES_LFMR =>
        "+" ("-I" & Install_Base & "/Lock-Free_Red_Black_Tree " &
             "-I" & Install_Base & "/Lock-Free_Red_Black_Tree/LFMR"),
      LF_UBTREES_LFRC =>
        "+" ("-I" & Install_Base & "/Lock-Free_Red_Black_Tree " &
             "-I" & Install_Base & "/Lock-Free_Red_Black_Tree/LFRC")
      );

   --  Component dependencies.
   --  NOTE: All dependencies must be explicit.
   Depends : constant array (Target) of Target_Array :=
     (PRIMITIVES      => (others => False),
      LF_POOLS        => (PRIMITIVES => True, others => False),
      --  Memory reclamation schemes.
      EBMR            => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False),
      HPMR            => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False),
      PTB             => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False),
      LFRC            => (PRIMITIVES => True,
                          LF_POOLS   => True,
                          PTB        => True, others => False),
      LFMR            => (PRIMITIVES => True,
                          LF_POOLS   => True, others => False),
      --  Algorithms and data structures.
      SW_LL_SC        => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          HPMR            => True, others => False),
      LF_STACKS_EBMR  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          EBMR            => True, others => False),
      LF_STACKS_HPMR  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          HPMR            => True, others => False),
      LF_QUEUES_BOUNDED => (PRIMITIVES    => True, others => False),
      LF_QUEUES_EBMR  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          EBMR            => True, others => False),
      LF_QUEUES_HPMR  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          HPMR            => True, others => False),
      LF_QUEUES_LFMR  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          LFMR            => True, others => False),
      LF_QUEUES_LFRC  => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          PTB             => True,
                          LFRC            => True, others => False),
      LF_QUEUES_LFMR_2 => (PRIMITIVES      => True,
                           LF_POOLS        => True,
                           LFMR            => True, others => False),
      LF_QUEUES_LFRC_2 => (PRIMITIVES      => True,
                           LF_POOLS        => True,
                           PTB             => True,
                           LFRC            => True, others => False),
      LF_DEQUES_LFMR  => (PRIMITIVES => True,
                          LF_POOLS   => True,
                          LFMR       => True, others => False),
      LF_DEQUES_LFRC  => (PRIMITIVES => True,
                          LF_POOLS   => True,
                          PTB        => True,
                          LFRC       => True, others => False),
      LF_PRIORITY_QUEUES_EBMR  => (PRIMITIVES      => True,
                                   LF_POOLS        => True,
                                   EBMR            => True, others => False),
      LF_PRIORITY_QUEUES_HPMR  => (PRIMITIVES      => True,
                                   LF_POOLS        => True,
                                   HPMR            => True, others => False),
      LF_SETS_EBMR    => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          EBMR            => True, others => False),
      LF_SETS_HPMR    => (PRIMITIVES      => True,
                          LF_POOLS        => True,
                          HPMR            => True, others => False),
      LF_DICTIONARIES_EBMR => (PRIMITIVES      => True,
                               LF_POOLS        => True,
                               EBMR            => True,
                               LF_SETS_EBMR    => True, others => False),
      LF_DICTIONARIES_HPMR => (PRIMITIVES      => True,
                               LF_POOLS        => True,
                               HPMR            => True,
                               LF_SETS_HPMR    => True, others => False),
      LF_UBTREES_LFMR =>      (PRIMITIVES => True,
                               LF_POOLS   => True,
                               HPMR       => True,
                               LFMR       => True, others => False),
      LF_UBTREES_LFRC =>      (PRIMITIVES => True,
                               LF_POOLS   => True,
                               HPMR       => True,
                               PTB        => True,
                               LFRC       => True, others => False)
      );

   ----------------------------------------------------------------------
   type Config_State is
      record
         ISA    : Architecture  := Default_Architecture;
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
