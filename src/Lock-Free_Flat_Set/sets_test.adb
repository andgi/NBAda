-------------------------------------------------------------------------------
--  Lock-free Flat-sets - An implementation of A. Gidenstam et al.'s
--                        flat set algorithm.
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
--  Filename        : sets_test.adb
--  Description     : Based on A. Gidenstam,
--                    M. Papatriantafilou and P. Tsigas,
--                    "Allocating memory in a lock-free manner",
--                    The 13th Annual European Symposium on Algorithms
--                    (ESA 2005), LNCS 3669, pages 329 - 242, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Jan 18 15:14:04 2008
--  $Id: sets_test.adb,v 1.1 2008/01/21 18:21:31 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Flat_Sets;
with NBAda.Process_Identification;
with NBAda.Primitives;

with Ada.Command_Line;
with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Real_Time;
with System.Task_Info;

procedure Sets_Test is

   use NBAda;

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 65);

   type Item_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   package FLS is new Lock_Free_Flat_Sets (Element_Type => Item_Type,
                                           Process_Ids   => PID);
   use FLS;


   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Sets      : constant := 4;
   Set_Size       :  constant := 8;
   No_Of_Moves     : constant := 1_000;
   No_Of_Movers    : Natural  := 4;
   No_Of_Elements  : Natural  := 7;

   type Move_Status is (Get_Ok, Get_Empty, Insert);
   Move_Count : array (Move_Status) of aliased Primitives.Unsigned_32 :=
     (others => 0);

   type Index is mod No_Of_Sets;
   type Set_Array is array (Index) of FLS.Flat_Set_Type (Set_Size);

   ----------------------------------------------------------------------
   function Pinned_Task return System.Task_Info.Task_Info_Type;
   procedure Print_Usage;

   task type Mover is
      pragma Task_Info (Pinned_Task);
      --  pragma Storage_Size (1 * 1024 * 1024);
   end Mover;

   ----------------------------------------------------------------------
   Set : Set_Array;

   Start             : aliased Primitives.Unsigned_32 := 0;
   Set_No            : aliased Primitives.Unsigned_32 := 0;

   ----------------------------------------------------------------------
--   Task_Count : aliased Primitives.Unsigned_32 := 0;
   function Pinned_Task return System.Task_Info.Task_Info_Type is
   begin
      --  GNAT/IRIX
--        return new System.Task_Info.Thread_Attributes'
--          (Scope       => System.Task_Info.PTHREAD_SCOPE_SYSTEM,
--           Inheritance => System.Task_Info.PTHREAD_EXPLICIT_SCHED,
--           Policy      => System.Task_Info.SCHED_RR,
--           Priority    => System.Task_Info.No_Specified_Priority,
--           Runon_CPU   =>
--             --System.Task_Info.ANY_CPU
--             Integer (Primitives.Fetch_And_Add_32 (Task_Count'Access, 1))
--           );
      --  GNAT/Linux
      return System.Task_Info.System_Scope;
      --  GNAT/Solaris
--      return System.Task_Info.New_Bound_Thread_Attributes;
   end Pinned_Task;

   ----------------------------------------------------------------------
   task body Mover is
      Count : array (Move_Status) of Natural :=
        (others => 0);
   begin
      PID.Register;

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            null;
         end loop;
      end;

      declare
         use type Primitives.Unsigned_32;
         ID     : constant PID.Process_ID_Type := PID.Process_ID;
         My_Set : constant Index :=
           Index (Primitives.Fetch_And_Add_32 (Set_No'Access, 1) mod
                  Index'Modulus);
         Target : Index := My_Set;
         Elem   : Element_Reference;
      begin
         for M in 1 .. No_Of_Moves loop
            loop
               Target := Target + 1;
               begin
                  Get_Any (Set (Target),
                           Elem);
                  Count (Get_Ok) := Count (Get_Ok) + 1;
                  Insert (Element => Elem,
                          Into    => Set (My_Set));
                  Count (Insert) := Count (Insert) + 1;
                  exit;

               exception
                  when FLS.Flat_Set_Empty =>
                     Count (Get_Empty) := Count (Get_Empty) + 1;
               end;
            end loop;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Mover (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line;
      end;

      Ada.Text_IO.Put_Line ("Mover (?): exited.");
      declare
         use type Primitives.Unsigned_32;
      begin
         for T in Count'Range loop
            Primitives.Fetch_And_Add_32 (Move_Count (T)'Access,
                                         Primitives.Unsigned_32 (Count (T)));
         end loop;
      end;


   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Mover (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;

   end Mover;

   ----------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line
        ("Usage: " &
         Ada.Command_Line.Command_Name &
         " [OPTION] ");
      Ada.Text_IO.Put_Line
        ("  -h             Print this message.");
   end Print_Usage;

   ----------------------------------------------------------------------------
   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   --  Parse command line.
   declare
      N : Natural := 1;
   begin
      while N <= Ada.Command_Line.Argument_Count loop

         if Ada.Command_Line.Argument (N) = "-h" then
            Print_Usage;
            return;
         else
            Ada.Text_IO.Put_Line ("Unknown option.");
            Ada.Text_IO.New_Line;
            Print_Usage;
            return;
         end if;

         N := N + 1;
      end loop;
   end;

   Ada.Text_IO.Put_Line ("Initializing: ");
   for I in 1 .. No_Of_Elements loop
      declare
         E : Element_Reference := New_Element ((PID.Process_ID, I));
      begin
         Insert (Element => E,
                 Into    => Set (0));
      end;
   end loop;

   for I in Set'Range loop
      Dump (Set (I));
   end loop;

   Ada.Text_IO.Put_Line ("Testing with " &
                         Integer'Image (No_Of_Movers) & " mover tasks.");
   declare
      use type Primitives.Unsigned_32;
      Mover_Array : array (1 .. No_Of_Movers) of Mover;
   begin
      if Mover_Array'First = Mover_Array'Last then  --  Silence warnings.
         null;
      end if;

      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add_32 (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

   delay 1.0;
   Ada.Text_IO.Put_Line
     ("Elapsed time:" &
      Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) & " sec.");
   declare
      use type Primitives.Unsigned_32;
      Sum : Primitives.Unsigned_32 := 0;
   begin
      Ada.Text_IO.Put_Line ("Move counts:");
      for T in Move_Count'Range loop
         Sum := Sum + Move_Count (T);
         Ada.Text_IO.Put_Line ("  " & Move_Status'Image (T) & ": " &
                               Primitives.Unsigned_32'Image (Move_Count (T)));
      end loop;
      Ada.Text_IO.Put_Line ("  Total:" &
                            Primitives.Unsigned_32'Image (Sum));
   end;

   for I in Set'Range loop
      Dump (Set (I));
   end loop;

end Sets_Test;
