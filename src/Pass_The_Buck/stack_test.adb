-------------------------------------------------------------------------------
--  Stack test - A small test benchmark for concurrent stack data-structures.
--  Copyright (C) 2005 - 2008  Anders Gidenstam
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
--  Filename        : stack_test.adb
--  Description     : Test of the lock-free stack.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 18:54:53 2005
--  $Id: stack_test.adb,v 1.4.2.1 2008/09/17 21:39:41 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with Lock_Free_Stack;

with Ada.Command_Line;

procedure Stack_Test is

   use NBAda;

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 65);


   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Stacks is new Lock_Free_Stack (Element_Type => Value_Type);
   use Stacks;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 100_000;
   STACK_LIFO_PROPERTY_VIOLATION : exception;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;


   procedure Print_Usage;
   procedure Put_Line (S : in String);

   task type Pusher is
      pragma Storage_Size (1 * 1024 * 1024);
   end Pusher;

   task type Popper is
      pragma Storage_Size (1 * 1024 * 1024);
   end Popper;

   Stack                : aliased Stacks.Stack_Type;

   Start                : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Start);
   Push_Count           : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Push_Count);
   Pop_Count            : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Pop_Count);
   No_Pushers_Running   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Pushers_Running);
   No_Poppers_Running   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Poppers_Running);

   ----------------------------------------------------------------------------
   task body Pusher is
      No_Pushes : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Pushers_Running'Access, 1);

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            null;
         end loop;
      end;

      declare
         ID          : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         for I in 1 .. No_Of_Elements loop
            Push (Stack, Value_Type'(ID, I));
            No_Pushes := Primitives.Unsigned_32'Succ (No_Pushes);
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Pusher (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
      end;
      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add_32 (Push_Count'Access, No_Pushes);
         Primitives.Fetch_And_Add_32 (No_Pushers_Running'Access, -1);
      end;
      Put_Line ("Pusher (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Pusher (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Pusher;

   ----------------------------------------------------------------------------
   task body Popper is
      No_Pops : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Poppers_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Last : array (PID.Process_ID_Type) of Integer := (others => 0);
         V    : Value_Type;
         Done : Boolean := False;
         pragma Volatile (Done); --  Strange GNAT GPL 2008 workaround.
      begin

         declare
            use type Primitives.Unsigned_32;
         begin
            while Start = 0 loop
               null;
            end loop;
         end;

         loop

            begin
               V       := Pop (Stack'Access);
               No_Pops := Primitives.Unsigned_32'Succ (No_Pops);

               Done := False;

--                 if V.Index <= Last (V.Creator) then
--                    raise QUEUE_FIFO_PROPERTY_VIOLATION;
--                 end if;
--                 Last (V.Creator) := V.Index;

            exception
               when Stacks.Stack_Empty =>
--                  Ada.Text_IO.Put (".");
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when Done and No_Pushers_Running = 0;
                  end;
                  delay 0.0;

                  Done := True;
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Popper (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
      end;

      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add_32 (Pop_Count'Access, No_Pops);
         Primitives.Fetch_And_Add_32 (No_Poppers_Running'Access, -1);
      end;

      Put_Line ("Popper (?): exited.");
   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Consumer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Popper;

   ----------------------------------------------------------------------
   procedure Print_Usage is
   begin
      Ada.Text_IO.Put_Line
        ("Usage: " &
         Ada.Command_Line.Command_Name &
         " [OPTION] ");
      Ada.Text_IO.Put_Line
        ("  -h             Print this message.");
      Ada.Text_IO.Put_Line
        ("  -o <#threads>  Set the number of popper threads.");
      Ada.Text_IO.Put_Line
        ("  -u <#threads>  Set the number of pusher threads.");
      Ada.Text_IO.Put_Line
        ("  -s             Single line output.");
   end Print_Usage;

   ----------------------------------------------------------------------
   Silent : Boolean := False;
   procedure Put_Line (S : in String) is
   begin
      if not Silent then
         Ada.Text_IO.Put_Line (S);
      end if;
   end Put_Line;


   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;

   No_Pushers : Natural := 1;
   No_Poppers : Natural := 1;
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
         elsif Ada.Command_Line.Argument (N) = "-o" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Poppers := T;
            end;
         elsif Ada.Command_Line.Argument (N) = "-u" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Pushers := T;
            end;
         elsif Ada.Command_Line.Argument (N) = "-s" then
            Silent := True;
         else
            Ada.Text_IO.Put_Line ("Unknown option.");
            Ada.Text_IO.New_Line;
            Print_Usage;
            return;
         end if;

         N := N + 1;
      end loop;
   end;

   Put_Line ("Testing with " &
             Integer'Image (No_Pushers) & " pusher and " &
             Integer'Image (No_Poppers) & " popper tasks.");
   declare
      use type Primitives.Unsigned_32;
      Pusher_Array : array (1 .. No_Pushers) of Pusher;
      Popper_Array : array (1 .. No_Poppers) of Popper;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add_32 (Start'Access, 1);
   end;

   T2 := Ada.Real_Time.Clock;


   delay 1.0;
   Put_Line ("Push count: " &
             Primitives.Unsigned_32'Image (Push_Count));
   Put_Line ("Pop count: " &
             Primitives.Unsigned_32'Image (Pop_Count));
   Put_Line ("Elapsed time:" &
             Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   if Silent then
      Ada.Text_IO.Put (Integer'Image (No_Pushers)  & "  " &
                       Integer'Image (No_Poppers)  & "  " &
                       Primitives.Unsigned_32'Image (Push_Count) & "  " &
                       Primitives.Unsigned_32'Image (Pop_Count)  & "  " &
                       Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) &
                       "  ");
   end if;

   Put_Line ("Emptying stack.");
   delay 5.0;

   declare
      V : Value_Type;
   begin
      loop
         V := Pop (Stack'Access);
         Put_Line ("Pop() = (" &
                   PID.Process_ID_Type'Image (V.Creator) & ", " &
                   Integer'Image (V.Index) & ")");
         Primitives.Fetch_And_Add_32 (Pop_Count'Access, 1);
      end loop;
   exception
      when Stacks.Stack_Empty =>
         null;
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end;

   Put_Line ("Final push count: " &
             Primitives.Unsigned_32'Image (Push_Count));
   Put_Line ("Final pop count: " &
             Primitives.Unsigned_32'Image (Pop_Count));

   if Silent then
      Ada.Text_IO.Put (Primitives.Unsigned_32'Image (Push_Count) & "  " &
                       Primitives.Unsigned_32'Image (Pop_Count));
      Ada.Text_IO.New_Line;
   end if;

--   HP.Print_Statistics;
end Stack_Test;
