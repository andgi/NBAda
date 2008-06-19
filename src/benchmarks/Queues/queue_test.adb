-------------------------------------------------------------------------------
--  Lock-free Queue Test - Test benchmark for lock-free queues.
--
--  Copyright (C) 2004 - 2008  Anders Gidenstam
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
--  Filename        : queue_test.adb
--  Description     : Benchmark application for lock-free queues.
--  Author          : Anders Gidenstam
--  Created On      : Wed Apr 13 22:09:40 2005
--  $Id: queue_test.adb,v 1.15 2008/06/19 17:06:16 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with System.Task_Info;

with Test_Queues;

with Ada.Command_Line;

procedure Queue_Test is

   use NBAda;

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 65);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Queues is new Test_Queues (Element_Type  => Value_Type,
                                      Process_Ids   => PID);
   use Queues;


   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 100_000;
   QUEUE_FIFO_PROPERTY_VIOLATION : exception;

   procedure Print_Usage;
   procedure Put_Line (S : in String);
   procedure Put (S : in String);

   task type Producer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Producer;

   task type Consumer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Consumer;

   Queue                : aliased Queues.Queue_Type;

   Start                : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Start);
   Enqueue_Count        : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Enqueue_Count);
   Dequeue_Count        : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Dequeue_Count);
   No_Producers_Running : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Producers_Running);
   No_Consumers_Running : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Consumers_Running);

   ----------------------------------------------------------------------------
   task body Producer is
      No_Enqueues : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, 1);

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
            Enqueue (Queue, Value_Type'(ID, I));
            No_Enqueues := Primitives.Unsigned_32'Succ (No_Enqueues);
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Producer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line;
      end;
      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add_32 (Enqueue_Count'Access, No_Enqueues);
         Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);
      end;
      Put_Line ("Producer (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
      No_Dequeues : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Last : array (PID.Process_ID_Type) of Integer := (others => 0);
         V    : Value_Type;
         Done : Boolean := False;
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
               V           := Dequeue (Queue'Access);
               No_Dequeues := Primitives.Unsigned_32'Succ (No_Dequeues);

               Done := False;

               if V.Index <= Last (V.Creator) then
                  raise QUEUE_FIFO_PROPERTY_VIOLATION;
               end if;
               Last (V.Creator) := V.Index;

            exception
               when Queues.Queue_Empty =>
                  Put (".");
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when Done and No_Producers_Running = 0;
                  end;
                  delay 0.0;

                  Done := True;
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Consumer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line;
      end;

      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add_32 (Dequeue_Count'Access, No_Dequeues);
         Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, -1);
      end;

      Put_Line ("Consumer (?): exited.");
   exception
      when E : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Consumer (?): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line;
   end Consumer;

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
        ("  -p <#threads>  Set the number of producer threads.");
      Ada.Text_IO.Put_Line
        ("  -c <#threads>  Set the number of consumer threads.");
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
   procedure Put (S : in String) is
   begin
      if not Silent then
         Ada.Text_IO.Put (S);
      end if;
   end Put;


   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;

   No_Producers : Natural := 4;
   No_Consumers : Natural := 4;
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
         elsif Ada.Command_Line.Argument (N) = "-p" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Producers := T;
            end;
         elsif Ada.Command_Line.Argument (N) = "-c" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Consumers := T;
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

   Put ("Initializing: ");
   Init (Queue);
   Put_Line (" Queue ");

   Put_Line ("Testing with " &
             Integer'Image (No_Producers) & " producer and " &
             Integer'Image (No_Consumers) & " consumer tasks.");
   declare
      use type Primitives.Unsigned_32;
      Producer_Array : array (1 .. No_Producers) of Producer;
      Consumer_Array : array (1 .. No_Consumers) of Consumer;
   begin
      if Producer_Array'First = Consumer_Array'Last then  --  Silence warnings.
         null;
      end if;

      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add_32 (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

   delay 1.0;
   Put_Line ("Enqueue count: " &
             Primitives.Unsigned_32'Image (Enqueue_Count));
   Put_Line ("Dequeue count: " &
             Primitives.Unsigned_32'Image (Dequeue_Count));
   Put_Line ("Elapsed time:" &
             Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) & " sec.");

   if Silent then
      Ada.Text_IO.Put (Integer'Image (No_Producers)  & "  " &
                       Integer'Image (No_Consumers)  & "  " &
                       Primitives.Unsigned_32'Image (Enqueue_Count) & "  " &
                       Primitives.Unsigned_32'Image (Dequeue_Count)  & "  " &
                       Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) &
                       "  ");
   end if;

   Put_Line ("Emptying queue.");
   delay 5.0;

   declare
      V : Value_Type;
   begin
      loop
         V := Dequeue (Queue'Access);
         Put_Line ("Dequeue() = (" &
                   PID.Process_ID_Type'Image (V.Creator) & ", " &
                   Integer'Image (V.Index) & ")");
         Primitives.Fetch_And_Add_32 (Dequeue_Count'Access, 1);
      end loop;
   exception
      when Queues.Queue_Empty =>
         null;
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end;

   Put_Line ("Final enqueue count: " &
             Primitives.Unsigned_32'Image (Enqueue_Count));
   Put_Line ("Final dequeue count: " &
             Primitives.Unsigned_32'Image (Dequeue_Count));

   if Silent then
      Ada.Text_IO.Put (Primitives.Unsigned_32'Image (Enqueue_Count) & "  " &
                       Primitives.Unsigned_32'Image (Dequeue_Count));
      Ada.Text_IO.New_Line;
   end if;

   --  Queues.Print_Statistics;

end Queue_Test;
