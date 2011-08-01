-------------------------------------------------------------------------------
--  Priority Queue Test - Test benchmark for lock-free priority queues.
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
--  Filename        : priority_queue_test.adb
--  Description     : Test benchmark for lock-free priority queues.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 20 16:39:08 2003
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Primitives;
with NBAda.Process_Identification;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Real_Time;
with Ada.Command_Line;

with Test_Priority_Queues;

procedure Priority_Queue_Test is

   package PID is
      new NBAda.Process_Identification (Max_Number_Of_Processes => 65);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   function "<" (Left, Right : Value_Type) return Boolean;
   function Image (X : Value_Type) return String;

   package Priority_Queues is
      new Test_Priority_Queues (Element_Type => Value_Type,
                                "<"          => "<",
                                Image        => Image,
                                Process_Ids  => PID);
   use Priority_Queues;

   use NBAda;

   ----------------------------------------------------------------------------
   function "<" (Left, Right : Value_Type) return Boolean is
      use type PID.Process_ID_Type;
   begin
      return Left.Index < Right.Index or
        else (Left.Index = Right.Index and Left.Creator < Right.Creator);
   end "<";

   ----------------------------------------------------------------------------
   function Image (X : Value_Type) return String is
   begin
      return
        "(" &
        Integer'Image (X.Index) & ", " &
        PID.Process_ID_Type'Image (X.Creator) &
        ")";
   end Image;


   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 1000;

   ----------------------------------------------------------------------------
   procedure Print_Usage;
   procedure Put_Line (S : in String);
   procedure Put (S : in String);

   ----------------------------------------------------------------------------
   task type Producer is
   end Producer;
   task type Consumer is
   end Consumer;

   ----------------------------------------------------------------------------
   My_Q : Priority_Queue_Type;

   Start                      : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Start);
   Insert_Count               : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Insert_Count);
   Delete_Min_Count           : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Delete_Min_Count);
   No_Producers_Running       : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Producers_Running);
   No_Consumers_Running       : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Consumers_Running);


   ----------------------------------------------------------------------------
   task body Producer is
      No_Inserts : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, 1);

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            delay 0.0;
         end loop;
      end;

      declare
         ID : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         for I in reverse 1 .. No_Of_Elements loop
            Insert (My_Q,  Value_Type'(Creator => ID, Index => I));
            No_Inserts := Primitives.Unsigned_32'Succ (No_Inserts);
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
         ID : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         Primitives.Fetch_And_Add_32 (Insert_Count'Access, No_Inserts);
         Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);

         Put_Line ("Producer (" &
                   PID.Process_ID_Type'Image (ID) &
                   "): exited.");
      end;

   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
         declare
            use type Primitives.Unsigned_32;
         begin
            Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);
         end;
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
      No_Deletes : Primitives.Unsigned_32 := 0;
      Done       : Boolean := False;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, 1);

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            delay 0.0;
         end loop;
      end;

      loop
         declare
            use type Primitives.Unsigned_32;
            Min : Value_Type;
         begin
            Delete_Min (My_Q, Min);
            No_Deletes := Primitives.Unsigned_32'Succ (No_Deletes);
--            Ada.Text_IO.Put_Line ("Consumer: Delete_Min: " &
--                                  Image (Min));

            Done := False;

         exception
            when Priority_Queues.Queue_Empty =>
               Put (".");

               exit when Done and No_Producers_Running = 0;

               delay 0.0;

               Done := True;
         end;
      end loop;

      declare
         use type Primitives.Unsigned_32;
         ID : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         Primitives.Fetch_And_Add_32 (Delete_Min_Count'Access, No_Deletes);
         Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, -1);

         Put_Line ("Consumer (" &
                   PID.Process_ID_Type'Image (ID) &
                   "): exited.");
      end;

   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Consumer: raised " &
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

   ----------------------------------------------------------------------------
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
   Initialize (My_Q);
   Put_Line (" Priority Queue ");

   Put_Line ("Verifying priority queue...");
--   Verify (My_Q, Print => True);

   delay 1.0;
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
   Put_Line ("Insert count: " &
             Primitives.Unsigned_32'Image (Insert_Count));
   Put_Line ("Delete_Min count: " &
             Primitives.Unsigned_32'Image (Delete_Min_Count));
   Put_Line ("Elapsed time:" &
             Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) & " sec.");

   if Silent then
      Ada.Text_IO.Put
        (Integer'Image (No_Producers)  & "  " &
         Integer'Image (No_Consumers)  & "  " &
         Primitives.Unsigned_32'Image (Insert_Count) & "  " &
         Primitives.Unsigned_32'Image (Delete_Min_Count)  & "  " &
         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) &
         "  ");
   end if;

   Put_Line ("Verifying priority queue...");
--   Verify (My_Q, Print => False);

   delay 5.0;
   Put_Line ("Emptying queue.");

   declare
      Last : Value_Type := (1, Integer'First);
      Min  : Value_Type;
   begin
      loop
         Delete_Min (My_Q, Min);
         Primitives.Fetch_And_Add_32 (Delete_Min_Count'Access, 1);

         Ada.Text_IO.Put_Line ("Delete_Min: " &
                               Image (Min));

         if Min < Last then
            Ada.Text_IO.Put_Line ("Heap property failure! ");
            Ada.Text_IO.Put_Line ("Last " &
                                  Image (Last) &
                                  " got " &
                                  Image (Min) &
                                  ".");
            return;
         end if;
         Last := Min;

      end loop;

   exception
      when Queue_Empty =>
         null;
   end;

   Put_Line ("Final insert count: " &
             Primitives.Unsigned_32'Image (Insert_Count));
   Put_Line ("Final delete_min count: " &
             Primitives.Unsigned_32'Image (Delete_Min_Count));

   if Silent then
      Ada.Text_IO.Put (Primitives.Unsigned_32'Image (Insert_Count) & "  " &
                       Primitives.Unsigned_32'Image (Delete_Min_Count));
      Ada.Text_IO.New_Line;
   end if;

   Put_Line ("Verifying priority queue...");
--   Verify (My_Q, Print => True);

end Priority_Queue_Test;
