-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - An implementation of the lock-free skip-list
--                              algorithm by H. Sundell.
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
--  Filename        : priority_queue_test.adb
--  Description     : Test of non-blocking priority queue.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 20 16:39:08 2003
--  $Id: priority_queue_test.adb,v 1.3 2007/10/31 17:21:54 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;
with System.Task_Info;

with My_Priority_Queue;

with NBAda.Primitives;

procedure Priority_Queue_Test is

   use NBAda;

   use My_Priority_Queue.Priority_Queues;
   use My_Priority_Queue;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 100;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   ----------------------------------------------------------------------------
   function Pinned_Task return System.Task_Info.Task_Info_Type;

   ----------------------------------------------------------------------------
   task type Producer is
      pragma Task_Info (Pinned_Task);
   end Producer;
   task type Consumer is
      pragma Task_Info (Pinned_Task);
   end Consumer;

   ----------------------------------------------------------------------------
   My_Q : My_Priority_Queue.Priority_Queues.Priority_Queue_Type;

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


   --   Task_Count : aliased Primitives.Unsigned_32 := 0;
   ----------------------------------------------------------------------------
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
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Producer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
      end;

      declare
         use type Primitives.Unsigned_32;
         ID : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         Primitives.Fetch_And_Add_32 (Insert_Count'Access, No_Inserts);
         Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);

         Ada.Text_IO.Put_Line (Output_File,
                               "Producer (" &
                               PID.Process_ID_Type'Image (ID) &
                               "): exited.");
      end;

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
         declare
            use type Primitives.Unsigned_32;
         begin
            Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);
         end;
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
      Min        : Value_Type;
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
         begin
            Delete_Min (My_Q, Min);
            No_Deletes := Primitives.Unsigned_32'Succ (No_Deletes);
            Ada.Text_IO.Put_Line ("Consumer: Delete_Min: " &
                                  Image (Min));

            Done := False;

         exception
            when My_Priority_Queue.Priority_Queues.Queue_Empty =>
               Ada.Text_IO.Put (".");

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

         Ada.Text_IO.Put_Line (Output_File,
                               "Consumer (" &
                               PID.Process_ID_Type'Image (ID) &
                               "): exited.");
      end;

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Consumer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Consumer;

begin
   PID.Register;
   Initialize (My_Q);
   Ada.Text_IO.Put_Line ("Verifying priority queue...");
   Verify (My_Q, Print => True);
   delay 1.0;
   Ada.Text_IO.Put_Line ("Testing with producer/consumer tasks...");

   declare
      P1, P2, P3, P4
        : Producer;
--      C1, C2--, C3
--        : Consumer;
   begin
      Start := 1;
   end;
   Ada.Text_IO.Put_Line ("Verifying priority queue...");
   Verify (My_Q, Print => False);

   delay 5.0;
   Ada.Text_IO.Put_Line ("Emptying queue.");

   declare
      Last : Value_Type := (1, Integer'First);
      Min  : Value_Type;
   begin
      loop
         Delete_Min (My_Q, Min);
         Primitives.Fetch_And_Add_32 (Delete_Min_Count'Access, 1);

         Ada.Text_IO.Put_Line ("Heap_Test: Delete_Min: " &
                               Image (Min));

         if Min < Last then
            Ada.Text_IO.Put_Line ("Heap_Test: Heap property failure! ");
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Heap_Test: Heap property failure! ");
            Ada.Text_IO.Put_Line ("Heap_Test: Last " &
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

   Ada.Text_IO.Put_Line ("Final insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Ada.Text_IO.Put_Line ("Final delete_min count: " &
                         Primitives.Unsigned_32'Image (Delete_Min_Count));


   Ada.Text_IO.Put_Line ("Verifying priority queue...");
   Verify (My_Q, Print => True);

end Priority_Queue_Test;
