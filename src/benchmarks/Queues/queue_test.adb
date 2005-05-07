-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : queue_test.adb
-- Description     : Example application for lock-free reference counting.
-- Author          : Anders Gidenstam
-- Created On      : Wed Apr 13 22:09:40 2005
-- $Id: queue_test.adb,v 1.6 2005/05/07 22:35:41 anders Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with System.Task_Info;

with Example_Queue;

procedure Queue_Test is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 17);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Queues is new Example_Queue (Value_Type  => Value_Type,
                                        Process_Ids => PID);
   use Queues;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 2_000;
   QUEUE_FIFO_PROPERTY_VIOLATION : exception;

   function Pinned_Task return System.Task_Info.Task_Info_Type;

   task type Producer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Producer;

   task type Consumer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Consumer;

   Queue                : aliased Queues.Queue_Type;

   Start                : aliased Primitives.Unsigned_32 := 0;
   Enqueue_Count        : aliased Primitives.Unsigned_32 := 0;
   Dequeue_Count        : aliased Primitives.Unsigned_32 := 0;
   No_Producers_Running : aliased Primitives.Unsigned_32 := 0;
   No_Consumers_Running : aliased Primitives.Unsigned_32 := 0;

   Task_Count : aliased Primitives.Unsigned_32 := 0;
   function Pinned_Task return System.Task_Info.Task_Info_Type is
   begin
      --  GNAT/IRIX
--        return new System.Task_Info.Thread_Attributes'
--          (Scope       => System.Task_Info.PTHREAD_SCOPE_SYSTEM,
--           Inheritance => System.Task_Info.PTHREAD_EXPLICIT_SCHED,
--           Policy      => System.Task_Info.SCHED_RR,
--           Priority    => System.Task_Info.No_Specified_Priority,
--           Runon_CPU   =>
--             System.Task_Info.ANY_CPU
--             --Integer (Primitives.Fetch_And_Add (Task_Count'Access, 1))
--           );
      --  GNAT/Linux
      return System.Task_Info.System_Scope;
   end Pinned_Task;

   ----------------------------------------------------------------------------
   task body Producer is
      No_Enqueues : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Producers_Running'Access, 1);

      declare
         use type Primitives.Unsigned_32;
      begin
         while (Start = 0) loop
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
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Producer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;
      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add (Enqueue_Count'Access, No_Enqueues);
         Primitives.Fetch_And_Add (No_Producers_Running'Access, -1);
      end;
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "Producer (?): exited.");
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
      No_Dequeues : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Consumers_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Last : array (PID.Process_ID_Type) of Integer := (others => 0);
         V    : Value_Type;
         Done : Boolean := False;
      begin

         declare
            use type Primitives.Unsigned_32;
         begin
            while (Start = 0) loop
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
                  Ada.Text_IO.Put (".");
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
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Consumer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;

      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add (Dequeue_Count'Access, No_Dequeues);
         Primitives.Fetch_And_Add (No_Consumers_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            "Consumer (?): exited.");
   end Consumer;

   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Init (Queue);
   Ada.Text_IO.Put_Line (" Queue ");

   Ada.Text_IO.Put_Line ("Testing with producer/consumer tasks.");
   declare
      use type Primitives.Unsigned_32;
      P1, P2 : Producer;--, P3, P4 : Producer;
--      C1 : Consumer;--, C2, C3, C4 : Consumer;
--      P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14 : Producer;
--      C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14 : Consumer;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add (Start'Access, 1);
   end;
   declare
      C1 : Consumer;
   begin
      null;
   end;
   T2 := Ada.Real_Time.Clock;

   delay 1.0;
   Ada.Text_IO.Put_Line ("Enqueue count: " &
                         Primitives.Unsigned_32'Image (Enqueue_Count));
   Ada.Text_IO.Put_Line ("Dequeue count: " &
                         Primitives.Unsigned_32'Image (Dequeue_Count));
   Ada.Text_IO.Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   Ada.Text_IO.Put_Line ("Emptying queue.");
   delay 5.0;

   declare
      V : Value_Type;
   begin
      loop
         V := Dequeue (Queue'Access);
         Ada.Text_IO.Put_Line ("Dequeue() = (" &
                               PID.Process_ID_Type'Image (V.Creator) & ", " &
                               Integer'Image (V.Index) & ")");
         Primitives.Fetch_And_Add (Dequeue_Count'Access, 1);
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);

         Ada.Text_IO.Put_Line ("Final enqueue count: " &
                               Primitives.Unsigned_32'Image (Enqueue_Count));
         Ada.Text_IO.Put_Line ("Final dequeue count: " &
                               Primitives.Unsigned_32'Image (Dequeue_Count));
   end;
end Queue_Test;
