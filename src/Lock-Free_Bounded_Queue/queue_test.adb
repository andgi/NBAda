-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : queue_test.adb
-- Description     : Example application for lock-free FIFO queue.
-- Author          : Anders Gidenstam
-- Created On      : Mon Jun 27 19:09:40 2005
-- $Id: queue_test.adb,v 1.1 2005/06/27 17:24:17 anders Exp $
-------------------------------------------------------------------------------

with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with System.Task_Info;

with Lock_Free_Bounded_Queues;

procedure Queue_Test is

   type Process_Id is mod 2**16;
   type Index_Type is mod 2**16;

   type Value_Type is
      record
         Creator : Process_Id;
         Index   : Index_Type;
      end record;
   for Value_Type'Size use 32;
   pragma Atomic (Value_Type);

   Null_0 : constant Value_Type := (Process_Id'Last, Index_Type'Last);
   Null_1 : constant Value_Type := (Process_Id'Last, Index_Type'Last - 1);

   package Queues is
      new Lock_Free_Bounded_Queues (Element_Type => Value_Type,
                                    Null_0       => Null_0,
                                    Null_1       => Null_1);
   use Queues;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 10_000;
   QUEUE_FIFO_PROPERTY_VIOLATION : exception;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   function Pinned_Task return System.Task_Info.Task_Info_Type;

   task type Producer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Producer;

   task type Consumer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Consumer;

   Queue                : aliased Queues.Lock_Free_Queue (2**15);

   Start                : aliased Primitives.Unsigned_32 := 0;
   Enqueue_Count        : aliased Primitives.Unsigned_32 := 0;
   Dequeue_Count        : aliased Primitives.Unsigned_32 := 0;
   No_Producers_Running : aliased Primitives.Unsigned_32 := 0;
   No_Consumers_Running : aliased Primitives.Unsigned_32 := 0;

   Task_Count : aliased Primitives.Unsigned_32 := 0;
   function Pinned_Task return System.Task_Info.Task_Info_Type is
   begin
      --  GNAT/IRIX
      return new System.Task_Info.Thread_Attributes'
        (Scope       => System.Task_Info.PTHREAD_SCOPE_SYSTEM,
         Inheritance => System.Task_Info.PTHREAD_EXPLICIT_SCHED,
         Policy      => System.Task_Info.SCHED_RR,
         Priority    => System.Task_Info.No_Specified_Priority,
         Runon_CPU   =>
           System.Task_Info.ANY_CPU
--           Integer (Primitives.Fetch_And_Add (Task_Count'Access, 1))
         );
      --  GNAT/Linux
--        return System.Task_Info.System_Scope;
   end Pinned_Task;

   ----------------------------------------------------------------------------
   task body Producer is
      No_Enqueues : Primitives.Unsigned_32 := 0;
      ID          : constant Process_Id    :=
        Process_Id (Primitives.Fetch_And_Add (No_Producers_Running'Access, 1));
   begin
      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            null;
         end loop;
      end;

      begin
         for I in 1 .. Index_Type (No_Of_Elements) loop
            Enqueue (Queue, Value_Type'(ID, I));
            No_Enqueues := Primitives.Unsigned_32'Succ (No_Enqueues);
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Producer (" &
                                  Process_Id'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
      end;
      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add (Enqueue_Count'Access, No_Enqueues);
         Primitives.Fetch_And_Add (No_Producers_Running'Access, -1);
      end;
      Ada.Text_IO.Put_Line (Output_File,
                            "Producer (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
      No_Dequeues : Primitives.Unsigned_32 := 0;
      ID          : constant Process_Id :=
        Process_Id (Primitives.Fetch_And_Add (No_Consumers_Running'Access, 1));
   begin
      declare
         Last : array (Process_Id) of Index_Type := (others => 0);
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
               Dequeue (Queue, V);
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
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Consumer (" &
                                  Process_Id'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
      end;

      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add (Dequeue_Count'Access, No_Dequeues);
         Primitives.Fetch_And_Add (No_Consumers_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Output_File,
                            "Consumer (?): exited.");
   exception
      when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Consumer (?): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
   end Consumer;

   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   Ada.Text_IO.Put_Line ("Testing with producer/consumer tasks.");
   declare
      use type Primitives.Unsigned_32;
--      P1, P2, P3, P4 : Producer;
--      C1, C2, C3, C4 : Consumer;
      P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
        : Producer;
      C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
        : Consumer;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add (Start'Access, 1);
   end;
--     declare
--        C1 : Consumer;
--     begin
--        null;
--     end;
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
         Dequeue (Queue, V);
         Ada.Text_IO.Put_Line (Output_File,
                               "Dequeue() = (" &
                               Process_Id'Image (V.Creator) & ", " &
                               Index_Type'Image (V.Index) & ")");
         Primitives.Fetch_And_Add (Dequeue_Count'Access, 1);
      end loop;
   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);

         Ada.Text_IO.Put_Line ("Final enqueue count: " &
                               Primitives.Unsigned_32'Image (Enqueue_Count));
         Ada.Text_IO.Put_Line ("Final dequeue count: " &
                               Primitives.Unsigned_32'Image (Dequeue_Count));
   end;
end Queue_Test;
