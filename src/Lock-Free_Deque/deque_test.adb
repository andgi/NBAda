-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : deque_test.adb
--  Description     : Test program for the lock-free deque.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 16 16:06:25 2006
-- $Id: deque_test.adb,v 1.2 2006/02/17 20:04:25 anders Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Process_Identification;
with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with System.Task_Info;

with Lock_Free_Deques;

procedure Deque_Test is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Deques is new Lock_Free_Deques (Value_Type  => Value_Type,
                                           Process_Ids => PID);
   use Deques;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 10_000;
   DEQUE_FIFO_PROPERTY_VIOLATION : exception;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   function Pinned_Task return System.Task_Info.Task_Info_Type;

   task type Left_Producer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Left_Producer;

   task type Right_Consumer is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Right_Consumer;

   Deque                      : aliased Deques.Deque_Type;

   Start                      : aliased Primitives.Unsigned_32 := 0;
   Left_Push_Count            : aliased Primitives.Unsigned_32 := 0;
   Right_Pop_Count            : aliased Primitives.Unsigned_32 := 0;
   No_Producers_Running       : aliased Primitives.Unsigned_32 := 0;
   No_Consumers_Running       : aliased Primitives.Unsigned_32 := 0;

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
--             --System.Task_Info.ANY_CPU
--             Integer (Primitives.Fetch_And_Add (Task_Count'Access, 1))
--           );
      --  GNAT/Linux
      return System.Task_Info.System_Scope;
   end Pinned_Task;

   ----------------------------------------------------------------------------
   task body Left_Producer is
      No_Pushes : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Producers_Running'Access, 1);

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
            Push_Left (Deque, Value_Type'(ID, I));
            No_Pushes := Primitives.Unsigned_32'Succ (No_Pushes);
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Left_Producer (" &
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
         Primitives.Fetch_And_Add (Left_Push_Count'Access, No_Pushes);
         Primitives.Fetch_And_Add (No_Producers_Running'Access, -1);
      end;
      Ada.Text_IO.Put_Line (Output_File,
                            "Left_Producer (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Left_Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Left_Producer;

   ----------------------------------------------------------------------------
   task body Right_Consumer is
      No_Pops : Primitives.Unsigned_32 := 0;
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
            while Start = 0 loop
               null;
            end loop;
         end;

         loop

            begin
               V       := Pop_Right (Deque'Access);
               No_Pops := Primitives.Unsigned_32'Succ (No_Pops);

               Done := False;

               if V.Index <= Last (V.Creator) then
                  raise DEQUE_FIFO_PROPERTY_VIOLATION;
               end if;
               Last (V.Creator) := V.Index;

            exception
               when Deques.Deque_Empty =>
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
                                  "Right_Consumer (" &
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
         Primitives.Fetch_And_Add (Right_Pop_Count'Access, No_Pops);
         Primitives.Fetch_And_Add (No_Consumers_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Output_File,
                            "Right_Consumer (?): exited.");
   exception
      when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Right_Consumer (?): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
   end Right_Consumer;

   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Init (Deque);
   Ada.Text_IO.Put_Line (" Deque ");

   Ada.Text_IO.Put_Line ("Testing with left producer/right consumer tasks.");
   declare
      use type Primitives.Unsigned_32;
      P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
        : Left_Producer;
      C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
        : Right_Consumer;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

--     Push_Left (Deque, Value_Type'(1, 10));
--     Push_Left (Deque, Value_Type'(1, 20));
--     Push_Left (Deque, Value_Type'(1, 30));
--     Push_Left (Deque, Value_Type'(1, 40));

   delay 1.0;
   Ada.Text_IO.Put_Line ("Left_Push count: " &
                         Primitives.Unsigned_32'Image (Left_Push_Count));
   Ada.Text_IO.Put_Line ("Right_Pop count: " &
                         Primitives.Unsigned_32'Image (Right_Pop_Count));
   Ada.Text_IO.Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   Ada.Text_IO.Put_Line ("Emptying deque.");
   delay 5.0;

   declare
      V : Value_Type;
   begin
      loop
         V := Pop_Right (Deque'Access);
         Ada.Text_IO.Put_Line (Output_File,
                               "Pop_Right() = (" &
                               PID.Process_ID_Type'Image (V.Creator) & ", " &
                               Integer'Image (V.Index) & ")");
         Primitives.Fetch_And_Add (Right_Pop_Count'Access, 1);
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

         Ada.Text_IO.Put_Line ("Final push count: " &
                               Primitives.Unsigned_32'Image
                               (Left_Push_Count));
         Ada.Text_IO.Put_Line ("Final pop count: " &
                               Primitives.Unsigned_32'Image
                               (Right_Pop_Count));
   end;
end Deque_Test;
