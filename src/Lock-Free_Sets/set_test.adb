-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : set_test.adb
--  Description     : Test application for the lock-free sets.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 17:51:23 2006
--  $Id: set_test.adb,v 1.1 2006/03/10 18:43:50 anders Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with Ada.Numerics.Discrete_Random;

with System.Task_Info;

with My_Set;

procedure Set_Test is

   use My_Set;
   use My_Set.Sets;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 100;
   Key_Universe   : constant := No_Of_Elements / 10;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   package Random is new Ada.Numerics.Discrete_Random (Natural);

   function Pinned_Task return System.Task_Info.Task_Info_Type;

   task type Inserter is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Inserter;
   task type Remover is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (1 * 1024 * 1024);
   end Remover;
--   task type Finder is
--      pragma Task_Info (Pinned_Task);
--      pragma Storage_Size (1 * 1024 * 1024);
--   end Finder;

   Set                    : aliased Sets.Set_Type;

   Start                  : aliased Primitives.Unsigned_32 := 0;
   Insert_Count           : aliased Primitives.Unsigned_32 := 0;
   Delete_Count           : aliased Primitives.Unsigned_32 := 0;
   Find_Count             : aliased Primitives.Unsigned_32 := 0;
   No_Inserters_Running   : aliased Primitives.Unsigned_32 := 0;
   No_Removers_Running    : aliased Primitives.Unsigned_32 := 0;

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
   task body Inserter is
      No_Inserts : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Inserters_Running'Access, 1);

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            null;
         end loop;
      end;

      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
         Gen : Random.Generator;
      begin
         Random.Reset (Gen);

         for I in 1 .. No_Of_Elements loop
            begin
               Ada.Text_IO.Put (',');
               Insert (Into  => Set,
                       Key   =>
                         I,
                         --Random.Random (Gen) mod Key_Universe,
                       Value => Value_Type'(ID, I));
               Ada.Text_IO.Put ('.');
               No_Inserts := Primitives.Unsigned_32'Succ (No_Inserts);

            exception
               when Sets.Already_Present =>
                  Ada.Text_IO.Put ('`');
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Inserter (" &
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
         Primitives.Fetch_And_Add (Insert_Count'Access, No_Inserts);
         Primitives.Fetch_And_Add (No_Inserters_Running'Access, -1);
      end;
      Ada.Text_IO.Put_Line (Output_File,
                            "Inserter (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Inserter (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Inserter;

   ----------------------------------------------------------------------------
   task body Remover is
      No_Removes : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Removers_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Gen  : Random.Generator;
         Done : Natural := 0;
      begin
         Random.Reset (Gen);

         declare
            use type Primitives.Unsigned_32;
         begin
            while Start = 0 loop
               null;
            end loop;
         end;

         loop

            declare
               I : Natural := 1;
            begin
               Delete (From => Set,
                       Key  =>
                         I
                       --Random.Random (Gen) mod Key_Universe
                       );
               No_Removes := Primitives.Unsigned_32'Succ (No_Removes);
               I := I + 1;
               Done := 0;

            exception
               when Sets.Not_Found =>
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when Done > Key_Universe and
                       No_Inserters_Running = 0;
                  end;

                  Done := Done + 1;
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Remover (" &
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
         Primitives.Fetch_And_Add (Delete_Count'Access, No_Removes);
         Primitives.Fetch_And_Add (No_Removers_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Output_File,
                            "Remover (?): exited.");
   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Remover (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Remover;

   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Sets.Init (Set);
   Ada.Text_IO.Put_Line (" Set ");

   Ada.Text_IO.Put_Line
     ("Testing with Inserters / Removers tasks.");
   declare
      use type Primitives.Unsigned_32;
      P0--, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
        : Inserter;
      C0--, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
        : Remover;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

end Set_Test;
