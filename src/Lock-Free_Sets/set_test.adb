-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : set_test.adb
--  Description     : Test application for the lock-free sets.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 17:51:23 2006
--  $Id: set_test.adb,v 1.2 2006/03/23 09:57:23 anders Exp $
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

   No_Of_Elements : constant := 1000;
   subtype Key_Universe is My_Set.Key_Type range 0 .. No_Of_Elements * 100;

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
      Gen : Random.Generator;
   begin
      PID.Register;
--      Primitives.Fetch_And_Add (No_Inserters_Running'Access, 1);
      Random.Reset (Gen,
                    Integer (Primitives.Fetch_And_Add
                             (No_Inserters_Running'Access, 1)));

      declare
         use type Primitives.Unsigned_32;
      begin
         while Start = 0 loop
            null;
         end loop;
      end;

      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         for I in 1 .. No_Of_Elements loop
            begin
               Insert (Into  => Set,
                       Key   =>
                         Random.Random (Gen) mod (Key_Universe'Last + 1),
                       Value => Value_Type'(ID, I));
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
                         Random.Random (Gen) mod (Key_Universe'Last + 1)
                       );
               No_Removes := Primitives.Unsigned_32'Succ (No_Removes);
               I := I + 1;
               Done := 0;

            exception
               when Sets.Not_Found =>
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when Done > Key_Universe'Last and
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

   use type Ada.Real_Time.Time;
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

   delay 1.0;
   Ada.Text_IO.Put_Line ("Insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Ada.Text_IO.Put_Line ("Delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   Ada.Text_IO.Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   Ada.Text_IO.Put_Line ("Emptying set.");
   delay 5.0;

   begin
      for K in Key_Universe loop
         begin
            Delete (From => Set,
                    Key  => K);

            Ada.Text_IO.Put_Line (Output_File,
                                  "Deleted " &
                                  Key_Universe'Image (K));
            Primitives.Fetch_And_Add (Delete_Count'Access, 1);

         exception
            when Sets.Not_Found =>
               null;
         end;
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
   end;

   Ada.Text_IO.Put_Line ("Final insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Ada.Text_IO.Put_Line ("Final delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   MRS.Print_Statistics;
end Set_Test;
