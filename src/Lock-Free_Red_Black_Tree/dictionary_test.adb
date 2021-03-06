-------------------------------------------------------------------------------
--  Lock-Free Dicitionaries - An implementation of the lock-free hash table
--                            algorithm by M. Michael.
--
--  Copyright (C) 2007 - 2008 Anders Gidenstam
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
--  Filename        : dictionary_test.adb
--  Description     : Test application for the lock-free dictionary.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 18 14:51:23 2006
--  $Id: dictionary_test.adb,v 1.1 2008/02/26 16:55:14 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with Ada.Numerics.Discrete_Random;

with System.Task_Info;

with My_Dictionary;

procedure Dictionary_Test is

   use NBAda;

   use My_Dictionary;
   use My_Dictionary.Dictionaries;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 10_000;
   subtype Key_Universe is
     My_Dictionary.Key_Type range 0 .. No_Of_Elements * 100;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   package Random is new Ada.Numerics.Discrete_Random (Natural);

   function Pinned_Task return System.Task_Info.Task_Info_Type;

   Default_Stack_Size : constant := 1 * 1024 * 1024;

   task type Inserter is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (Default_Stack_Size);
   end Inserter;
   task type Remover is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (Default_Stack_Size);
   end Remover;
   task type Finder is
      pragma Task_Info (Pinned_Task);
      pragma Storage_Size (Default_Stack_Size);
   end Finder;

   Dictionary  : aliased Dictionaries.Dictionary_Type;

   Start                  : aliased Primitives.Unsigned_32 := 0;
   Insert_Count           : aliased Primitives.Unsigned_32 := 0;
   Delete_Count           : aliased Primitives.Unsigned_32 := 0;
   Find_Count             : aliased Primitives.Unsigned_32 := 0;
   Found_Count            : aliased Primitives.Unsigned_32 := 0;
   No_Inserters_Running   : aliased Primitives.Unsigned_32 := 0;
   No_Removers_Running    : aliased Primitives.Unsigned_32 := 0;
   No_Finders_Running     : aliased Primitives.Unsigned_32 := 0;

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

   ----------------------------------------------------------------------------
   task body Inserter is
      No_Inserts : Primitives.Unsigned_32 := 0;
      Gen : Random.Generator;
   begin
      PID.Register;
--      Primitives.Fetch_And_Add_32 (No_Inserters_Running'Access, 1);
      Random.Reset (Gen,
                    Integer (Primitives.Fetch_And_Add_32
                             (No_Inserters_Running'Access, 1)));

      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         Ada.Text_IO.Put_Line (Output_File,
                               "Inserter (" &
                               PID.Process_ID_Type'Image (ID) &
                               ") started.");

         declare
            use type Primitives.Unsigned_32;
         begin
            while Start = 0 loop
               null;
            end loop;
         end;

         for I in 1 .. No_Of_Elements loop
            begin
               Insert (Into  => Dictionary,
                       Key   =>
                         Random.Random (Gen) mod (Key_Universe'Last + 1),
                       Value => Value_Type'(ID, I));
               No_Inserts := Primitives.Unsigned_32'Succ (No_Inserts);

            exception
               when Dictionaries.Already_Present =>
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
         Primitives.Fetch_And_Add_32 (Insert_Count'Access, No_Inserts);
         Primitives.Fetch_And_Add_32 (No_Inserters_Running'Access, -1);
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
      Primitives.Fetch_And_Add_32 (No_Removers_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Gen  : Random.Generator;
      begin
         Random.Reset (Gen);

         Ada.Text_IO.Put_Line (Output_File,
                               "Remover (" &
                               PID.Process_ID_Type'Image (ID) &
                               ") started.");


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
               Delete (From => Dictionary,
                       Key  =>
                         Random.Random (Gen) mod (Key_Universe'Last + 1)
                       );
               No_Removes := Primitives.Unsigned_32'Succ (No_Removes);
               I := I + 1;

            exception
               when Dictionaries.Not_Found =>
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when No_Inserters_Running = 0;
                  end;
            end;
         end loop;

         for K in Key_Universe loop
            begin
               Delete (From => Dictionary,
                       Key  => K
                       );
               No_Removes := Primitives.Unsigned_32'Succ (No_Removes);

            exception
               when Dictionaries.Not_Found =>
                  null;
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
         Primitives.Fetch_And_Add_32 (Delete_Count'Access, No_Removes);
         Primitives.Fetch_And_Add_32 (No_Removers_Running'Access, -1);
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

   ----------------------------------------------------------------------------
   task body Finder is
      No_Finds  : Primitives.Unsigned_32 := 0;
      No_Founds : Primitives.Unsigned_32 := 0;
   begin
      PID.Register;
      Primitives.Fetch_And_Add_32 (No_Finders_Running'Access, 1);

      declare
         ID   : constant PID.Process_ID_Type := PID.Process_ID;
         Gen  : Random.Generator;
      begin
         Random.Reset (Gen);

         Ada.Text_IO.Put_Line (Output_File,
                               "Finder (" &
                               PID.Process_ID_Type'Image (ID) &
                               ") started.");

         declare
            use type Primitives.Unsigned_32;
         begin
            while Start = 0 loop
               null;
            end loop;
         end;

         loop

            begin
               No_Finds := Primitives.Unsigned_32'Succ (No_Finds);
               declare
                  X : constant Value_Type :=
                    Lookup (From => Dictionary,
                            Key  =>
                              Random.Random (Gen) mod (Key_Universe'Last + 1)
                            );
               begin
                  null;
               end;
               No_Founds := Primitives.Unsigned_32'Succ (No_Founds);

            exception
               when Dictionaries.Not_Found =>
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when No_Removers_Running = 0;
                  end;
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Finder (" &
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
         Primitives.Fetch_And_Add_32 (Find_Count'Access, No_Finds);
         Primitives.Fetch_And_Add_32 (Found_Count'Access, No_Founds);
         Primitives.Fetch_And_Add_32 (No_Finders_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Output_File,
                            "Finder (?): exited.");
   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Finder (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Finder;

   ----------------------------------------------------------------------------
   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Dictionaries.Init (Dictionary);
   Ada.Text_IO.Put_Line (" Dictionary ");

   Verify (Dictionary, True);

   Ada.Text_IO.Put_Line
     ("Testing with Inserters / Finders/ Removers tasks.");
   declare
      use type Primitives.Unsigned_32;
      P0, P1 --  , P2, P3, P4 --  , P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
        : Inserter;
      F0, F1 --  , F2, F3, F4, F5, F6, F7, F8, F9, F10, F11, F12, F13, F14
        : Finder;
      C0, C1 --, C2, C3, C4 --  , C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
        : Remover;
   begin
      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add_32 (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

   delay 1.0;

   --  Verify (Dictionary, True);


   Ada.Text_IO.Put_Line ("Insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Ada.Text_IO.Put_Line ("Find count: " &
                         Primitives.Unsigned_32'Image (Find_Count));
   Ada.Text_IO.Put_Line ("Found count: " &
                         Primitives.Unsigned_32'Image (Found_Count));
   Ada.Text_IO.Put_Line ("Delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   Ada.Text_IO.Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   return;

   Ada.Text_IO.Put_Line ("Emptying dictionary.");
   delay 5.0;

   begin
      for K in Key_Universe loop
         begin
            Delete (From => Dictionary,
                    Key  => K);

            Ada.Text_IO.Put_Line (Output_File,
                                  "Deleted " &
                                  Key_Universe'Image (K));
            Primitives.Fetch_And_Add_32 (Delete_Count'Access, 1);

         exception
            when Dictionaries.Not_Found =>
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
   Ada.Text_IO.Put_Line ("Final find count: " &
                         Primitives.Unsigned_32'Image (Find_Count));
   Ada.Text_IO.Put_Line ("Final found count: " &
                         Primitives.Unsigned_32'Image (Found_Count));
   Ada.Text_IO.Put_Line ("Final delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   --  MRS.Print_Statistics;
end Dictionary_Test;
