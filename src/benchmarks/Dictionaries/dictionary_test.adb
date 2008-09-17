-------------------------------------------------------------------------------
--  Lock-Free Dicitionary Test - Test benchmark for lock-free dictionaries.
--
--  Copyright (C) 2007 - 2008  Anders Gidenstam
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
--  Description     : Test application for the lock-free dictionaries.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 18 14:51:23 2006
--  $Id: dictionary_test.adb,v 1.5.2.1 2008/09/17 20:35:56 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;

with Ada.Text_IO;
with Ada.Exceptions;
with Ada.Command_Line;

with Ada.Real_Time;

with Ada.Numerics.Discrete_Random;

with Test_Dictionaries;

procedure Dictionary_Test is

   use NBAda;

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 65);

   subtype Key_Type is Natural;
   function Hash_Nat (Ref  : in Natural;
                      Size : in Natural) return Natural;

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Dictionaries is new Test_Dictionaries (Key_Type    => Key_Type,
                                                  Value_Type  => Value_Type,
                                                  Hash        => Hash_Nat,
                                                  Process_Ids => PID);
   use Dictionaries;


   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 1_000;
   subtype Key_Universe is
     Key_Type range 0 .. No_Of_Elements * 100;

   package Random is new Ada.Numerics.Discrete_Random (Natural);

   procedure Print_Usage;
   procedure Put_Line (S : in String);
   procedure Put (S : in String);

   Default_Stack_Size : constant := 1 * 1024 * 1024;

   task type Inserter is
      pragma Storage_Size (Default_Stack_Size);
   end Inserter;
   task type Remover is
      pragma Storage_Size (Default_Stack_Size);
   end Remover;
   task type Finder is
      pragma Storage_Size (Default_Stack_Size);
   end Finder;

   Dictionary  : aliased Dictionaries.Dictionary_Type;

   Start                  : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Start);
   Insert_Count           : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Insert_Count);
   Delete_Count           : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Delete_Count);
   Find_Count             : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Find_Count);
   Found_Count            : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Found_Count);
   No_Inserters_Running   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Inserters_Running);
   No_Removers_Running    : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Removers_Running);
   No_Finders_Running     : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Finders_Running);

   -----------------------------------------------------------------------
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
         Put_Line ("Inserter (" &
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
                  Put ("`");
            end;
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Inserter (" &
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
         Primitives.Fetch_And_Add_32 (Insert_Count'Access, No_Inserts);
         Primitives.Fetch_And_Add_32 (No_Inserters_Running'Access, -1);
      end;
      Put_Line ("Inserter (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Inserter (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end Inserter;

   -----------------------------------------------------------------------
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

         Put_Line ("Remover (" &
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
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Remover (" &
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
         Primitives.Fetch_And_Add_32 (Delete_Count'Access, No_Removes);
         Primitives.Fetch_And_Add_32 (No_Removers_Running'Access, -1);
      end;

      Put_Line ("Remover (?): exited.");
   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Remover (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end Remover;

   -----------------------------------------------------------------------
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

         Put_Line ("Finder (" &
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
            Ada.Text_IO.New_Line;
            Ada.Text_IO.Put_Line ("Finder (" &
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
         Primitives.Fetch_And_Add_32 (Find_Count'Access, No_Finds);
         Primitives.Fetch_And_Add_32 (Found_Count'Access, No_Founds);
         Primitives.Fetch_And_Add_32 (No_Finders_Running'Access, -1);
      end;

      Put_Line ("Finder (?): exited.");
   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("Finder (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end Finder;

   ----------------------------------------------------------------------
   function Hash_Nat (Ref  : in Natural;
                      Size : in Natural) return Natural is
   begin
      return Ref mod Size;
   end Hash_Nat;

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
        ("  -i <#threads>  Set the number of inserter threads.");
      Ada.Text_IO.Put_Line
        ("  -r <#threads>  Set the number of remover threads.");
      Ada.Text_IO.Put_Line
        ("  -f <#threads>  Set the number of finder threads.");
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

   No_Inserters : Natural := 4;
   No_Removers  : Natural := 4;
   No_Finders   : Natural := 2;
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
         elsif Ada.Command_Line.Argument (N) = "-i" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Inserters := T;
            end;
         elsif Ada.Command_Line.Argument (N) = "-r" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Removers := T;
            end;
         elsif Ada.Command_Line.Argument (N) = "-f" then
            declare
               T : Natural;
            begin
               N := N + 1;
               T := Integer'Value (Ada.Command_Line.Argument (N));
               No_Finders := T;
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
   Dictionaries.Init (Dictionary);
   Put_Line (" Dictionary ");

   Put_Line
     ("Testing with " & Natural'Image (No_Inserters) &
      " Inserters / " & Natural'Image (No_Finders) &
      " Finders / " & Natural'Image (No_Removers) &
      " Removers tasks.");
   declare
      use type Primitives.Unsigned_32;
      Inserter_Array : array (1 .. No_Inserters) of Inserter;
      Finder_Array   : array (1 .. No_Finders) of Finder;
      Remover_Array  : array (1 .. No_Removers) of Remover;
   begin

      delay 5.0;
      T1 := Ada.Real_Time.Clock;
      Primitives.Fetch_And_Add_32 (Start'Access, 1);
   end;
   T2 := Ada.Real_Time.Clock;

   delay 1.0;
   Put_Line ("Insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Put_Line ("Find count: " &
                         Primitives.Unsigned_32'Image (Find_Count));
   Put_Line ("Found count: " &
                         Primitives.Unsigned_32'Image (Found_Count));
   Put_Line ("Delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   if Silent then
      Ada.Text_IO.Put (Integer'Image (No_Inserters)  & "  " &
                       Integer'Image (No_Finders)  & "  " &
                       Integer'Image (No_Removers)  & "  " &
                       Primitives.Unsigned_32'Image (Insert_Count) & "  " &
                       Primitives.Unsigned_32'Image (Find_Count)  & "  " &
                       Primitives.Unsigned_32'Image (Found_Count)  & "  " &
                       Primitives.Unsigned_32'Image (Delete_Count)  & "  " &
                       Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) &
                       "  ");
   end if;


   Put_Line ("Emptying dictionary.");
   delay 5.0;

   begin
      for K in Key_Universe loop
         begin
            Delete (From => Dictionary,
                    Key  => K);

            Put_Line ("Deleted " &
                      Key_Universe'Image (K));
            Primitives.Fetch_And_Add_32 (Delete_Count'Access, 1);

         exception
            when Dictionaries.Not_Found =>
               null;
         end;
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put_Line ("raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line;
   end;

   if Silent then
      Ada.Text_IO.Put (Primitives.Unsigned_32'Image (Insert_Count) & "  " &
                       Primitives.Unsigned_32'Image (Find_Count) & "  " &
                       Primitives.Unsigned_32'Image (Found_Count) & "  " &
                       Primitives.Unsigned_32'Image (Delete_Count));
      Ada.Text_IO.New_Line;
   end if;


   Put_Line ("Final insert count: " &
                         Primitives.Unsigned_32'Image (Insert_Count));
   Put_Line ("Final find count: " &
                         Primitives.Unsigned_32'Image (Find_Count));
   Put_Line ("Final found count: " &
                         Primitives.Unsigned_32'Image (Found_Count));
   Put_Line ("Final delete count: " &
                         Primitives.Unsigned_32'Image (Delete_Count));
   --  MRS.Print_Statistics;
end Dictionary_Test;
