-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
--
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--  Filename        : deque_test.adb
--  Description     : Test program for the lock-free deque.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 16 16:06:25 2006
-- $Id: deque_test.adb,v 1.11.2.1 2008/09/17 21:26:21 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with My_Deque;

procedure Deque_Test is

   use NBAda;

   use My_Deque;
   use My_Deque.Deques;

   ----------------------------------------------------------------------------
   --  Test application.
   ----------------------------------------------------------------------------

   No_Of_Elements : constant := 10_000;
   type Test_Type is (QUEUE_RIGHT, QUEUE_LEFT, STACK_RIGHT, STACK_LEFT,
                      ALL_TESTS);
   DEQUE_FIFO_PROPERTY_VIOLATION  : exception;
   DEQUE_STACK_PROPERTY_VIOLATION : exception;

   Test : constant Test_Type := ALL_TESTS;

   Output_File : Ada.Text_IO.File_Type renames
     Ada.Text_IO.Standard_Output;
--     Ada.Text_IO.Standard_Error;

   task type Left_Producer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Left_Producer;
   task type Right_Producer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Right_Producer;

   task type Right_Consumer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Right_Consumer;
   task type Left_Consumer is
      pragma Storage_Size (1 * 1024 * 1024);
   end Left_Consumer;


   Deque                      : aliased Deques.Deque_Type;

   Start                      : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Start);
   Right_Push_Count           : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Right_Push_Count);
   Left_Push_Count            : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Left_Push_Count);
   Right_Pop_Count            : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Right_Pop_Count);
   Left_Pop_Count             : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Left_Pop_Count);
   No_Producers_Running       : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Producers_Running);
   No_Consumers_Running       : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Consumers_Running);

   ----------------------------------------------------------------------------
   task body Right_Producer is
      No_Pushes : Primitives.Unsigned_32 := 0;
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
            Push_Right (Deque, Value_Type'(ID, I));
            No_Pushes := Primitives.Unsigned_32'Succ (No_Pushes);
         end loop;

      exception
         when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Right_Producer (" &
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
         Primitives.Fetch_And_Add_32 (Right_Push_Count'Access, No_Pushes);
         Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);
      end;
      Ada.Text_IO.Put_Line (Output_File,
                            "Right_Producer (?): exited.");

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Output_File);
         Ada.Text_IO.Put_Line (Output_File,
                               "Right_Producer (?): raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Output_File);
   end Right_Producer;

   ----------------------------------------------------------------------------
   task body Left_Producer is
      No_Pushes : Primitives.Unsigned_32 := 0;
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
         Primitives.Fetch_And_Add_32 (Left_Push_Count'Access, No_Pushes);
         Primitives.Fetch_And_Add_32 (No_Producers_Running'Access, -1);
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
               V       := Pop_Right (Deque'Access);
               No_Pops := Primitives.Unsigned_32'Succ (No_Pops);

               Done := False;

               case Test is
                  when QUEUE_LEFT | QUEUE_RIGHT =>
                     if V.Index <= Last (V.Creator) then
                        raise DEQUE_FIFO_PROPERTY_VIOLATION;
                     end if;
                     Last (V.Creator) := V.Index;

                  when ALL_TESTS | STACK_LEFT | STACK_RIGHT =>
                     null;

               end case;

            exception
               when Deques.Deque_Empty =>
--                  Ada.Text_IO.Put (".");
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
         Primitives.Fetch_And_Add_32 (Right_Pop_Count'Access, No_Pops);
         Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, -1);
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

   ----------------------------------------------------------------------------
   task body Left_Consumer is
      No_Pops : Primitives.Unsigned_32 := 0;
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
               V       := Pop_Left (Deque'Access);
               No_Pops := Primitives.Unsigned_32'Succ (No_Pops);

               Done := False;

               case Test is
                  when QUEUE_LEFT | QUEUE_RIGHT =>
                     if V.Index <= Last (V.Creator) then
                        raise DEQUE_FIFO_PROPERTY_VIOLATION;
                     end if;
                     Last (V.Creator) := V.Index;

                  when ALL_TESTS | STACK_LEFT | STACK_RIGHT =>
                     null;
               end case;

            exception
               when Deques.Deque_Empty =>
--                  Ada.Text_IO.Put (".");
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
                                  "Left_Consumer (" &
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
         Primitives.Fetch_And_Add_32 (Left_Pop_Count'Access, No_Pops);
         Primitives.Fetch_And_Add_32 (No_Consumers_Running'Access, -1);
      end;

      Ada.Text_IO.Put_Line (Output_File,
                            "Left_Consumer (?): exited.");
   exception
      when E : others =>
            Ada.Text_IO.New_Line (Output_File);
            Ada.Text_IO.Put_Line (Output_File,
                                  "Left_Consumer (?): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Output_File);
   end Left_Consumer;

   use type Ada.Real_Time.Time;
   T1, T2 : Ada.Real_Time.Time;
begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Init (Deque);
   Ada.Text_IO.Put_Line (" Deque ");

   case Test is

      -----------------------------------------------------------------
      when QUEUE_LEFT =>
         Ada.Text_IO.Put_Line
           ("Testing with right producer/left consumer tasks.");
         declare
            use type Primitives.Unsigned_32;
            P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
              : Right_Producer;
            C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
              : Left_Consumer;
         begin
            delay 5.0;
            T1 := Ada.Real_Time.Clock;
            Primitives.Fetch_And_Add_32 (Start'Access, 1);
         end;
         T2 := Ada.Real_Time.Clock;

      -----------------------------------------------------------------
      when QUEUE_RIGHT =>
         Ada.Text_IO.Put_Line
           ("Testing with left producer/right consumer tasks.");
         declare
            use type Primitives.Unsigned_32;
            P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
              : Left_Producer;
            C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
              : Right_Consumer;
         begin
            delay 5.0;
            T1 := Ada.Real_Time.Clock;
            Primitives.Fetch_And_Add_32 (Start'Access, 1);
         end;
         T2 := Ada.Real_Time.Clock;

      -----------------------------------------------------------------
      when STACK_LEFT =>
         Ada.Text_IO.Put_Line
           ("Testing with left producer/left consumer tasks.");
         declare
            use type Primitives.Unsigned_32;
            P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
              : Left_Producer;
            C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
              : Left_Consumer;
         begin
            delay 5.0;
            T1 := Ada.Real_Time.Clock;
            Primitives.Fetch_And_Add_32 (Start'Access, 1);
         end;
         T2 := Ada.Real_Time.Clock;

      -----------------------------------------------------------------
      when STACK_RIGHT =>
         Ada.Text_IO.Put_Line
           ("Testing with right producer/right consumer tasks.");
         declare
            use type Primitives.Unsigned_32;
            P0, P1, P2, P3, P4, P5, P6, P7, P8, P9, P10, P11, P12, P13, P14
              : Right_Producer;
            C0, C1, C2, C3, C4, C5, C6, C7, C8, C9, C10, C11, C12, C13, C14
              : Right_Consumer;
         begin
            delay 5.0;
            T1 := Ada.Real_Time.Clock;
            Primitives.Fetch_And_Add_32 (Start'Access, 1);
         end;
         T2 := Ada.Real_Time.Clock;

      -----------------------------------------------------------------
      when ALL_TESTS =>
         Ada.Text_IO.Put_Line
           ("Testing with right/left producer / right/left consumer tasks.");
         declare
            use type Primitives.Unsigned_32;
            RP0, RP1, RP2, RP3, RP4, RP5, RP6, RP7
              : Right_Producer;
            LP0, LP1, LP2, LP3, LP4, LP5, LP6, LP7
              : Left_Producer;
            RC0, RC1, RC2, RC3, RC4, RC5, RC6, RC7
              : Right_Consumer;
            LC0, LC1, LC2, LC3, LC4, LC5, LC6, LC7
              : Left_Consumer;
         begin
            delay 5.0;
            T1 := Ada.Real_Time.Clock;
            Primitives.Fetch_And_Add_32 (Start'Access, 1);
         end;
         T2 := Ada.Real_Time.Clock;

   end case;

--     Push_Left (Deque, Value_Type'(1, 10));
--     Push_Left (Deque, Value_Type'(1, 20));
--     Push_Left (Deque, Value_Type'(1, 30));
--     Push_Left (Deque, Value_Type'(1, 40));

   delay 1.0;
   Ada.Text_IO.Put_Line ("Right_Push count: " &
                         Primitives.Unsigned_32'Image (Right_Push_Count));
   Ada.Text_IO.Put_Line ("Left_Push count: " &
                         Primitives.Unsigned_32'Image (Left_Push_Count));
   Ada.Text_IO.Put_Line ("Right_Pop count: " &
                         Primitives.Unsigned_32'Image (Right_Pop_Count));
   Ada.Text_IO.Put_Line ("Left_Pop count: " &
                         Primitives.Unsigned_32'Image (Left_Pop_Count));
   Ada.Text_IO.Put_Line ("Elapsed time:" &
                         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)) &
                         " seconds.");

   Ada.Text_IO.Put_Line ("Verifying deque.");
   Verify (Deque, Print => False);

   Ada.Text_IO.Put_Line ("Emptying deque.");
   delay 5.0;

   declare
      use type Primitives.Unsigned_32;
      V : Value_Type;
   begin
      loop
         V := Pop_Left (Deque'Access);
         Ada.Text_IO.Put_Line (Output_File,
                               "Pop_Left() = (" &
                               PID.Process_ID_Type'Image (V.Creator) & ", " &
                               Integer'Image (V.Index) & ")");
         Primitives.Fetch_And_Add_32 (Left_Pop_Count'Access, 1);
         Ada.Text_IO.Put_Line ("Verifying deque.");
         Verify (Deque, Print => False);
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
                               (Right_Push_Count + Left_Push_Count));
         Ada.Text_IO.Put_Line ("Final pop count: " &
                               Primitives.Unsigned_32'Image
                               (Right_Pop_Count + Left_Pop_Count));

         Ada.Text_IO.Put_Line ("Verifying deque.");
         Verify (Deque, Print => True);
   end;
end Deque_Test;
