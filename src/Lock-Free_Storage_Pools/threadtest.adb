-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : threadtest.adb
--  Description     : Ada translation of the Hoard benchmark.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jun 27 22:25:04 2003
--  $Id: threadtest.adb,v 1.2 2005/06/20 16:43:12 anders Exp $
-------------------------------------------------------------------------------

with Ada.Real_Time;
with Ada.Unchecked_Deallocation;
with Ada.Exceptions;
with Ada.Command_Line;
with Ada.Text_IO;

--  with Lock_Free_Fixed_Size_Storage_Pools;
with NBmalloc_Storage_Pools;

procedure Threadtest is

   Version : constant String :=
     "$Id: threadtest.adb,v 1.2 2005/06/20 16:43:12 anders Exp $";

   No_Iterations : Natural := 500;
   No_Objects    : Natural := 30_000;
   No_Threads    : Natural := 1;
   Work          : Natural := 100;

   task type Worker;
   type Worker_Array is array (Positive range <>) of Worker;

   type Int_Array is array (Positive range <>) of Integer;

   type Foo (Z : Natural) is
      record
         X : Integer := 14;
         Y : Integer := 29;
         W : Int_Array (1 .. 4) := (others => 7);
      end record;

   Pool_Size : constant := 2**15;
   My_Pool :
--       Lock_Free_Fixed_Size_Storage_Pools.Lock_Free_Storage_Pool
--       (Pool_Size  => Pool_Size,
--        Block_Size => Foo'Max_Size_In_Storage_Elements);
     NBmalloc_Storage_Pools.Lock_Free_Storage_Pool;

   type Foo_Access is access Foo;
   --for Foo_Access'Storage_Pool use My_Pool;

   type Foo_Array is array (Positive range <>) of Foo_Access;

   procedure Free is new Ada.Unchecked_Deallocation (Foo, Foo_Access);

   task body Worker is
      A : Foo_Array (1 .. No_Iterations / No_Threads);
      type Size_Type is mod 13*17;
      S : Size_Type := 4;
   begin
      for J in 1 .. No_Iterations loop

         for I in A'Range loop
            A (I) := new Foo (Integer (S));
            for W in 1 .. Work loop
               S := S + 1;
               S := S * S;
               S := S * S;
               S := S * S;
               S := S * S;
            end loop;
            if A (I) = null or else
              (A (I).X /= 14 or A (I).Y /= 29)
            then
               Ada.Text_IO.Put_Line ("Worker: Implementation error!");
               raise Constraint_Error;
            end if;
         end loop;

         for I in A'Range loop
            Free (A (I));
            for W in 1 .. Work loop
               S := S + S;
               S := S * S;
               S := S + S + 1;
               S := S * S;
            end loop;
            if A (I) /= null then
               Ada.Text_IO.Put_Line ("Worker: Implementation error!");
               raise Constraint_Error;
            end if;
         end loop;

      end loop;

   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Worker: Exception!");
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Worker;

   use Ada.Real_Time;

   T1, T2 : Ada.Real_Time.Time;
begin
   --  Read command line.
   if Ada.Command_Line.Argument_Count > 1 then
      Ada.Text_IO.Put_Line ("Usage: " &
                            Ada.Command_Line.Command_Name &
                            " #threads");
      raise Constraint_Error;
   elsif Ada.Command_Line.Argument_Count = 0 then
      --  Use defaults.
      null;
   else
      No_Threads := Natural'Value (Ada.Command_Line.Argument (1));
   end if;

   Ada.Text_IO.Put_Line ("Running threadtest (" & Version & "): " &
                         "#iterations = " & Integer'Image (No_Iterations) &
                         ", #objects = " & Integer'Image (No_Objects) &
                         ", #threads = " & Integer'Image (No_Threads) &
                         ", #work = " & Integer'Image (Work));
   --   Ada.Text_IO.Put_Line (Lock_Free_Storage_Pools.Image (My_Pool));

   T1 := Ada.Real_Time.Clock;
   declare
      Workers : Worker_Array (1 .. No_Threads);
   begin
      null;
   end;
   T2 := Ada.Real_Time.Clock;
--   Ada.Text_IO.Put_Line (Lock_Free_Storage_Pools.Image (My_Pool));
   Ada.Text_IO.Put_Line ("Threadtest result: " &
                         Duration'Image (To_Duration (T2 - T1)) & " seconds." &
                         "");
end Threadtest;

