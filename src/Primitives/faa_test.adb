-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : faa_test.adb
-- Description     : Test of synchronization primitives package.
-- Author          : Anders Gidenstam
-- Created On      : Tue Jul  9 14:07:11 2002
-- $Id: faa_test.adb,v 1.1 2004/09/21 11:59:38 anders Exp $
-------------------------------------------------------------------------------

with Primitives;
with Ada.Text_IO;
with Ada.Exceptions;

procedure FAA_Test is

   Count : aliased Primitives.Unsigned_32 := 0;
   pragma Volatile (Count);

   task type Counter (Count : access Primitives.Unsigned_32);

   task body Counter is
      Old : Primitives.Unsigned_32;
   begin
      for I in 1 .. 10_000_000 loop
         Primitives.Fetch_And_Add (Target    => Count,
                                   Increment => 1);
      end loop;
      Ada.Text_IO.Put_Line ("Count: " &
                            Primitives.Unsigned_32'Image (Count.all));
   exception
      when E: others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information(E));
   end Counter;

   type Counter_Access is access Counter;
   type Counter_Array is array (Positive range <>) of Counter_Access;

   Counters : Counter_Array (1 .. 10);
begin
   declare
      Test : aliased Primitives.Unsigned_32 := 0;
   begin
      for I in 1 .. 10 loop
         Ada.Text_IO.Put_Line
           (Primitives.Unsigned_32'Image
            (Primitives.Fetch_And_Add (Target    => Test'Access,
                                       Increment => 2)));
      end loop;
   end;

   -- Start tasks;
   for I in Counters'Range loop
      Counters (I) := new Counter (Count'Access);
   end loop;
end FAA_Test;
