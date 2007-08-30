-------------------------------------------------------------------------------
--  Fetch and Add test.
--  Copyright (C) 2004 - 2006  Anders Gidenstam
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : faa_test.adb
--  Description     : Test of synchronization primitives package.
--  Author          : Anders Gidenstam
--  Created On      : Tue Jul  9 14:07:11 2002
--  $Id: faa_test.adb,v 1.9 2007/08/30 13:36:15 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Primitives;
with Ada.Text_IO;
with Ada.Exceptions;

with System.Task_Info;

procedure FAA_Test is

   Count : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Count);
   Count_CAS : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Count_CAS);

   function System_Scope_Task return System.Task_Info.Task_Info_Type;

   task type Counter is
      pragma Task_Info (System_Scope_Task);
   end Counter;

--   function CAS is
--      new Primitives.Boolean_Compare_And_Swap_32 (Primitives.Unsigned_32);

   ----------------------------------------------------------------------------
   function System_Scope_Task return System.Task_Info.Task_Info_Type is
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
--  GNAT/Solaris
--      return System.Task_Info.New_Bound_Thread_Attributes;
   end System_Scope_Task;

   task body Counter is
   begin
      for I in 1 .. 10_000_000 loop
         Primitives.Fetch_And_Add (Target    => Count'Access,
                                   Increment => 1);
--           loop
--              declare
--                 use type Primitives.Unsigned_32;
--                 T : Primitives.Unsigned_32 := Count_CAS;
--              begin
--                 exit when CAS (Count_CAS'Access, T, T + 1);
--              end;
--           end loop;
      end loop;
      Ada.Text_IO.Put_Line ("Count: " &
                            Primitives.Standard_Unsigned'Image (Count) &
                            "  Count_CAS: " &
                            Primitives.Standard_Unsigned'Image (Count_CAS));
   exception
      when E : others =>
         Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Information (E));
   end Counter;

   type Counter_Access is access Counter;
   type Counter_Array is array (Positive range <>) of Counter_Access;

   Counters : Counter_Array (1 .. 10);
begin
   declare
      use type Primitives.Standard_Unsigned;
      Test : aliased Primitives.Standard_Unsigned := 0;
   begin
      Ada.Text_IO.Put_Line ("Test 1: 10 x FAA(Test'Access, 2)." &
                            "Expected outcome: 0, 2, 4, .. , 18.");
      for I in 1 .. 10 loop
         Ada.Text_IO.Put_Line
           ("FAA(Test'Access, 2):" &
            Primitives.Standard_Unsigned'Image
            (Primitives.Fetch_And_Add (Target    => Test'Access,
                                       Increment => 2)));
      end loop;
      if Test /= 20 then
         Ada.Text_IO.Put_Line ("Test 1: Failed! Final value is incorrect.");
      end if;
   end;

   Ada.Text_IO.Put_Line ("Test 2: 10 concurrent tasks count to " &
                         "10_000_000 each. Expected final outcome: " &
                         "100_000_000.");

   --  Start tasks;
   for I in Counters'Range loop
      Counters (I) := new Counter;
   end loop;
end FAA_Test;
