-------------------------------------------------------------------------------
--  Large Primitives - An implementation of Maged Michael's LL/SC primitives.
--  Copyright (C) 2005  Anders Gidenstam
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
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : large_primitives_test.adb
--  Description     : Test of large word LL/SC/VL and hazard pointers.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 24 15:00:10 2005
--  $Id: large_primitives_test.adb,v 1.3 2005/06/09 14:53:17 anders Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Large_Primitives;
with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

procedure Large_Primitives_Test is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 50);

   package LL_SC is new Large_Primitives (Max_Number_Of_Links => 2,
                                          Process_Ids         => PID);

   type My_String is new String (1 .. 10);

   package My_LL_SC is
      new LL_SC.Load_Linked_Store_Conditional (Element => My_String);

   use My_LL_SC;

   task type Tester;
   task type Loader;

   --  Shared data.
   A : aliased Shared_Element;

   --  Shared statistics.
   Loaded       : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Loaded);
   SC_Succeeded : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (SC_Succeeded);
   SC_Failed    : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (SC_Failed);

   task body Tester is
   begin
      PID.Register;
      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
         Tmp : My_String;
         B   : Boolean;
         C   : Primitives.Unsigned_32 := 0;
         use type Primitives.Unsigned_32;
      begin
         for I in 1 .. 10_000 loop
--              Ada.Text_IO.Put_Line (PID.Process_ID_Type'Image (ID) &
--                                    ": LL(A) -> " &
--                                    String (Load_Linked (A'Access)));
            Tmp := Load_Linked (A'Access);
            Primitives.Fetch_And_Add (Loaded'Access, 1);

            --  Work.
            for J in 1 .. 1_000 loop
               C := C + Primitives.Unsigned_32 (J);
            end loop;

--              Ada.Text_IO.Put_Line
--                (PID.Process_ID_Type'Image (ID) &
--                 ": SC(A, ...): " &
--                 Boolean'Image
--                 (Store_Conditional (A'Access, "CepaDepa  ")));
            B := Store_Conditional (A'Access, "CepaDepa  ");
            if B then
               Primitives.Fetch_And_Add (SC_Succeeded'Access, 1);
            else
               Primitives.Fetch_And_Add (SC_Failed'Access, 1);
            end if;

         end loop;
      exception
         when E : others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  PID.Process_ID_Type'Image (ID) &
                                  ": raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;
   end Tester;

   task body Loader is
   begin
      PID.Register;
      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
         Tmp : My_String;
--         B   : Boolean;
      begin
         for I in 1 .. 100_000 loop
            Tmp := Load_Linked (A'Access);
            Primitives.Fetch_And_Add (Loaded'Access, 1);

--            B := Store_Conditional (A'Access, "CepaDepa  ");
         end loop;
      exception
         when E : others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  PID.Process_ID_Type'Image (ID) &
                                  ": raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;
   end Loader;

begin
   PID.Register;

   My_LL_SC.Initialize (A'Access, "ApaBepa   ");

   Ada.Text_IO.Put_Line (String (Load_Linked (A'Access)));

   Ada.Text_IO.Put_Line ("SC(A): " &
                         Boolean'Image
                         (Store_Conditional (A'Access, "CepaDepa  ")));

   Ada.Text_IO.Put_Line (String (Load_Linked (A'Access)));

   declare
      Tasks1 : array (1 .. 25) of Tester;
--      Tasks2 : array (1 .. 25) of Loader;
   begin
      null;
   end;

   LL_SC.Print_Statistics;
   LL_SC.HP.Print_Statistics;

   Ada.Text_IO.Put_Line ("Large_Primitives_Test:");
   Ada.Text_IO.Put_Line ("  #Loaded = " &
                         Primitives.Unsigned_32'Image (Loaded));
   Ada.Text_IO.Put_Line ("  #SC_Succeeded = " &
                         Primitives.Unsigned_32'Image (SC_Succeeded));
   Ada.Text_IO.Put_Line ("  #SC_Failed = " &
                         Primitives.Unsigned_32'Image (SC_Failed));

end Large_Primitives_Test;
