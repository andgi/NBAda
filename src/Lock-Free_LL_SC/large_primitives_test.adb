-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : large_primitives_test.adb
--  Description     : Test of large word LL/SC/VL and hazard pointers.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 24 15:00:10 2005
--  $Id: large_primitives_test.adb,v 1.1 2005/02/24 16:05:42 anders Exp $
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
            for J in 1 .. 50_000 loop
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
         when E: others =>
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
         B   : Boolean;
      begin
         for I in 1 .. 100_000 loop
            Tmp := Load_Linked (A'Access);
            Primitives.Fetch_And_Add (Loaded'Access, 1);

            --B := Store_Conditional (A'Access, "CepaDepa  ");
         end loop;
      exception
         when E: others =>
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
      Tasks1 : array (1 .. 2) of Tester;
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
