-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : cas_test.adb
-- Description     : Test of synchronization primitives package.
-- Author          : Anders Gidenstam
-- Created On      : Fri Jul  5 16:09:25 2002
-- $Id: cas_test.adb,v 1.3 2004/09/21 11:56:56 anders Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;

with Primitives;

with Ada.Unchecked_Conversion;

procedure CAS_Test is

   procedure CAS is new
     Primitives.Compare_And_Swap_32 (Element => Integer);
   function  CAS is new
     Primitives.Boolean_Compare_And_Swap_32 (Element => Integer);

   type My_Float is new Long_Float;
   --pragma Atomic (My_Float);
   procedure CAS is new
     Primitives.Compare_And_Swap_64 (Element => My_Float);


   type Int_Access is access all Integer;
   function To_Int is new Ada.Unchecked_Conversion (Int_Access,
                                                    Integer);

   A : aliased Integer := 77;
   B : Integer := 77;
   C : Integer := 2;

begin
   Ada.Text_IO.Put_Line ("Integer'Object_Size: " &
                         Integer'Image (Integer'Object_Size));
   Ada.Text_IO.Put_Line ("A'access: " & Integer'Image (To_Int (A'Access)));
   Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
   Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
   Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));

   --  Test 1.
   Ada.Text_IO.Put_Line ("CAS (A, B, C)");
   CAS (A'Access,
        Old_Value => B,
        New_Value => C);
   if A = 2 and C = 77 then
      Ada.Text_IO.Put_Line ("Compare_And_Swap_32: Passed test 1.");
   else
      Ada.Text_IO.Put_Line ("Compare_And_Swap_32 is faulty!");
      Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
      Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
      Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
   end if;

   Ada.Text_IO.Put_Line ("----------------------");
   -- Test 2.
   C := 8;
   Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
   Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
   Ada.Text_IO.Put_Line ("CAS (A, 3, C)");
   CAS (A'Access,
        Old_Value => 3,
        New_Value => C);
   if A = 2 and C = 2 then
      Ada.Text_IO.Put_Line ("Compare_And_Swap_32: Passed test 2.");
   else
      Ada.Text_IO.Put_Line ("Compare_And_Swap_32 is faulty!");
   end if;

   Ada.Text_IO.Put_Line ("----------------------");
   -- Test 3.
   C := 5;
   Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
   Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
   Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));

   Ada.Text_IO.Put_Line ("CAS (A, 2, B)");
   if (CAS (A'Access,
            Old_Value => 2,
            New_Value => C)) and then A = 5 then
      Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_32: Passed test 3.");
   else
      Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_32 is faulty!");
   end if;

   Ada.Text_IO.Put_Line ("----------------------");
   -- Test 4.
   Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
   Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
   Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));

   Ada.Text_IO.Put_Line ("CAS (A, 2, B)");
   if (CAS (A'Access,
            Old_Value => 2,
            New_Value => C)) or else A /= 5 then
      Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_32 is faulty!");
   else
      Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_32: Passed test 4.");
   end if;

   Ada.Text_IO.Put_Line ("----------------------");
   -- Test 5.
   declare
      LA : aliased My_Float := -2.0;
      LB : My_Float := 2.0;
   begin
      Ada.Text_IO.Put_Line ("My_Float'Object_Size: " &
                            Natural'Image (My_Float'Object_Size));
      CAS (LA'Access, -2.0, LB);
      Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
   end;

end CAS_Test;
