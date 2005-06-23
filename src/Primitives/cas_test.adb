-------------------------------------------------------------------------------
--  Compare and Swap test.
--  Copyright (C) 2004 - 2005  Anders Gidenstam
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
--  Filename        : cas_test.adb
--  Description     : Test of synchronization primitives package.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jul  5 16:09:25 2002
--  $Id: cas_test.adb,v 1.7 2005/06/23 13:47:59 anders Exp $
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
   pragma Atomic (My_Float);
   procedure CAS is new
     Primitives.Compare_And_Swap_64 (Element => My_Float);
   function CAS is new
     Primitives.Boolean_Compare_And_Swap_64 (Element => My_Float);
   procedure VCAS is new
     Primitives.Void_Compare_And_Swap_64 (Element => My_Float);


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
   --  Test 2.
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
   --  Test 3.
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
   --  Test 4.
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
   --  Test 5.
   declare
      LA : aliased My_Float := -2.0;
      LB : My_Float := 2.0;
   begin
      Ada.Text_IO.Put_Line ("My_Float'Object_Size: " &
                            Natural'Image (My_Float'Object_Size));
      Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
      Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));

      Ada.Text_IO.Put_Line ("CAS (LA, -2.0, LB)");
      CAS (LA'Access, -2.0, LB);
      if LB = -2.0 and LA = 2.0 then
         Ada.Text_IO.Put_Line ("Compare_And_Swap_64: Passed test 5.");
      else
         Ada.Text_IO.Put_Line ("Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
   end;

   Ada.Text_IO.Put_Line ("----------------------");
   --  Test 6.
   declare
      LA : aliased My_Float := -2.0;
      LB : My_Float := -2.0;
      LC : My_Float := 2.0;
   begin
      Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
      Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

      Ada.Text_IO.Put_Line ("CAS (LA, LB, LC)");
      if CAS (LA'Access, LB, LC) and then LA = LC then
         Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_64: Passed test 6.");
      else
         Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Boolean_Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
   end;

   Ada.Text_IO.Put_Line ("----------------------");
   --  Test 7.
   declare
      LA : aliased My_Float := -2.0;
      LB : My_Float := LA;
      LC : My_Float := 2.0;
   begin
      Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
      Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

      Ada.Text_IO.Put_Line ("VCAS (LA, LB, LC)");
      VCAS (LA'Access, LB, LC);
      if LA = 2.0 then
         Ada.Text_IO.Put_Line ("Void_Compare_And_Swap_64: Passed test 7.");
      else
         Ada.Text_IO.Put_Line ("Void_Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
      end if;

   exception
      when others =>
         Ada.Text_IO.Put_Line ("Void_Compare_And_Swap_64 is faulty!");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
   end;

end CAS_Test;
