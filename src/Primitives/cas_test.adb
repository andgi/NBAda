-------------------------------------------------------------------------------
--  Compare and Swap test.
--  Copyright (C) 2004 - 2007  Anders Gidenstam
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
--  Filename        : cas_test.adb
--  Description     : Test of synchronization primitives package.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jul  5 16:09:25 2002
--  $Id: cas_test.adb,v 1.10 2007/08/30 14:11:43 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Text_IO;

with NBAda.Primitives;

with Ada.Exceptions;
with Ada.Unchecked_Conversion;

procedure CAS_Test is

   generic
      type Element_Type is limited private;
   function Address_Image (Element : access Element_Type) return String;

   function Address_Image (Element : access Element_Type) return String is
      type Element_Access is access all Element_Type;
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element_Access,
                                       NBAda.Primitives.Standard_Unsigned);
   begin
      return NBAda.Primitives.Standard_Unsigned'Image
        (To_Unsigned (Element_Access (Element)));
   end Address_Image;

begin

   ----------------------------------------------------------------------------
   --  Compare_And_Swap_32

   declare
      procedure CAS is new
        NBAda.Primitives.Compare_And_Swap_32 (Element => Integer);
      Operation : constant String := "Compare_And_Swap_32";
      function Image is new Address_Image (Integer);

      A : aliased Integer := 77;
   begin

      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("Integer'Object_Size: " &
                            Integer'Image (Integer'Object_Size));
      Ada.Text_IO.Put_Line ("A'access: " & Image (A'Access));

      --  Test 1.
      declare
         B : Integer := 77;
         C : Integer := 2;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));


         Ada.Text_IO.Put_Line (Operation & " (A, B, C)");
         CAS (A'Access,
              Old_Value => B,
              New_Value => C);
         if A = 2 and C = 77 and B = 77 then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
         end if;

      end;

      --  Test 2.
      declare
         B : Integer := 77;
         C : Integer := 8;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
         Ada.Text_IO.Put_Line (Operation & " (A, 3, C)");
         CAS (A'Access,
              Old_Value => 3,
              New_Value => C);
         if A = 2 and C = 2 then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
         end if;
      end;
   end;

   ----------------------------------------------------------------------------
   --  Boolean_Compare_And_Swap
   declare
      function  CAS is new
        NBAda.Primitives.Boolean_Compare_And_Swap_32 (Element => Integer);
      Operation : constant String := "Boolean_Compare_And_Swap_32";
      function Image is new Address_Image (Integer);

      A : aliased Integer := 2;
   begin

      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("Integer'Object_Size: " &
                            Integer'Image (Integer'Object_Size));
      Ada.Text_IO.Put_Line ("A'access: " & Image (A'Access));

      --  Test 1.
      declare
         B : Integer := 77;
         C : Integer := 5;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));

         Ada.Text_IO.Put_Line ("CAS (A, 2, C)");
         if (CAS (A'Access,
                  Old_Value => 2,
                  New_Value => C)) and then A = 5 then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
      end;

      --  Test 2.
      declare
         B : Integer := 77;
         C : Integer := 5;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));

         Ada.Text_IO.Put_Line ("CAS (A, 2, C)");
         if (CAS (A'Access,
                  Old_Value => 2,
                  New_Value => C)) or else A /= 5 then
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
         else
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
      end;
   end;

   ----------------------------------------------------------------------------
   --  Void_Compare_And_Swap_32

   declare
      procedure CAS is new
        NBAda.Primitives.Void_Compare_And_Swap_32 (Element => Integer);
      Operation : constant String := "Void_Compare_And_Swap_32";
      function Image is new Address_Image (Integer);

      A : aliased Integer := 77;
   begin

      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("Integer'Object_Size: " &
                            Integer'Image (Integer'Object_Size));
      Ada.Text_IO.Put_Line ("A'access: " & Image (A'Access));

      --  Test 1.
      declare
         B : Integer := 77;
         C : Integer := 2;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));


         Ada.Text_IO.Put_Line (Operation & " (A, B, C)");
         CAS (A'Access,
              Old_Value => B,
              New_Value => C);
         if A = 2 and C = 2 and B = 77 then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
      end;

      --  Test 2.
      declare
         B : Integer := 77;
         C : Integer := 8;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
         Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
         Ada.Text_IO.Put_Line (Operation & " (A, 3, C)");
         CAS (A'Access,
              Old_Value => 3,
              New_Value => C);
         if A = 2 and C = 8 then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("A: " & Integer'Image (A));
            Ada.Text_IO.Put_Line ("B: " & Integer'Image (B));
            Ada.Text_IO.Put_Line ("C: " & Integer'Image (C));
      end;
   end;

   ----------------------------------------------------------------------------
   --  Compare_And_Swap_64

   declare
      type My_Float is new Long_Float;
      pragma Atomic (My_Float);
      procedure CAS is new
        NBAda.Primitives.Compare_And_Swap_64 (Element => My_Float);
      Operation : constant String := "Compare_And_Swap_64";
      function Image is new Address_Image (My_Float);

      LA : aliased My_Float := -2.0;
   begin
      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("My_Float'Object_Size: " &
                            Natural'Image (My_Float'Object_Size));
      Ada.Text_IO.Put_Line ("LA'access: " &
                            Image (LA'Access));

      --  Test 1.
      declare
         LB        : My_Float := -2.0;
         LC        : My_Float := 7.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         CAS (LA'Access, LB, LC);

         if LA = 7.0  and LC = LA_Before then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;

      --  Test 2.
      declare
         LB        : My_Float := -3.0;
         LC        : My_Float := 2.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         CAS (LA'Access, LB, LC);
         if LA = LA_Before and LC = LA_Before then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;
   end;

   ----------------------------------------------------------------------------
   --  Boolean_Compare_And_Swap_64

   declare
      type My_Float is new Long_Float;
      pragma Atomic (My_Float);
      function CAS is new
        NBAda.Primitives.Boolean_Compare_And_Swap_64 (Element => My_Float);
      Operation : constant String := "Boolean_Compare_And_Swap_64";
      function Image is new Address_Image (My_Float);

      LA : aliased My_Float := -45.0;
   begin
      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("My_Float'Object_Size: " &
                            Natural'Image (My_Float'Object_Size));
      Ada.Text_IO.Put_Line ("LA'access: " &
                            Image (LA'Access));

      --  Test 1.
      declare
         LB : My_Float := -45.0;
         LC : My_Float := 5.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         if CAS (LA'Access, LB, LC) and then LA = LC then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;

      --  Test 2.
      declare
         LB : My_Float := -2.0;
         LC : My_Float := 8.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         if
           CAS (LA'Access,
                Old_Value => LB,
                New_Value => LC)
           or else LA /= LA_Before
         then
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         else
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;
   end;

   ----------------------------------------------------------------------------
   --  Void_Compare_And_Swap_64

   declare
      type My_Float is new Long_Float;
      pragma Atomic (My_Float);
      procedure CAS is new
        NBAda.Primitives.Void_Compare_And_Swap_64 (Element => My_Float);
      Operation : constant String := "Void_Compare_And_Swap_64";
      function Image is new Address_Image (My_Float);

      LA : aliased My_Float := -2.0;
   begin
      Ada.Text_IO.Put_Line ("-----------------------------------------------");
      Ada.Text_IO.Put_Line ("My_Float'Object_Size: " &
                            Natural'Image (My_Float'Object_Size));
      Ada.Text_IO.Put_Line ("LA'access: " &
                            Image (LA'Access));

      --  Test 1.
      declare
         LB        : My_Float := -2.0;
         LC        : My_Float := 7.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         CAS (LA'Access, LB, LC);

         if LA = 7.0  then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 1.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;

      --  Test 2.
      declare
         LB        : My_Float := -3.0;
         LC        : My_Float := 2.0;
         LA_Before : constant  My_Float := LA;
      begin
         Ada.Text_IO.Put_Line ("----------------------");
         Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
         Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
         Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));

         Ada.Text_IO.Put_Line (Operation & " (LA, LB, LC)");
         CAS (LA'Access, LB, LC);
         if LA = LA_Before then
            Ada.Text_IO.Put_Line (Operation & ": Passed test 2.");
         else
            Ada.Text_IO.Put_Line (Operation & " is faulty!");
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
            Ada.Text_IO.Put_Line ("LC: " & My_Float'Image (LC));
         end if;

      exception
         when E : others =>
            Ada.Text_IO.Put_Line (Operation & " is faulty! (exception)");
            Ada.Text_IO.Put_Line (Ada.Exceptions.Exception_Name (E) & ": " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.Put_Line ("LA: " & My_Float'Image (LA));
            Ada.Text_IO.Put_Line ("LB: " & My_Float'Image (LB));
      end;
   end;

end CAS_Test;
