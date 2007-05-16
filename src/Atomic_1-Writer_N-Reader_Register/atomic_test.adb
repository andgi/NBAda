--                              -*- Mode: Ada -*-
-- Filename        : atomic_test.adb
-- Description     : Test of wait-free constructions.
-- Author          : Anders Gidenstam
-- Created On      : Sat Oct 20 00:43:30 2001
-- $Id: atomic_test.adb,v 1.3 2007/05/16 14:42:26 andersg Exp $

with Ada.Text_IO; use Ada.Text_IO;

with Atomic_Single_Writer_Registers;

procedure Atomic_Test is

   type My_String is new String (1..40);

   package Wait_Free_Strings is new Atomic_Single_Writer_Registers (My_String);
   use Wait_Free_Strings;

   task Writer;
   task type Reader (No : Positive);

   Reg : Atomic_1_M_Register (No_Of_Readers => 10);

   R1  : Reader (1);
   R2  : Reader (2);
   R3  : Reader (3);
   R4  : Reader (4);
   R5  : Reader (5);
   R6  : Reader (6);
   R7  : Reader (7);
   R8  : Reader (8);
   R9  : Reader (9);
   R10 : Reader (10);

   Str1 : constant My_String := "Hej din gamla hoppräka!                 ";
   Str2 : constant My_String := "Sju sjösjuka sjömän från Shanghai..     ";

   task body Writer is
   begin
      loop
         Write (Reg, Str1);
         delay 0.0;
         Write (Reg, Str2);
         delay 0.0;
      end loop;
   end Writer;

   task body Reader is
      Str : My_String;
   begin
      delay 1.0;
      loop
         Read (Reg, No, Str);
         if Str /= Str1 and Str /= Str2 then
            Put_Line (Integer'Image (No) & ": " & String (Str));
         end if;
         delay 0.0;
      end loop;
   end Reader;

begin
   null;
end Atomic_Test;
