-------------------------------------------------------------------------------
--  Ada implementation of atomic multi-word register based on the algorithm
--  by G. Peterson.
--  Copyright (C) 2001 - 2007  Anders Gidenstam
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
--  Filename        : atomic_test.adb
--  Description     : Test of wait-free register constructions.
--  Author          : Anders Gidenstam
--  Created On      : Sat Oct 20 00:43:30 2001
--  $Id: atomic_test.adb,v 1.4 2007/05/18 13:46:15 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Text_IO; use Ada.Text_IO;

with Atomic_Single_Writer_Registers;

procedure Atomic_Test is

   type My_String is new String (1 .. 40);

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
   Ada.Text_IO.Put_Line ("NOTE: Unless there are severe errors detected " &
                         "this program runs forever.");
end Atomic_Test;
