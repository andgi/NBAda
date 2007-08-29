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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : atomic_single_writer_registers.ads
--  Description     : Ada implementation of atomic multi-word register.
--                    Based on G. Peterson, "Concurrent Reading While Writing",
--                    ACM Transactions on Programming Languages and Systems,
--                    1983.
--  Author          : Anders Gidenstam
--  Created On      : Fri Oct 19 23:57:47 2001
--  $Id: nbada-atomic_single_writer_registers.ads,v 1.5 2007/08/29 14:25:09 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

generic
   type Element_Type is private;

package Atomic_Single_Writer_Registers is

   pragma Pure;

   type Atomic_1_M_Register (No_Of_Readers : Positive) is limited private;

   procedure Write (Register : in out Atomic_1_M_Register;
                    Value    : in     Element_Type);
   procedure Read  (Register  : in out Atomic_1_M_Register;
                    Reader_No : in     Positive;
                    Value     :    out Element_Type);

private

   type Boolean_Array is array (Positive range <>) of Boolean;
   pragma Atomic_Components (Boolean_Array);
   type Element_Array is array (Positive range <>) of Element_Type;
   pragma Volatile_Components (Element_Array);

   type Atomic_1_M_Register (No_Of_Readers : Positive) is
      record
         Reading  : Boolean_Array (1 .. No_Of_Readers) := (others => False);
         Writing  : Boolean_Array (1 .. No_Of_Readers) := (others => False);
         Wflag    : Boolean := False;
         pragma Atomic (Wflag);
         Switch   : Boolean := False;
         pragma Atomic (Switch);
         Buff1    : Element_Type;
         pragma Volatile (Buff1);
         Buff2    : Element_Type;
         pragma Volatile (Buff2);
         Copybuff : Element_Array (1 .. No_Of_Readers);
         pragma Volatile (Copybuff);
      end record;

end Atomic_Single_Writer_Registers;

