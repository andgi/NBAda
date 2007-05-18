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
--  Filename        : wait_free.adb
--  Description     : Ada implementation of atomic multi-word register.
--                    Based on G. Peterson, "Concurrent Reading While Writing",
--                    ACM Transactions on Programming Languages and Systems,
--                    1983.
--  Author          : Anders Gidenstam
--  Created On      : Sat Oct 20 00:04:58 2001
--  $Id: nbada-atomic_single_writer_registers.adb,v 1.5 2007/05/18 13:46:15 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

package body Atomic_Single_Writer_Registers is

   procedure Write (Register : in out Atomic_1_M_Register;
                    Value    : in     Element_Type) is
   begin
      Register.Wflag  := True;
      Register.Buff1  := Value;
      Register.Switch := not Register.Switch;
      Register.Wflag  := False;
      for J in Register.Reading'Range loop
         if Register.Reading (J) /= Register.Writing (J) then
            Register.Copybuff (J) := Value;
            Register.Writing  (J) := Register.Reading (J);
         end if;
      end loop;
      Register.Buff2  := Value;
   end Write;

   procedure Read  (Register  : in out Atomic_1_M_Register;
                    Reader_No : in     Positive;
                    Value     :    out Element_Type) is
      Sflag       : Boolean;
      Sflag2      : Boolean;
      Sswitch     : Boolean;
      Sswitch2    : Boolean;
      Buff1_Value : Element_Type;
      Buff2_Value : Element_Type;
   begin
      Register.Reading (Reader_No) := not Register.Writing (Reader_No);
      Sflag       := Register.Wflag;
      Sswitch     := Register.Switch;
      Buff1_Value := Register.Buff1;
      Sflag2      := Register.Wflag;
      Sswitch2    := Register.Switch;
      Buff2_Value := Register.Buff2;
      if Register.Reading (Reader_No) = Register.Writing (Reader_No) then
         Value    := Register.Copybuff (Reader_No);
      elsif Sswitch /= Sswitch2 or Sflag or Sflag2 then
         Value    := Buff2_Value;
      else
         Value    := Buff1_Value;
      end if;
   end Read;

end Atomic_Single_Writer_Registers;

