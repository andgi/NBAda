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
--  $Id: nbada-atomic_single_writer_registers.adb,v 1.8 2007/10/24 14:22:44 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Primitives;

with Ada.Exceptions;

package body NBAda.Atomic_Single_Writer_Registers is

   function CAS is
      new NBAda.Primitives.Standard_Boolean_Compare_And_Swap (Natural);

   ----------------------------------------------------------------------------
   procedure Write (Register : in out Atomic_1_M_Register;
                    Value    : in     Element_Type) is
      procedure MB renames NBAda.Primitives.Membar;
   begin
      MB;
      Register.Wflag  := True;
      MB;
      Register.Buff1  := Value;
      MB;
      Register.Switch := not Register.Switch;
      MB;
      Register.Wflag  := False;
      MB;
      for J in Register.Reading'Range loop
         if Register.Reading (J) /= Register.Writing (J) then
            MB;
            Register.Copybuff (J) := Value;
            MB;
            Register.Writing  (J) := Register.Reading (J);
            MB;
         end if;
      end loop;
      Register.Buff2  := Value;
      MB;
   end Write;

   ----------------------------------------------------------------------------
   procedure Read  (Register : in out Atomic_1_M_Register;
                    Reader   : in     Reader_Id;
                    Value    :    out Element_Type) is
      procedure MB renames NBAda.Primitives.Membar;
      Sflag       : Boolean;
      Sflag2      : Boolean;
      Sswitch     : Boolean;
      Sswitch2    : Boolean;
      Buff1_Value : Element_Type;
      Buff2_Value : Element_Type;
   begin
      if Reader.Register /= Register'Unrestricted_Access then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "NBAda.Atomic_Single_Writer_Register: Invalid Reader_Id");
      end if;

      MB;
      Register.Reading (Reader.Id) := not Register.Writing (Reader.Id);
      MB;
      Sflag       := Register.Wflag;
      MB;
      Sswitch     := Register.Switch;
      MB;
      Buff1_Value := Register.Buff1;
      MB;
      Sflag2      := Register.Wflag;
      MB;
      Sswitch2    := Register.Switch;
      MB;
      Buff2_Value := Register.Buff2;
      MB;

      if Register.Reading (Reader.Id) = Register.Writing (Reader.Id) then
         MB;
         Value    := Register.Copybuff (Reader.Id);
      elsif Sswitch /= Sswitch2 or Sflag or Sflag2 then
         Value    := Buff2_Value;
      else
         Value    := Buff1_Value;
      end if;
   end Read;

   ----------------------------------------------------------------------------
   function  Register_Reader (Register : in Atomic_1_M_Register)
                             return Reader_Id is
   begin
      for I in Register.Reader'Range loop
         if Register.Reader (I) = 0 and
           then CAS (Target    => Register.Reader (I)'Unrestricted_Access,
                     Old_Value => 0,
                     New_Value => 1)
         then
            return (Id       => I,
                    Register => Register'Unrestricted_Access);
         end if;
      end loop;
      raise Maximum_Number_Of_Readers_Exceeded;
   end Register_Reader;

   ----------------------------------------------------------------------------
   procedure Deregister_Reader (Register : in out Atomic_1_M_Register;
                                Reader   : in     Reader_Id) is
      procedure MB renames NBAda.Primitives.Membar;
   begin
      if Reader.Register = Register'Unrestricted_Access then
         MB;
         Register.Reader (Reader.Id) := 0;
      else
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "NBAda.Atomic_Single_Writer_Register: Invalid Reader_Id");
      end if;
   end Deregister_Reader;


end NBAda.Atomic_Single_Writer_Registers;
