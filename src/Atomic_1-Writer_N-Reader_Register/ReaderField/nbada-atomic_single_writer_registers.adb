-------------------------------------------------------------------------------
--  Ada implementation of atomic multi-word register based on the algorithm
--  by in [A. Larsson, A. Gidenstam, P. H. Ha, M. Papatriantafilou,
--  P. Tsigas, "Multi-word Atomic Read/Write Registers on Multiprocessor
--  Systems", European Symposium on Algorithms, 2004].
--
--  Copyright (C) 2007 - 2012  Anders Gidenstam
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
--  Filename        : atomic_single_writer_registers.adb<2>
--  Description     : Ada implementation of atomic multi-word register.
--                    Based on A. Larsson, A. Gidenstam, P. H. Ha,
--                    M. Papatriantafilou, P. Tsigas,
--                    "Multi-word Atomic Read/Write Registers on Multiprocessor
--                    Systems", Proceedings of the 12th Annual European
--                    Symposium on Algorithms, LNCS 3221 pages 736--748, 2004.
--  Author          : Anders Gidenstam
--  Created On      : Tue Aug 28 20:26:15 2007
--  $Id: nbada-atomic_single_writer_registers.adb,v 1.3 2007/10/24 14:22:44 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with Ada.Exceptions;

package body NBAda.Atomic_Single_Writer_Registers is

   use type Primitives.Standard_Unsigned;
   subtype Standard_Unsigned is Primitives.Standard_Unsigned;


   function CAS is new Primitives.Standard_Boolean_Compare_And_Swap
     (Element => Standard_Unsigned);

   function CAS is new NBAda.Primitives.Boolean_Compare_And_Swap_32
     (Element => NBAda.Primitives.Unsigned_32);


   function Swap (Target : access Standard_Unsigned;
                  Value  : in     Standard_Unsigned) return Standard_Unsigned;
   function Fetch_And_Or (Target : access Standard_Unsigned;
                          Value  : in     Standard_Unsigned)
                         return Standard_Unsigned;

   function Encode (I             : Index;
                    No_Of_Readers : Positive) return Standard_Unsigned;
   pragma Inline_Always (Encode);
   function Decode (W             : Standard_Unsigned;
                    No_Of_Readers : Positive) return Index;
   pragma Inline_Always (Decode);
   function Reader_Bit (R : Positive) return Standard_Unsigned;
   pragma Inline_Always (Reader_Bit);

   ----------------------------------------------------------------------------
   procedure Write (Register : in out Atomic_1_M_Register;
                    Value    : in     Element_Type) is
      Next     : Index;
      Old_Sync : Standard_Unsigned;
   begin
      --  Find an available buffer.
      declare
         Available : array (Register.BUF'Range) of Boolean := (others => True);
      begin
         Available (Register.Last) := False;
         for R in Register.Trace'Range loop
            Available (Register.Trace (R)) := False;
         end loop;
         for I in reverse Available'Range loop
            if Available (I) then
               Next := I;
               exit;
            end if;
         end loop;
      end;
      Register.Last := Next;

      Register.BUF (Next) := Value;

      Old_Sync := Swap (Register.SYNC'Access,
                        Encode (Next, Register.No_Of_Readers));

      --  Update reader trace.
      declare
         Old_Index : constant Index :=
           Decode (Old_Sync, Register.No_Of_Readers);
      begin
         for R in Register.Trace'Range loop
            if (Old_Sync and Reader_Bit (R)) /= 0 then
               Register.Trace (R) := Old_Index;
            end if;
         end loop;
      end;
   end Write;

   ----------------------------------------------------------------------------
   procedure Read  (Register  : in out Atomic_1_M_Register;
                    Reader    : in     Reader_Id;
                    Value     :    out Element_Type) is
      Old_Sync : Standard_Unsigned;
   begin
      if Reader.Register /= Register'Unrestricted_Access then
         Ada.Exceptions.Raise_Exception
           (Constraint_Error'Identity,
            "NBAda.Atomic_Single_Writer_Register: Invalid Reader_Id");
      end if;

      Old_Sync := Fetch_And_Or (Register.SYNC'Access,
                                Reader_Bit (Reader.Id));

      Value    := Register.BUF (Decode (Old_Sync, Register.No_Of_Readers));
   end Read;

   ----------------------------------------------------------------------------
   function Swap (Target : access Standard_Unsigned;
                  Value  : in     Standard_Unsigned)
                 return Standard_Unsigned is
      Old : Standard_Unsigned;
   begin
      loop
         Old := Target.all;
         exit when CAS (Target,
                        Old_Value => Old,
                        New_Value => Value);
      end loop;
      return Old;
   end Swap;

   ----------------------------------------------------------------------------
   function Fetch_And_Or (Target : access Standard_Unsigned;
                          Value  : in     Standard_Unsigned)
                         return Standard_Unsigned is
      Old : Standard_Unsigned;
   begin
      loop
         Old := Target.all;

         exit when
           Old = (Old or Value) or else
           CAS (Target,
                Old_Value => Old,
                New_Value => Old or Value);
      end loop;
      return Old;
   end Fetch_And_Or;

   ----------------------------------------------------------------------------
   function Encode (I             : Index;
                    No_Of_Readers : Positive) return Standard_Unsigned is
   begin
      return Standard_Unsigned (I - Index'First) *
        Reader_Bit (No_Of_Readers + 1);
   end Encode;

   ----------------------------------------------------------------------------
   function Decode (W             : Standard_Unsigned;
                    No_Of_Readers : Positive) return Index is
   begin
      return Index (Integer (Index'First) +
                    Integer (W / Reader_Bit (No_Of_Readers + 1)));
   end Decode;

   ----------------------------------------------------------------------------
   function Reader_Bit (R : Positive) return Standard_Unsigned is
   begin
      return 2**(Natural (R) - 1);
   end Reader_Bit;

   ----------------------------------------------------------------------------
   function  Register_Reader (Register : in Atomic_1_M_Register)
                             return Reader_Id is
      use type NBAda.Primitives.Unsigned_32;
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
