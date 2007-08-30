-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : primitives.adb
--  Description     : Synchronization primitives.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jul  5 14:53:50 2002
--  $Id: nbada-primitives.adb,v 1.17 2007/08/30 13:34:11 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with System.Machine_Code;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;

package body Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives for IA32.
   --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  Configuration options.

   --  NOTE: Multiprocessor memory consistency.
   --   - mfence: Guarantees that all memory reads and writes
   --             issued before the mfence instruction are completed
   --             before any reads or writes after the mfence instruction.
   --   - lfence: Guarantees that all memory loads issued before the lfence
   --             instruction are completed before any loads after the
   --             lfence instruction.
   --   - sfence: Guarantees that all memory stores issued before the sfence
   --             instruction are completed before any stores after the
   --             sfence instruction.

   MFENCE : constant String := "mfence";
   --  NOTE: The "mfence" instruction in the machine code below is
   --        necessary to guarantee sequencially consistent memory on some
   --        modern IA32 multiprocessors.  However, when enabled the
   --        produced binary cannot be run on older x86 machines (like my
   --        AMD Athlon XP 1600+).
   --        To disable the mfence instruction, change the string constant
   --        MFENCE to "#mfence" instead of "mfence".

   PAUSE : constant String := "pause";
   --  NOTE: The "pause" instruction in the machine code below is
   --        important to lower the impact of spinning on
   --        multi-core or hyper-threaded IA32 processors.
   --        To disable the pause instruction, change the string constant
   --        PAUSE to "#pause" instead of "pause".

   CAS_Based_FAA : constant Boolean := True;
   --  NOTE: The pure assembler FAA breaks on some IA32 implementations
   --        when optimized for some reason.
   --        This is a workaround.


   ----------------------------------------------------------------------------
   --  Home made assert construction. Provides some degree of compile time
   --  checking.
   subtype Always_True is Boolean range True .. True;
   type Assertion (Assert : Always_True) is
     null record;

   ----------------------------------------------------------------------------
   --  Primitives for the platform's standard word size.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Standard_Atomic_Read (Target : access Element) return Element is
      function Atomic_Read is new Atomic_Read_32 (Element);
   begin
      return Atomic_Read (Target);
   end Standard_Atomic_Read;

   ----------------------------------------------------------------------------
   procedure Standard_Atomic_Write (Target : access Element;
                                    Value  : in     Element) is
      procedure Atomic_Write is new Atomic_Write_32 (Element);
   begin
      Atomic_Write (Target, Value);
   end Standard_Atomic_Write;

   ----------------------------------------------------------------------------
   procedure Standard_Compare_And_Swap (Target    : access Element;
                                        Old_Value : in     Element;
                                        New_Value : in out Element) is
      procedure Compare_And_Swap is new Compare_And_Swap_32 (Element);
   begin
      Compare_And_Swap (Target, Old_Value, New_Value);
   end Standard_Compare_And_Swap;

   ----------------------------------------------------------------------------
   function Standard_Boolean_Compare_And_Swap (Target    : access Element;
                                               Old_Value : in     Element;
                                               New_Value : in     Element)
                                              return Boolean is
      function Compare_And_Swap is new Boolean_Compare_And_Swap_32 (Element);
   begin
      return Compare_And_Swap (Target, Old_Value, New_Value);
   end Standard_Boolean_Compare_And_Swap;

   ----------------------------------------------------------------------------
   procedure Standard_Void_Compare_And_Swap (Target    : access Element;
                                             Old_Value : in     Element;
                                             New_Value : in     Element) is
      procedure Compare_And_Swap is new Void_Compare_And_Swap_32 (Element);
   begin
      Compare_And_Swap (Target, Old_Value, New_Value);
   end Standard_Void_Compare_And_Swap;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add (Target    : access Standard_Unsigned;
                            Increment : in     Standard_Unsigned) is
      function To_U32 is
         new Ada.Unchecked_Conversion (Standard_Unsigned, Unsigned_32);
      type SU_Access is access all Standard_Unsigned;
      type U32_Access is access all Unsigned_32;
      function To_U32_Access is
         new Ada.Unchecked_Conversion (SU_Access, U32_Access);
   begin
      Fetch_And_Add_32 (To_U32_Access (SU_Access (Target)),
                        To_U32 (Increment));
   end Fetch_And_Add;

   ----------------------------------------------------------------------------
   function  Fetch_And_Add (Target    : access Standard_Unsigned;
                            Increment : in     Standard_Unsigned)
                           return Standard_Unsigned is
      function To_U32 is
         new Ada.Unchecked_Conversion (Standard_Unsigned, Unsigned_32);
      function To_SU is
         new Ada.Unchecked_Conversion (Unsigned_32, Standard_Unsigned);
      type SU_Access is access all Standard_Unsigned;
      type U32_Access is access all Unsigned_32;
      function To_U32_Access is
         new Ada.Unchecked_Conversion (SU_Access, U32_Access);
   begin
      return To_SU (Fetch_And_Add_32 (To_U32_Access (SU_Access (Target)),
                                      To_U32 (Increment)));
   end Fetch_And_Add;


   ----------------------------------------------------------------------------
   --  32-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Atomic_Read_32 (Target : access Element) return Element is
      A1  : Assertion (Assert => Element'Object_Size = 32);
      pragma Unreferenced (A1);
   begin
      Membar;
      declare
         Tmp : constant Element := Target.all;
      begin
         Membar;
         return Tmp;
      end;
   end Atomic_Read_32;

   ----------------------------------------------------------------------------
   procedure Atomic_Write_32 (Target : access Element;
                              Value  : in     Element) is
      A1  : Assertion (Assert => Element'Object_Size = 32);
      pragma Unreferenced (A1);
   begin
      Membar;
      Target.all := Value;
      Membar;
   end Atomic_Write_32;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_32 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1  : Assertion (Assert => Element'Object_Size = 32);
      pragma Unreferenced (A1);
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32"  & LF & HT &
           MFENCE                        & LF & HT &
           "movl %2, %%eax"              & LF & HT &
           "lock cmpxchg %3, (%1)"       & LF & HT &   -- Compare & swap
           "movl %%eax, %0"              & LF & HT &
           MFENCE                        & LF & HT &
           "#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=g", New_Value), -- %0 = New_Value
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("g", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "eax",
         Volatile => True);
   end Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_32 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1  : Assertion (Assert => Element'Object_Size = 32);
      pragma Unreferenced (A1);
      Tmp : Element;
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32"  & LF & HT &
           MFENCE                        & LF & HT &
           "movl %2, %%eax"              & LF & HT &
           "lock cmpxchg %3, (%1)"       & LF & HT &   -- Compare & swap
           "movl %%eax, %0"              & LF & HT &
           MFENCE                        & LF & HT &
           "#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=g", Tmp),       -- %0 = Tmp
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("g", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "eax",
         Volatile => True);
      return Tmp = Old_Value;
   end Boolean_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_32 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1  : Assertion (Assert => Element'Object_Size = 32);
      pragma Unreferenced (A1);
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Void_Compare_And_Swap_32" & LF & HT &
           MFENCE                            & LF & HT &
           "movl %1, %%eax"                  & LF & HT &
           "lock cmpxchg %2, (%0)"           & LF & HT &   -- Compare & swap
           MFENCE                            & LF & HT &
          "#END Void_Compare_And_Swap_32",
         Inputs   => (Element_Access'Asm_Input ("r",       -- %0 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("g", Old_Value),  -- %1 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %2 = New_Value
         Clobber  => "eax",
         Volatile => True);
   end Void_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   function CAS is new Boolean_Compare_And_Swap_32 (Unsigned_32);
   --  Used in the implementation of CAS-based Fetch_And_Add.

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add_32 (Target    : access Unsigned_32;
                               Increment : in     Unsigned_32) is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;
   begin
      if CAS_Based_FAA then
         loop
            declare
               use type Primitives.Unsigned_32;
               Tmp : Unsigned_32;
            begin
               Tmp := Target.all;
               exit when CAS (Target, Tmp, Tmp + Increment);
            end;
         end loop;
      else
         --  This code breaks when optimized.
         System.Machine_Code.Asm
           (Template =>
              "#BEGIN Fetch_And_Add_32"      & LF & HT &
              MFENCE                         & LF & HT &
              "lock xaddl %1, (%0)"          & LF & HT &   -- Fetch & add
              MFENCE                         & LF & HT &
              "#END Fetch_And_Add_32",
            Inputs   => (Unsigned_32_Access'Asm_Input         -- %0 = Target
                         ("r", Unsigned_32_Access (Target)),
                         Unsigned_32'Asm_Input ("r",          -- %1 = Increment
                                                Increment)),
            Volatile => True);
      end if;
   end Fetch_And_Add_32;

   ----------------------------------------------------------------------------
   function Fetch_And_Add_32 (Target    : access Unsigned_32;
                              Increment : in     Unsigned_32)
                             return Unsigned_32 is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;

      Tmp : Unsigned_32;
   begin
      if CAS_Based_FAA then
         loop
            declare
               use type Primitives.Unsigned_32;
            begin
               Tmp := Target.all;
               exit when CAS (Target, Tmp, Tmp + Increment);
            end;
         end loop;
      else
         --  This code breaks when optimized.
         System.Machine_Code.Asm
           (Template =>
              "#BEGIN Fetch_And_Add_32"      & LF & HT &
              MFENCE                         & LF & HT &
              "lock xaddl %2, (%1)"          & LF & HT &   -- Fetch & add
              "movl %2, %0"                  & LF & HT &
              MFENCE                         & LF & HT &
              "#END Fetch_And_Add_32",
            Outputs  => Unsigned_32'Asm_Output ("=r", Tmp),   -- %0 = Tmp
            Inputs   => (Unsigned_32_Access'Asm_Input         -- %1 = Target
                         ("r", Unsigned_32_Access (Target)),
                         Unsigned_32'Asm_Input ("r",          -- %2 = Increment
                                                Increment)),
            Volatile => True);
      end if;
      return Tmp;
   end Fetch_And_Add_32;


   ----------------------------------------------------------------------------
   --  64-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  The newer IA32 CPUs supports atomic operations on 64 bit objects.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Atomic_Read_64 (Target : access Element) return Element is
   begin
      raise Not_Implemented;
   end Atomic_Read_64;

   ----------------------------------------------------------------------------
   procedure Atomic_Write_64 (Target : access Element;
                              Value  : in     Element) is
   begin
      raise Not_Implemented;
   end Atomic_Write_64;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
      pragma Unreferenced (A1);

      type Unsigned_32_Array is array (1 .. 2) of aliased Unsigned_32;
      for Unsigned_32_Array'Size use 64;
      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      function To_UA is new
        Ada.Unchecked_Conversion (Element_Access, Unsigned_32_Array_Access);

      Tmp_Old : aliased Element := Old_Value;  --  In edx:eax
      Tmp_New : aliased Element := New_Value;  --  In ecx:ebx
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_64"  & LF & HT &
           "movl %2, %%edi"              & LF & HT &
           MFENCE                        & LF & HT &
           "lock cmpxchg8b (%%edi)"      & LF & HT &   -- Compare & swap
           MFENCE                        & LF & HT &
           "#END Compare_And_Swap_64",
         Outputs  => (Unsigned_32'Asm_Output   ("=a",       -- %0 old[1] = eax
                                                To_UA (Tmp_Old'Access)(1)),
                      Unsigned_32'Asm_Output   ("=d",       -- %1 old[2] = edx
                                                To_UA (Tmp_Old'Access)(2))),
         Inputs   => (Element_Access'Asm_Input ("m",        -- %2 = Target
                                                Element_Access (Target)),
                      Unsigned_32'Asm_Input    ("a",        -- %3 eax = old[1]
                                                To_UA (Tmp_Old'Access)(1)),
                      Unsigned_32'Asm_Input    ("d",        -- %4 edx = old[2]
                                                To_UA (Tmp_Old'Access)(2)),
                      Unsigned_32'Asm_Input    ("b",        -- %5 ebx = new[1]
                                                To_UA (Tmp_New'Access)(1)),
                      Unsigned_32'Asm_Input    ("c",        -- %6 ecx = new[2]
                                                To_UA (Tmp_New'Access)(2))),
         Clobber  => "edi",
         Volatile => True);
      New_Value := Tmp_Old;
   end Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
      pragma Unreferenced (A1);

      type Unsigned_32_Array is array (1 .. 2) of aliased Unsigned_32;
      for Unsigned_32_Array'Size use 64;
      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      function To_UA is new
        Ada.Unchecked_Conversion (Element_Access, Unsigned_32_Array_Access);

      Tmp_Old : aliased Element := Old_Value;  --  In edx:eax
      Tmp_New : aliased Element := New_Value;  --  In ecx:ebx
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_64"  & LF & HT &
           "movl %2, %%edi"              & LF & HT &
           MFENCE                        & LF & HT &
           "lock cmpxchg8b (%%edi)"      & LF & HT &   -- Compare & swap
           MFENCE                        & LF & HT &
           "#END Compare_And_Swap_64",
         Outputs  => (Unsigned_32'Asm_Output   ("=a",       -- %0 old[1] = eax
                                                To_UA (Tmp_Old'Access)(1)),
                      Unsigned_32'Asm_Output   ("=d",       -- %1 old[2] = edx
                                                To_UA (Tmp_Old'Access)(2))),
         Inputs   => (Element_Access'Asm_Input ("m",        -- %2 = Target
                                                Element_Access (Target)),
                      Unsigned_32'Asm_Input    ("a",        -- %3 eax = old[1]
                                                To_UA (Tmp_Old'Access)(1)),
                      Unsigned_32'Asm_Input    ("d",        -- %4 edx = old[2]
                                                To_UA (Tmp_Old'Access)(2)),
                      Unsigned_32'Asm_Input    ("b",        -- %5 ebx = new[1]
                                                To_UA (Tmp_New'Access)(1)),
                      Unsigned_32'Asm_Input    ("c",        -- %6 ecx = new[2]
                                                To_UA (Tmp_New'Access)(2))),
         Clobber  => "edi",
         Volatile => True);
      return Old_Value = Tmp_Old;
   end Boolean_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_64 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
      pragma Unreferenced (A1);

      type Unsigned_32_Array is array (1 .. 2) of aliased Unsigned_32;
      for Unsigned_32_Array'Size use 64;
      type Unsigned_32_Array_Access is access all Unsigned_32_Array;

      function To_UA is new
        Ada.Unchecked_Conversion (Element_Access, Unsigned_32_Array_Access);

      Tmp_Old : aliased Element := Old_Value;  --  In edx:eax
      Tmp_New : aliased Element := New_Value;  --  In ecx:ebx
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_64"  & LF & HT &
           "movl %0, %%edi"              & LF & HT &
           MFENCE                        & LF & HT &
           "lock cmpxchg8b (%%edi)"      & LF & HT &   -- Compare & swap
           MFENCE                        & LF & HT &
           "#END Compare_And_Swap_64",
         Inputs   => (Element_Access'Asm_Input ("m",        -- %0 = Target
                                                Element_Access (Target)),
                      Unsigned_32'Asm_Input    ("a",        -- %1 eax = old[1]
                                                To_UA (Tmp_Old'Access)(1)),
                      Unsigned_32'Asm_Input    ("d",        -- %2 edx = old[2]
                                                To_UA (Tmp_Old'Access)(2)),
                      Unsigned_32'Asm_Input    ("b",        -- %3 ebx = new[1]
                                                To_UA (Tmp_New'Access)(1)),
                      Unsigned_32'Asm_Input    ("c",        -- %4 ecx = new[2]
                                                To_UA (Tmp_New'Access)(2))),
         Clobber  => "edi",
         Volatile => True);
   end Void_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add_64 (Target    : access Unsigned_64;
                               Increment : in     Unsigned_64) is
   begin
      raise Not_Implemented;
   end Fetch_And_Add_64;

   ----------------------------------------------------------------------------
   function Fetch_And_Add_64 (Target    : access Unsigned_64;
                              Increment : in     Unsigned_64)
                             return Unsigned_64 is
   begin
      raise Not_Implemented;
      return 0;
   end Fetch_And_Add_64;

   ----------------------------------------------------------------------------
   procedure Membar is
      use Ada.Characters.Latin_1;
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Membar" & LF & HT &
           MFENCE          & LF & HT &
           "#END Membar",
        Volatile => True);
   end Membar;

end Primitives;
