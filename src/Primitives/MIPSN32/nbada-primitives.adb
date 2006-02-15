-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
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
--  Filename        : primitives.adb
--  Description     : Synchronization primitives.
--  Author          : Anders Gidenstam
--  Created On      : Tue Apr 26 23:49:50 2005
--  $Id: nbada-primitives.adb,v 1.8 2006/02/15 13:33:58 anders Exp $
-------------------------------------------------------------------------------

with System.Machine_Code;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;

package body Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives for MIPS.
   --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  Home made assert construction. Provides some degree of compile time
   --  checking.
   subtype Always_True is Boolean range True .. True;
   type Assertion (Assert : Always_True) is
     null record;

   ----------------------------------------------------------------------------
   --  Backend functions containing the inline assembler.
   --  Labels in assembler code inside a generic function causes
   --  name space problems when the generic is instantiated several times.
   procedure Compare_And_Swap_Unsigned_32 (Target    : access Unsigned_32;
                                           Old_Value : in     Unsigned_32;
                                           New_Value : in out Unsigned_32);
--   pragma Inline (Compare_And_Swap_Unsigned_32);
--   pragma Inline_Always (Compare_And_Swap_Unsigned_32);

   type Unsigned_64 is mod 2**64;
   for Unsigned_64'Size use 64;
   pragma Atomic (Unsigned_64);

   procedure Compare_And_Swap_Unsigned_64 (Target    : access Unsigned_64;
                                           Old_Value : in     Unsigned_64;
                                           New_Value : in out Unsigned_64);
--   pragma Inline (Compare_And_Swap_Unsigned_64);
--   pragma Inline_Always (Compare_And_Swap_Unsigned_64);


   ----------------------------------------------------------------------------
   function Atomic_Read_32 (Target : access Element) return Element is
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
      type Unsigned_Access is access all Unsigned_32;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_32, Element);

      A1 : Assertion (Assert => Element'Object_Size = 32);
   begin
      declare
         New_Val : Unsigned_32 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_32
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
         New_Value := To_Element (New_Val);
      end;
   end Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_32 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;
      type Unsigned_Access is access all Unsigned_32;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_32, Element);

      A1  : Assertion (Assert => Element'Object_Size = 32);
   begin
      declare
         New_Val : Unsigned_32 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_32
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
         return To_Element (New_Val) = Old_Value;
      end;
   end Boolean_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_32 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;
      type Unsigned_Access is access all Unsigned_32;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);

      A1 : Assertion (Assert => Element'Object_Size = 32);
   begin
      declare
         New_Val : Unsigned_32 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_32
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
      end;
   end Void_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   --  gcc 3.4 for MIPS N32 seems to handle 64 bit atomic objects.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;
      type Unsigned_Access is access all Unsigned_64;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_64, Element);

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      declare
         New_Val : Unsigned_64 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_64
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
         New_Value := To_Element (New_Val);
      end;
   end Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;
      type Unsigned_Access is access all Unsigned_64;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_64, Element);

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      declare
         New_Val : Unsigned_64 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_64
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
         return Old_Value = To_Element (New_Val);
      end;
   end Boolean_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_64 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;
      type Unsigned_Access is access all Unsigned_64;

      function To_Unsigned_Access is
         new Ada.Unchecked_Conversion (Element_Access, Unsigned_Access);
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      declare
         New_Val : Unsigned_64 := To_Unsigned (New_Value);
      begin
         Compare_And_Swap_Unsigned_64
           (Target    => To_Unsigned_Access (Element_Access (Target)),
            Old_Value => To_Unsigned (Old_Value),
            New_Value => New_Val);
      end;
   end Void_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add (Target    : access Unsigned_32;
                            Increment : in     Unsigned_32) is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;
   begin
      System.Machine_Code.Asm
        (Template =>
           "# BEGIN Fetch_And_Add_32"        & LF & HT &
           ".set nomove"                     & LF & HT &
           "$FAA1:"                          & LF & HT &
           "ll    $12, 0(%0)"                & LF & HT &
           "add   $12, $12, %1"              & LF & HT &
           "sc    $12, 0(%0)"                & LF & HT &
           "beqz  $12, $FAA1"                & LF & HT &
           "# END Fetch_And_Add_32",
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %0 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %1 = Increment
                                             Increment)),
         Clobber  => "$12",
         Volatile => True);
   end Fetch_And_Add;

   ----------------------------------------------------------------------------
   function Fetch_And_Add (Target    : access Unsigned_32;
                           Increment : in     Unsigned_32)
                          return Unsigned_32 is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;

      Tmp : Unsigned_32;
   begin
      System.Machine_Code.Asm
        (Template =>
           "# BEGIN Fetch_And_Add_32"        & LF & HT &
           ".set nomove"                     & LF &
           "$FAA2:"                          & LF & HT &
           "ll    $13, 0(%1)"                & LF & HT &
           "add   $12, $13, %2"              & LF & HT &
           "sc    $12, 0(%1)"                & LF & HT &
           "beqz  $12, $FAA2"                & LF & HT &
           "move  %0,  $13"                  & LF & HT &
           "# END Fetch_And_Add_32",
         Outputs  => Unsigned_32'Asm_Output ("=r", Tmp),   -- %0 = Tmp
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %1 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %2 = Increment
                                             Increment)),
         Clobber  => "$12,$13",
         Volatile => True);
      return Tmp;
   end Fetch_And_Add;

   ----------------------------------------------------------------------------
   procedure Membar is
      use Ada.Characters.Latin_1;
   begin
      null;
   end Membar;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_Unsigned_32 (Target    : access Unsigned_32;
                                           Old_Value : in     Unsigned_32;
                                           New_Value : in out Unsigned_32) is
      use Ada.Characters.Latin_1;
      type Unsigned_Access is access all Unsigned_32;

      A1 : Assertion (Assert => Unsigned_32'Object_Size = 32);
   begin
      System.Machine_Code.Asm
        (Template =>
           "# BEGIN Compare_And_Swap_32"     & LF & HT &
           ".set nomove"                     & LF &
           "$CAS1:"                          & LF & HT &
           "ll    $12, 0(%1)"                & LF & HT &
           "bne   $12, %2, $CAS2"            & LF & HT &
           "move  $13, %3"                   & LF & HT &
           "sc    $13, 0(%1)"                & LF & HT &
           "beqz  $13, $CAS1"                & LF &
           "$CAS2:"                          & LF & HT &
           "move  %0, $12"                   & LF & HT &
           "# END Compare_And_Swap_32",
         Outputs  =>
           Unsigned_32'Asm_Output ("=r", New_Value),  -- %0 = New_Value
         Inputs   =>
           (Unsigned_Access'Asm_Input ("r",           -- %1 = Target
                                       Unsigned_Access (Target)),
            Unsigned_32'Asm_Input ("r", Old_Value),   -- %2 = Old_Value
            Unsigned_32'Asm_Input ("r", New_Value)),  -- %3 = New_Value
         Clobber  => "$12,$13",
         Volatile => True);
   end Compare_And_Swap_Unsigned_32;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_Unsigned_64 (Target    : access Unsigned_64;
                                           Old_Value : in     Unsigned_64;
                                           New_Value : in out Unsigned_64) is
      use Ada.Characters.Latin_1;
      type Unsigned_Access is access all Unsigned_64;

      A1 : Assertion (Assert => Unsigned_64'Object_Size = 64);
   begin
      System.Machine_Code.Asm
        (Template =>
           "# BEGIN Compare_And_Swap_64"     & LF & HT &
           ".set nomove"                     & LF &
           "$LCAS1:"                         & LF & HT &
           "lld   $12, 0(%1)"                & LF & HT &
           "bne   $12, %2, $LCAS2"           & LF & HT &
           "move  $13, %3"                   & LF & HT &
           "scd   $13, 0(%1)"                & LF & HT &
           "beqz  $13, $LCAS1"               & LF &
           "$LCAS2:"                         & LF & HT &
           "move  %0, $12"                   & LF & HT &
           "# END Compare_And_Swap_64",
         Outputs  =>
           Unsigned_64'Asm_Output ("=r", New_Value), -- %0 = New_Value
         Inputs   =>
           (Unsigned_Access'Asm_Input ("r",          -- %1 = Target
                                       Unsigned_Access (Target)),
            Unsigned_64'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
            Unsigned_64'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "$12,$13",
         Volatile => True);
   end Compare_And_Swap_Unsigned_64;

end Primitives;
