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
--  $Id: nbada-primitives.adb,v 1.12 2007/04/17 17:14:28 andersg Exp $
-------------------------------------------------------------------------------

with System.Machine_Code;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Conversion;

package body Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives for 32-bit SPARC v9.
   --
   --  Requires the flags '-cargs -Wa,-xarch=v8plus' to gnatmake.
   ----------------------------------------------------------------------------

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

      A1 : Assertion (Assert => Element'Object_Size = 32);
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Compare_And_Swap_32" & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "mov %3, %%l0"                & LF & HT &   -- l0 <- %3
           "cas [%1], %2, %%l0"          & LF & HT &   -- Compare & swap
           "mov %%l0, %0"                & LF & HT &   -- %0 <- l0
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=r", New_Value), -- %0 = New_Value
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "l0",
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
      Tmp : Element;
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Boolean_Compare_And_Swap_32" & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "mov %3, %%l0"                & LF & HT &   -- l0 <- %3
           "cas [%1], %2, %%l0"          & LF & HT &   -- Compare & swap
           "mov %%l0, %0"                & LF & HT &   -- %0 <- l0
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Boolean_Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=r", Tmp),       -- %0 = Tmp
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "l0",
         Volatile => True);
      return Tmp = Old_Value;
   end Boolean_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_32 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 32);
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Compare_And_Swap_32" & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "mov %2, %%l0"                & LF & HT &   -- l0 <- %3
           "cas [%0], %1, %%l0"          & LF & HT &   -- Compare & swap
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Compare_And_Swap_32",
         Inputs   => (Element_Access'Asm_Input ("r",       -- %0 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %1 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %2 = New_Value
         Clobber  => "l0",
         Volatile => True);
   end Void_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add_32 (Target    : access Unsigned_32;
                               Increment : in     Unsigned_32) is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Fetch_And_Add_32"      & LF &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "retry1:"                       & LF & HT &   -- retry:
           "ld [%0], %%l0"                 & LF & HT &   --  l0 <- [%0]
           "add %%l0, %1, %%l1"            & LF & HT &   --  l1 <- l0 + %1
           "cas [%0], %%l0, %%l1"          & LF & HT &   --  cas [%0] l0 l1
           "cmp %%l0, %%l1"                & LF & HT &   --  if l0 /= l1
           "bne retry1"                    & LF & HT &   --   goto retry
           "nop"                           & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Fetch_And_Add_32",
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %0 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %1 = Increment
                                             Increment)),
         Clobber  => "l0,l1",
         Volatile => True);
   end Fetch_And_Add_32;

   ----------------------------------------------------------------------------
   function Fetch_And_Add_32 (Target    : access Unsigned_32;
                              Increment : in     Unsigned_32)
                             return Unsigned_32 is
      use Ada.Characters.Latin_1;
      type Unsigned_32_Access is access all Unsigned_32;

      Tmp : Unsigned_32;
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Fetch_And_Add_32"      & LF &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "retry2:"                       & LF & HT &   -- retry:
           "ld [%1], %%l0"                 & LF & HT &   --  l0 <- [%1]
           "add %%l0, %2, %%l1"            & LF & HT &   --  l1 <- l0 + %2
           "cas [%1], %%l0, %%l1"          & LF & HT &   --  cas [%1] l0 l1
           "cmp %%l0, %%l1"                & LF & HT &   --  if l0 /= l1
           "bne retry2"                    & LF & HT &   --   goto retry
           "nop"                           & LF & HT &
           "mov %%l1, %0"                  & LF & HT &   --  %0 <- l1
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Fetch_And_Add_32",
         Outputs  => Unsigned_32'Asm_Output ("=r", Tmp),   -- %0 = Tmp
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %1 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %2 = Increment
                                             Increment)),
         Clobber  => "l0,l1",
         Volatile => True);
      return Tmp;
   end Fetch_And_Add_32;


   ----------------------------------------------------------------------------
   --  64-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  My GCC does not generate 64-bit aware code, so 64-bit objects cannot be
   --  handled atomically.
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
   begin
      raise Not_Implemented;
   end Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      raise Not_Implemented;
      return False;
   end Boolean_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_64 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      raise Not_Implemented;
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
           "!#BEGIN Membar"      & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Membar",
         Volatile => True);
   end Membar;

end Primitives;
