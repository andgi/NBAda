-------------------------------------------------------------------------------
-- Primitives - A binding to the synchronization primitives of the hardware.
-- Copyright (C) 2004  Anders Gidenstam
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : primitives.adb
-- Description     : Synchronization primitives.
-- Author          : Anders Gidenstam
-- Created On      : Fri Jul  5 14:53:50 2002
-- $Id: nbada-primitives.adb,v 1.6 2004/09/21 20:05:23 anders Exp $
-------------------------------------------------------------------------------

with System.Machine_Code;
with Ada.Characters.Latin_1;

package body Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives for SPARC v9.
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
   --  My GCC does not generate 64-bit aware code, so 64-bit objects cannot be
   --  handled atomically.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      raise Not_Implemented;
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Compare_And_Swap_64" & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "mov %3, %%l0"                & LF & HT &   -- l0 <- %3
           "casx [%1], %2, %%l0"         & LF & HT &   -- Compare & swap
           "mov %%l0, %0"                & LF & HT &   -- %0 <- l0
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Compare_And_Swap_64",
         Outputs  => Element'Asm_Output ("=r", New_Value), -- %0 = New_Value
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "l0",
         Volatile => True);
   end Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
      Tmp : Element;
   begin
      raise Not_Implemented;
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Boolean_Compare_And_Swap_64" & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "mov %3, %%l0"                & LF & HT &   -- l0 <- %3
           "casx [%1], %2, %%l0"         & LF & HT &   -- Compare & swap
           "mov %%l0, %0"                & LF & HT &   -- %0 <- l0
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Boolean_Compare_And_Swap_64",
         Outputs  => Element'Asm_Output ("=r", Tmp),       -- %0 = Tmp
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "l0",
         Volatile => True);
      return Tmp = Old_Value;
   end Boolean_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add (Target    : access Unsigned_32;
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
   end Fetch_And_Add;

   ----------------------------------------------------------------------------
   procedure Membar is
      use Ada.Characters.Latin_1;
   begin
      System.Machine_Code.Asm
        (Template =>
           "!#BEGIN Membar"      & LF & HT &
           "membar #LoadLoad | #StoreStore | #LoadStore | #StoreLoad" &LF &HT&
           "!#END Membar");
   end Membar;

end Primitives;
