-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : primitives.adb
-- Description     : Synchronization primitives.
-- Author          : Anders Gidenstam
-- Created On      : Fri Jul  5 14:53:50 2002
-- $Id: nbada-primitives.adb,v 1.3 2004/09/20 23:18:13 anders Exp $
-------------------------------------------------------------------------------

with System.Machine_Code;
with Ada.Characters.Latin_1;

package body Primitives is

   ----------------------------------------------------------------------------
   -- Synchronization primitives for IA32.
   --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   -- Home made assert construction. Provides some degree of compile time
   -- checking.
   subtype Always_True is Boolean range True .. True;
   type Assertion (Assert : Always_True) is
     null record;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_32 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1  : Assertion (Assert => Element'Object_Size = 32);
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32" & LF & HT &
           "movl %2, %%eax"              & LF & HT &
           "lock"                        & LF & HT &
           "cmpxchg %3, (%1)"            & LF & HT &   -- Compare & swap
           "movl %%eax, %0"              & LF & HT &
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
      Tmp : Element;
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32" & LF & HT &
           "movl %2, %%eax"              & LF & HT &
           "lock"                        & LF & HT &
           "cmpxchg %3, (%1)"            & LF & HT &   -- Compare & swap
           "movl %%eax, %0"              & LF & HT &
           "#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=g", Tmp),       -- %0 = Tmp
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("g", New_Value)), -- %3 = New_Value
         Clobber  => "eax",
         Volatile => True);
      return Tmp = Old_Value;
   end Boolean_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   -- My GCC does not generate 64-bit aware code, so 64-bit objects cannot be
   -- handled atomically. Not tested on IA32 yet.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      use Ada.Characters.Latin_1;
      type Element_Access is access all Element;

      A1 : Assertion (Assert => Element'Object_Size = 64);
   begin
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32" & LF & HT &
--         "movl %2, %%eax"              & LF & HT &
--            "lock"                        & LF & HT &
--         "cmpxchg8b %3, (%1)"          & LF & HT &   -- Compare & swap
--         "movl %%eax, %0"              & LF & HT &
           "#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=r", New_Value), -- %0 = New_Value
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "eax",
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
      System.Machine_Code.Asm
        (Template =>
           "#BEGIN Compare_And_Swap_32" & LF & HT &
--         "movl %2, %%eax"              & LF & HT &
--            "lock"                        & LF & HT &
--         "cmpxchg8b %3, (%1)"          & LF & HT &   -- Compare & swap
--         "movl %%eax, %0"              & LF & HT &
           "#END Compare_And_Swap_32",
         Outputs  => Element'Asm_Output ("=r", Tmp),       -- %0 = Tmp
         Inputs   => (Element_Access'Asm_Input ("r",       -- %1 = Target
                                                Element_Access (Target)),
                      Element'Asm_Input ("r", Old_Value),  -- %2 = Old_Value
                      Element'Asm_Input ("r", New_Value)), -- %3 = New_Value
         Clobber  => "eax",
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
           "#BEGIN Fetch_And_Add_32"      & LF &
           "lock"                         & LF & HT &
           "xaddl %1, (%0)"               & LF & HT &   -- Fetch & add
           "#END Fetch_And_Add_32",
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %0 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %1 = Increment
                                             Increment)),
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
           "#BEGIN Fetch_And_Add_32"      & LF &
           "lock"                         & LF & HT &
           "xaddl %2, (%1)"               & LF & HT &   -- Fetch & add
           "movl %2, %0"                  & LF & HT &
           "#END Fetch_And_Add_32",
         Outputs  => Unsigned_32'Asm_Output ("=r", Tmp),   -- %0 = Tmp
         Inputs   => (Unsigned_32_Access'Asm_Input         -- %1 = Target
                      ("r", Unsigned_32_Access (Target)),
                      Unsigned_32'Asm_Input ("r",          -- %2 = Increment
                                             Increment)),
         Volatile => True);
      return Tmp;
   end Fetch_And_Add;

   ----------------------------------------------------------------------------
   procedure Membar is
   begin
      null;
   end Membar;

   ----------------------------------------------------------------------------
   procedure Membar_StoreLoad is
   begin
      null;
   end Membar_StoreLoad;

   ----------------------------------------------------------------------------
   procedure Membar_StoreStore_LoadStore is
   begin
      null;
   end Membar_StoreStore_LoadStore;


end Primitives;
