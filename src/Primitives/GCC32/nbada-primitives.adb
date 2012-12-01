-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
--  Copyright (C) 2012  Anders Gidenstam
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
--  Filename        : nbada-primitives.adb
--  Description     : Synchronization primitives.
--  Author          : Anders Gidenstam
--  Created On      : Sat Dec  1 20:40:00 2012
-------------------------------------------------------------------------------

pragma License (GPL);

with System.Address_To_Access_Conversions;
with Ada.Unchecked_Conversion;

package body NBAda.Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives implemented using GCC's intrinsics.
   --
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
      subtype A1 is Assertion (Assert => Element'Object_Size = 32);
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
      subtype A1 is Assertion (Assert => Element'Object_Size = 32);
   begin
      Membar;
      Target.all := Value;
      Membar;
   end Atomic_Write_32;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_32 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      function To_U32 is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_32, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_32
        (Target    : System.Address;
         Old_Value : Unsigned_32;
         New_Value : Unsigned_32) return Unsigned_32;
      pragma Import (Intrinsic,
                     CAS_32,
                     "__sync_val_compare_and_swap_4");

      subtype A1 is Assertion (Assert => Element'Object_Size = 32);
   begin
      New_Value :=
        To_Element (CAS_32 (Convert.To_Address
                              (Convert.Object_Pointer (Target)),
                            To_U32 (Old_Value),
                            To_U32 (New_Value)));
   end Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_32 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      function To_U32 is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_32, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_32
        (Target    : System.Address;
         Old_Value : Unsigned_32;
         New_Value : Unsigned_32) return Boolean;
      pragma Import (Intrinsic,
                     CAS_32,
                     "__sync_bool_compare_and_swap_4");

      subtype A1 is Assertion (Assert => Element'Object_Size = 32);
   begin
      return CAS_32 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     To_U32 (Old_Value),
                     To_U32 (New_Value));
   end Boolean_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_32 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      function To_U32 is
         new Ada.Unchecked_Conversion (Element, Unsigned_32);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_32, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_32
        (Target    : System.Address;
         Old_Value : Unsigned_32;
         New_Value : Unsigned_32) return Boolean;
      pragma Import (Intrinsic,
                     CAS_32,
                     "__sync_bool_compare_and_swap_4");

      subtype A1 is Assertion (Assert => Element'Object_Size = 32);
      Tmp : Boolean;
   begin
      Tmp := CAS_32 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     To_U32 (Old_Value),
                     To_U32 (New_Value));
   end Void_Compare_And_Swap_32;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add_32 (Target    : access Unsigned_32;
                               Increment : in     Unsigned_32) is
      package Convert is
         new System.Address_To_Access_Conversions (Unsigned_32);

      function FAA_32
        (Target    : System.Address;
         Increment : Unsigned_32) return Unsigned_32;
      pragma Import (Intrinsic,
                     FAA_32,
                     "__sync_fetch_and_add_4");
      Tmp : Unsigned_32;
   begin
      Tmp := FAA_32 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     Increment);
   end Fetch_And_Add_32;

   ----------------------------------------------------------------------------
   function Fetch_And_Add_32 (Target    : access Unsigned_32;
                              Increment : in     Unsigned_32)
                             return Unsigned_32 is
      package Convert is
         new System.Address_To_Access_Conversions (Unsigned_32);

      function FAA_32
        (Target    : System.Address;
         Increment : Unsigned_32) return Unsigned_32;
      pragma Import (Intrinsic,
                     FAA_32,
                     "__sync_fetch_and_add_4");
   begin
      return FAA_32 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     Increment);
   end Fetch_And_Add_32;


   ----------------------------------------------------------------------------
   --  64-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Atomic_Read_64 (Target : access Element) return Element is
      subtype A1 is Assertion (Assert => Element'Object_Size = 64);
   begin
      raise Not_Implemented;
      return Target.all;
   end Atomic_Read_64;

   ----------------------------------------------------------------------------
   procedure Atomic_Write_64 (Target : access Element;
                              Value  : in     Element) is
      subtype A1 is Assertion (Assert => Element'Object_Size = 64);
   begin
      raise Not_Implemented;
   end Atomic_Write_64;

   ----------------------------------------------------------------------------
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element) is
      function To_U64 is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_64, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_64
        (Target    : System.Address;
         Old_Value : Unsigned_64;
         New_Value : Unsigned_64) return Unsigned_64;
      pragma Import (Intrinsic,
                     CAS_64,
                     "__sync_val_compare_and_swap_8");

      subtype A1 is Assertion (Assert => Element'Object_Size = 64);
   begin
      New_Value :=
        To_Element (CAS_64 (Convert.To_Address
                              (Convert.Object_Pointer (Target)),
                            To_U64 (Old_Value),
                            To_U64 (New_Value)));
   end Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean is
      function To_U64 is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_64, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_64
        (Target    : System.Address;
         Old_Value : Unsigned_64;
         New_Value : Unsigned_64) return Boolean;
      pragma Import (Intrinsic,
                     CAS_64,
                     "__sync_bool_compare_and_swap_8");

      subtype A1 is Assertion (Assert => Element'Object_Size = 64);
   begin
      return CAS_64 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     To_U64 (Old_Value),
                     To_U64 (New_Value));
   end Boolean_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Void_Compare_And_Swap_64 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element) is
      function To_U64 is
         new Ada.Unchecked_Conversion (Element, Unsigned_64);
      function To_Element is
         new Ada.Unchecked_Conversion (Unsigned_64, Element);
      package Convert is
         new System.Address_To_Access_Conversions (Element);

      function CAS_64
        (Target    : System.Address;
         Old_Value : Unsigned_64;
         New_Value : Unsigned_64) return Boolean;
      pragma Import (Intrinsic,
                     CAS_64,
                     "__sync_bool_compare_and_swap_8");

      subtype A1 is Assertion (Assert => Element'Object_Size = 64);
      Tmp : Boolean;
   begin
      Tmp := CAS_64 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     To_U64 (Old_Value),
                     To_U64 (New_Value));
   end Void_Compare_And_Swap_64;

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add_64 (Target    : access Unsigned_64;
                               Increment : in     Unsigned_64) is
      package Convert is
         new System.Address_To_Access_Conversions (Unsigned_64);

      function FAA_64
        (Target    : System.Address;
         Increment : Unsigned_64) return Unsigned_64;
      pragma Import (Intrinsic,
                     FAA_64,
                     "__sync_fetch_and_add_8");
      Tmp : Unsigned_64;
   begin
      Tmp := FAA_64 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     Increment);
   end Fetch_And_Add_64;

   ----------------------------------------------------------------------------
   function Fetch_And_Add_64 (Target    : access Unsigned_64;
                              Increment : in     Unsigned_64)
                             return Unsigned_64 is
      package Convert is
         new System.Address_To_Access_Conversions (Unsigned_64);

      function FAA_64
        (Target    : System.Address;
         Increment : Unsigned_64) return Unsigned_64;
      pragma Import (Intrinsic,
                     FAA_64,
                     "__sync_fetch_and_add_8");
   begin
      return FAA_64 (Convert.To_Address (Convert.Object_Pointer (Target)),
                     Increment);
   end Fetch_And_Add_64;

   ----------------------------------------------------------------------------
   procedure Membar is
      procedure Synchronize;
      pragma Import (Intrinsic,
                     Synchronize,
                     "__sync_synchronize");
   begin
      Synchronize;
   end Membar;

end NBAda.Primitives;
