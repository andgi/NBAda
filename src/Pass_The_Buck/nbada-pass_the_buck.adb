-------------------------------------------------------------------------------
--  Pass-the-buck - An implementation of Herlihy et. al.'s algorithm.
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--  Filename        : pass_the_buck.adb
--  Description     : Lock-Free Ada implementation of Herlihy et. al.'s
--                    Pass-The-Buck Algorithm.
--                    Based on M. Herlihy, V. Luchangco, P. Martin and M. Moir,
--                    "Nonblocking Memory Management Support for
--                     Dynamic-Sized Data Structures", ACM Transcations on
--                    Computer Systems, 23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
--  Created On      : Thu Nov 23 17:30:49 2006
--  $Id: nbada-pass_the_buck.adb,v 1.3 2007/05/18 09:00:07 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Primitives;

with Hash_Tables;

with Ada.Unchecked_Conversion;

package body Pass_The_Buck is

   ----------------------------------------------------------------------
   type Atomic_Boolean is new Primitives.Unsigned_32;

   ----------------------------------------------------------------------
   function Hash_Value (Value : in Value_Type;
                        Size  : in Natural) return Natural;

   package Value_Sets is new Hash_Tables (Value_Type, "=", Hash_Value);

   ----------------------------------------------------------------------
   --  Internal state.

   Max_Guard : aliased Guard_Type := 1;
   pragma Atomic (Max_Guard);

   Guard     : array (Guard_Type range 1 .. Guard_Type (Max_Number_Of_Guards))
     of aliased Atomic_Boolean := (others => 0);
--   pragma Atomic_Components (Guard);
   Post      : array (Guard_Type range 1 .. Guard_Type (Max_Number_Of_Guards))
     of aliased Value_Type := (others => Null_Value);
--   pragma Atomic_Components (Post);
   Hand_Off  : array (Guard_Type range 1 .. Guard_Type (Max_Number_Of_Guards))
     of aliased Value_Type := (others => Null_Value);
--   pragma Atomic_Components (Post);

   ----------------------------------------------------------------------
   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Atomic_Boolean);
   procedure Compare_And_Swap is
      new Primitives.Void_Compare_And_Swap_32 (Guard_Type);
   function  Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap (Value_Type);


   ----------------------------------------------------------------------
   function  Hire_Guard return Guard_Type is
   begin
      loop
         for I in Guard'Range loop
            if Compare_And_Swap (Target    => Guard (I)'Access,
                                 Old_Value => 0,
                                 New_Value => 1)
            then
               declare
                  Max : Guard_Type := Max_Guard;
               begin
                  while Max < I loop
                     Compare_And_Swap (Target    => Max_Guard'Access,
                                       Old_Value => Max,
                                       New_Value => I);
                     Max := Max_Guard;
                  end loop;
               end;

               return I;
            end if;
         end loop;
      end loop;
   end Hire_Guard;

   ----------------------------------------------------------------------
   procedure Fire_Guard (Guard : in Guard_Type) is
   begin
      Primitives.Membar;
      Pass_The_Buck.Guard (Guard) := 0;
      Primitives.Membar;
   end Fire_Guard;

   ----------------------------------------------------------------------
   procedure Post_Guard (Guard : in Guard_Type;
                         Value : in Value_Type) is
   begin
      Primitives.Membar;
      Post (Guard) := Value;
      Primitives.Membar;
   end Post_Guard;

   ----------------------------------------------------------------------
   function Liberate (Values : in Value_Set) return Value_Set is
      use Value_Sets;

      V_Set : Value_Sets.Hash_Table (2 * Max_Number_Of_Guards - 1);
   begin
      --  Insert all values in the V_Set.
      for I in Values'Range loop
         Insert (Values (I), V_Set);
      end loop;

      for I in Guard'Range loop
         declare
            Value : Value_Type := Post (I);
         begin
            if Value /= Null_Value and Member (Value, V_Set) then

               while Value /= Null_Value and Value = Post (I) loop
                  declare
                     H : constant Value_Type := Hand_Off (I);
                  begin
                     if Compare_And_Swap (Target     => Hand_Off (I)'Access,
                                          Old_Value  => H,
                                          New_Value  => Value)
                     then
                        Delete (Value, V_Set);
                        if H /= Null_Value then
                           Insert (H, V_Set);
                        end if;
                        Value := Hand_Off (I);
                     end if;
                  end;
               end loop;

            else

               declare
                  H : constant Value_Type := Hand_Off (I);
               begin
                  if H /= Null_Value and H /= Value then
                     if Compare_And_Swap (Target     => Hand_Off (I)'Access,
                                          Old_Value  => H,
                                          New_Value  => Null_Value)
                     then
                        Insert (H, V_Set);
                     end if;
                  end if;
               end;
            end if;
         end;
      end loop;
      return Value_Set (To_Array (V_Set));
   end Liberate;

   ----------------------------------------------------------------------
   function Hash_Value (Value : in Value_Type;
                        Size  : in Natural) return Natural is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Value_Type,
                                       Primitives.Standard_Unsigned);
      use type Primitives.Standard_Unsigned;
   begin
      return Natural ((To_Unsigned (Value) / 4) mod
                      Primitives.Standard_Unsigned (Size));
   end Hash_Value;

end Pass_The_Buck;
