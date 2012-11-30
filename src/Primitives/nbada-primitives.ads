-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
--  Copyright (C) 2004 - 2012  Anders Gidenstam
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
--  Filename        : primitives.ads
--  Description     : Synchronization primitives.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jul  5 12:27:13 2002
-------------------------------------------------------------------------------

pragma License (GPL);

with System;

package NBAda.Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives.
   --
   ----------------------------------------------------------------------------
   pragma Preelaborate (Primitives);

   ----------------------------------------------------------------------------
   --  Not_Implemented is raised if a particular primitive is not implemented
   --  on the platform in use.
   Not_Implemented : exception;

   ----------------------------------------------------------------------------
   --  NOTE: Memory consistency on multiprocessors/multicores.
   --        Most contemporary multiprocessor or multicore computers
   --        do not guarantee sequential consistency for normal
   --        memory reads and writes. Memory synchronization barriers
   --        MUST be used if the order in which the memory opertations
   --        become visible to other processors matters.
   --        All operations provided by this package include the required
   --        memory barriers.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  Memory synchronization barrier.
   --    Guarantees that all memory accesses by the thread in question before
   --    the barrier takes effect before any accesses after the barrier.
   procedure Membar;
   pragma Inline (Membar);
   pragma Inline_Always (Membar);

   ----------------------------------------------------------------------------
   --  Primitives for the platform's standard word size.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  Unsigned word of the platform's standard word size.

   type Standard_Unsigned is mod 2**System.Word_Size;
   pragma Atomic (Standard_Unsigned);

   ----------------------------------------------------------------------------
   --  Standard Atomic Read.
   --  Reads a single word entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   function Standard_Atomic_Read (Target : access Element) return Element;
   --  Specification:
   --    begin atomic
   --      return Target.all;
   --    end atomic;
   pragma Inline (Standard_Atomic_Read);
   pragma Inline_Always (Standard_Atomic_Read);

   ----------------------------------------------------------------------------
   --  Standard Atomic Write.
   --  Writes a single word entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   procedure Standard_Atomic_Write (Target : access Element;
                                    Value  : in     Element);
   --  Specification:
   --    begin atomic
   --      Target.all := Value;
   --    end atomic;
   pragma Inline (Standard_Atomic_Write);
   pragma Inline_Always (Standard_Atomic_Write);

   ----------------------------------------------------------------------------
   --  Standard Compare and Swap.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   procedure Standard_Compare_And_Swap (Target    : access Element;
                                        Old_Value : in     Element;
                                        New_Value : in out Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --      New_Value := Tmp;
   --    end atomic;
   pragma Inline (Standard_Compare_And_Swap);
   pragma Inline_Always (Standard_Compare_And_Swap);

   ----------------------------------------------------------------------------
   --  Standard Boolean Compare and Swap.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   function Standard_Boolean_Compare_And_Swap (Target    : access Element;
                                               Old_Value : in     Element;
                                               New_Value : in     Element)
                                              return Boolean;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --        return True;
   --      else
   --        return False;
   --      end if;
   --    end atomic;
   pragma Inline (Standard_Boolean_Compare_And_Swap);
   pragma Inline_Always (Standard_Boolean_Compare_And_Swap);

   ----------------------------------------------------------------------------
   --  Standard Void Compare and Swap.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   procedure Standard_Void_Compare_And_Swap (Target    : access Element;
                                             Old_Value : in     Element;
                                             New_Value : in     Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --    end atomic;
   pragma Inline (Standard_Void_Compare_And_Swap);
   pragma Inline_Always (Standard_Void_Compare_And_Swap);


   ----------------------------------------------------------------------------
   --  Standard Fetch and Add.
   --  Use pragma Volatile for Target.all.
   procedure Fetch_And_Add (Target    : access Standard_Unsigned;
                            Increment : in     Standard_Unsigned);
   --  Specification:
   --    begin atomic
   --      Target.all := Target.all + Increment;
   --    end atomic;
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);


   function  Fetch_And_Add (Target    : access Standard_Unsigned;
                            Increment : in     Standard_Unsigned)
                           return Standard_Unsigned;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      Target.all := Tmp + Increment;
   --      return Tmp;
   --    end atomic;
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);


   ----------------------------------------------------------------------------
   --  32-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  32 bit unsigned word.

   type Unsigned_32 is mod 2**32;
   pragma Atomic (Unsigned_32);

   ----------------------------------------------------------------------------
   --  Atomic Read 32.
   --  Reads a 32 bit entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 32.
      type Element is private;
   function Atomic_Read_32 (Target : access Element) return Element;
   --  Specification:
   --    begin atomic
   --      return Target.all;
   --    end atomic;
   pragma Inline (Atomic_Read_32);
   pragma Inline_Always (Atomic_Read_32);

   ----------------------------------------------------------------------------
   --  Atomic Write 32.
   --  Writes a 32 bit entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 32.
      type Element is private;
   procedure Atomic_Write_32 (Target : access Element;
                              Value  : in     Element);
   --  Specification:
   --    begin atomic
   --      Target.all := Value;
   --    end atomic;
   pragma Inline (Atomic_Write_32);
   pragma Inline_Always (Atomic_Write_32);

   ----------------------------------------------------------------------------
   --  Compare and Swap 32.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 32.
      type Element is private;
   procedure Compare_And_Swap_32 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --      New_Value := Tmp;
   --    end atomic;
   pragma Inline (Compare_And_Swap_32);
   pragma Inline_Always (Compare_And_Swap_32);

   ----------------------------------------------------------------------------
   --  Boolean Compare and Swap 32
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 32.
      type Element is private;
   function Boolean_Compare_And_Swap_32 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --        return True;
   --      else
   --        return False;
   --      end if;
   --    end atomic;
   pragma Inline (Boolean_Compare_And_Swap_32);
   pragma Inline_Always (Boolean_Compare_And_Swap_32);

   ----------------------------------------------------------------------------
   --  Void Compare and Swap 32.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 32.
      type Element is private;
   procedure Void_Compare_And_Swap_32 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --    end atomic;
   pragma Inline (Void_Compare_And_Swap_32);
   pragma Inline_Always (Void_Compare_And_Swap_32);


   ----------------------------------------------------------------------------
   --  Fetch and Add 32.
   --  Use pragma Volatile for Target.all.
   procedure Fetch_And_Add_32 (Target    : access Unsigned_32;
                               Increment : in     Unsigned_32);
   --  Specification:
   --    begin atomic
   --      Target.all := Target.all + Increment;
   --    end atomic;
--   pragma Inline (Fetch_And_Add_32);
--   pragma Inline_Always (Fetch_And_Add_32);


   function  Fetch_And_Add_32 (Target    : access Unsigned_32;
                               Increment : in     Unsigned_32)
                              return Unsigned_32;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      Target.all := Tmp + Increment;
   --      return Tmp;
   --    end atomic;
--   pragma Inline (Fetch_And_Add_32);
--   pragma Inline_Always (Fetch_And_Add_32);


   ----------------------------------------------------------------------------
   --  64-bit primitives.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   --  64 bit unsigned word.

   type Unsigned_64 is mod 2**64;
--   pragma Atomic (Unsigned_64);
   --  NOTE: Not all platforms support atomic operations on 64 bit objects.

   ----------------------------------------------------------------------------
   --  Atomic Read 64.
   --  Reads a 64 bit entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 64.
      type Element is private;
   function Atomic_Read_64 (Target : access Element) return Element;
   --  Specification:
   --    begin atomic
   --      return Target.all;
   --    end atomic;
   pragma Inline (Atomic_Read_64);
   pragma Inline_Always (Atomic_Read_64);

   ----------------------------------------------------------------------------
   --  Atomic Write 64.
   --  Writes a 64 bit entity with sequential consistency.
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 64.
      type Element is private;
   procedure Atomic_Write_64 (Target : access Element;
                              Value  : in     Element);
   --  Specification:
   --    begin atomic
   --      Target.all := Value;
   --    end atomic;
   pragma Inline (Atomic_Write_64);
   pragma Inline_Always (Atomic_Write_64);

   ----------------------------------------------------------------------------
   --  Compare and Swap 64
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 64.
      type Element is private;
   procedure Compare_And_Swap_64 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --      New_Value := Tmp;
   --    end atomic;
   pragma Inline (Compare_And_Swap_64);
   pragma Inline_Always (Compare_And_Swap_64);

   ----------------------------------------------------------------------------
   --  Boolean Compare and Swap 64
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 64.
      type Element is private;
   function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --        return True;
   --      else
   --        return False;
   --      end if;
   --    end atomic;
   pragma Inline (Boolean_Compare_And_Swap_64);
   pragma Inline_Always (Boolean_Compare_And_Swap_64);

   ----------------------------------------------------------------------------
   --  Void Compare and Swap 64
   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be 64.
      type Element is private;
   procedure Void_Compare_And_Swap_64 (Target    : access Element;
                                       Old_Value : in     Element;
                                       New_Value : in     Element);
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      if Tmp = Old_Value then
   --        Target.all := New_Value;
   --      end if;
   --    end atomic;
   pragma Inline (Void_Compare_And_Swap_64);
   pragma Inline_Always (Void_Compare_And_Swap_64);

   ----------------------------------------------------------------------------
   --  Fetch and Add 64.
   --  Use pragma Volatile for Target.all.
   procedure Fetch_And_Add_64 (Target    : access Unsigned_64;
                               Increment : in     Unsigned_64);
   --  Specification:
   --    begin atomic
   --      Target.all := Target.all + Increment;
   --    end atomic;
--   pragma Inline (Fetch_And_Add_64);
--   pragma Inline_Always (Fetch_And_Add_64);


   function  Fetch_And_Add_64 (Target    : access Unsigned_64;
                               Increment : in     Unsigned_64)
                              return Unsigned_64;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      Target.all := Tmp + Increment;
   --      return Tmp;
   --    end atomic;
--   pragma Inline (Fetch_And_Add_64);
--   pragma Inline_Always (Fetch_And_Add_64);

end NBAda.Primitives;
