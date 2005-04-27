-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
--  Copyright (C) 2004  Anders Gidenstam
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
--  Filename        : primitives.ads
--  Description     : Synchronization primitives.
--  Author          : Anders Gidenstam
--  Created On      : Fri Jul  5 12:27:13 2002
--  $Id: nbada-primitives.ads,v 1.11 2005/04/27 13:14:28 anders Exp $
-------------------------------------------------------------------------------

with Interfaces;

package Primitives is

   ----------------------------------------------------------------------------
   --  Synchronization primitives.
   --
   --
   ----------------------------------------------------------------------------
   pragma Pure (Primitives);

   ----------------------------------------------------------------------------
   --  Not_Implemented is raised if a particular primitive is not implemented
   --  on the platform in use.
   Not_Implemented : exception;

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
   --  Fetch and Add.
   --  Use pragma Volatile for Target.all.
   procedure Fetch_And_Add (Target    : access Unsigned_32;
                            Increment : in     Unsigned_32);
   --  Specification:
   --    begin atomic
   --      Target.all := Target.all + Increment;
   --    end atomic;
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);


   function  Fetch_And_Add (Target    : access Unsigned_32;
                            Increment : in     Unsigned_32)
                           return Unsigned_32;
   --  Specification:
   --    begin atomic
   --      Tmp := Target.all;
   --      Target.all := Tmp + Increment;
   --      return Tmp;
   --    end atomic;
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);

   ----------------------------------------------------------------------------
   --  Memory synchronization barrier.
   --  Guarantees that all memory accesses by the thread in question before
   --  the barrier takes effect before any accesses after the barrier.
   procedure Membar;
   pragma Inline (Membar);
   pragma Inline_Always (Membar);

end Primitives;
