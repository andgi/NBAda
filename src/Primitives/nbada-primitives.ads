-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : primitives.ads
-- Description     : Synchronization primitives.
-- Author          : Anders Gidenstam
-- Created On      : Fri Jul  5 12:27:13 2002
-- $Id: nbada-primitives.ads,v 1.7 2004/09/21 09:52:42 anders Exp $
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
