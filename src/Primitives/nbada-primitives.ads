-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : primitives.ads
-- Description     : Synchronization primitives.
-- Author          : Anders Gidenstam
-- Created On      : Fri Jul  5 12:27:13 2002
-- $Id: nbada-primitives.ads,v 1.5 2003/03/12 13:42:18 andersg Exp $
-------------------------------------------------------------------------------

with Interfaces;

package Primitives is

   ----------------------------------------------------------------------------
   -- Synchronization primitives.
   --
   --
   ----------------------------------------------------------------------------
   pragma Pure (Primitives);

   ----------------------------------------------------------------------------
   -- Not_Implemented is raised if a particular primitive is not implemented
   -- on the platform in use.
   Not_Implemented : exception;

   ----------------------------------------------------------------------------
   -- 32 bit unsigned word.
   type Unsigned_32 is mod 2**32;
   pragma Atomic (Unsigned_32);

   ----------------------------------------------------------------------------
   -- Compare and Swap 32
   generic
      -- Use pragma Atomic and pragma Volatile for Target.
      -- Element'Object_Size MUST be 32.
      type Element is private;
   procedure Compare_And_Swap_32 (Target    : access Element;
                                  Old_Value : in     Element;
                                  New_Value : in out Element);
   pragma Inline (Compare_And_Swap_32);
   pragma Inline_Always (Compare_And_Swap_32);

   ----------------------------------------------------------------------------
   -- Boolean Compare and Swap 32
   generic
      -- Use pragma Atomic and pragma Volatile for Target.
      -- Element'Object_Size MUST be 32.
      type Element is private;
   function Boolean_Compare_And_Swap_32 (Target    : access Element;
                                         Old_Value : in     Element;
                                         New_Value : in     Element)
                                        return Boolean;
   pragma Inline (Boolean_Compare_And_Swap_32);
   pragma Inline_Always (Boolean_Compare_And_Swap_32);

    ----------------------------------------------------------------------------
    -- Compare and Swap 64
    generic
       -- Use pragma Atomic and pragma Volatile for Target.
       -- Element'Object_Size MUST be 64.
       type Element is private;
    procedure Compare_And_Swap_64 (Target    : access Element;
                                   Old_Value : in     Element;
                                   New_Value : in out Element);
    pragma Inline (Compare_And_Swap_64);
    pragma Inline_Always (Compare_And_Swap_64);

    ----------------------------------------------------------------------------
    -- Boolean Compare and Swap 64
    generic
       -- Use pragma Atomic and pragma Volatile for Target.
       -- Element'Object_Size MUST be 64.
       type Element is private;
    function Boolean_Compare_And_Swap_64 (Target    : access Element;
                                          Old_Value : in     Element;
                                          New_Value : in     Element)
                                         return Boolean;
    pragma Inline (Boolean_Compare_And_Swap_64);
    pragma Inline_Always (Boolean_Compare_And_Swap_64);

   ----------------------------------------------------------------------------
   -- Fetch and Add
   -- Use pragma Volatile for Target.all.
   procedure Fetch_And_Add (Target    : access Unsigned_32;
                            Increment : in     Unsigned_32);
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);
   function  Fetch_And_Add (Target    : access Unsigned_32;
                            Increment : in     Unsigned_32)
                           return Unsigned_32;
   pragma Inline (Fetch_And_Add);
   pragma Inline_Always (Fetch_And_Add);

   ----------------------------------------------------------------------------
   -- Memory barriers
   procedure Membar;
   pragma Inline (Membar);
   pragma Inline_Always (Membar);
   procedure Membar_StoreLoad;
   pragma Inline (Membar_StoreLoad);
   pragma Inline_Always (Membar_StoreLoad);
   procedure Membar_StoreStore_LoadStore;
   pragma Inline (Membar_StoreStore_LoadStore);
   pragma Inline_Always (Membar_StoreStore_LoadStore);

end Primitives;
