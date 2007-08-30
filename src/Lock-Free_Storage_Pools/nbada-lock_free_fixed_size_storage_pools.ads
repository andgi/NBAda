-------------------------------------------------------------------------------
--  Lock-free fixed size storage pool.
--  Copyright (C) 2003 - 2007  Anders Gidenstam
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_fixed_size_storage_pools.ads
--  Description     : A lock-free fixed size storage pool implementation.
--  Author          : Anders Gidenstam
--  Created On      : Thu Apr  3 17:06:17 2003
--  $Id: nbada-lock_free_fixed_size_storage_pools.ads,v 1.9 2007/08/30 15:13:13 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with System.Storage_Elements;
with System.Storage_Pools;

package NBAda.Lock_Free_Fixed_Size_Storage_Pools is

   pragma Preelaborate (Lock_Free_Fixed_Size_Storage_Pools);

   ----------------------------------------------------------------------------
   --  The pool size is in blocks.
   type Block_Count is range 0 .. 2**16 - 1;

   type Lock_Free_Storage_Pool
     (Pool_Size  : Block_Count;
      Block_Size : System.Storage_Elements.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with private;

   ----------------------------------------------------------------------------
   procedure Allocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count);

   ----------------------------------------------------------------------------
   procedure Deallocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count);

   ----------------------------------------------------------------------------
   function Storage_Size (Pool : Lock_Free_Storage_Pool)
                         return System.Storage_Elements.Storage_Count;

   ----------------------------------------------------------------------------
   function Validate (Pool : Lock_Free_Storage_Pool)
                     return Block_Count;

   ----------------------------------------------------------------------------
   function Belongs_To (Pool            : Lock_Free_Storage_Pool;
                        Storage_Address : System.Address)
                       return Boolean;

   ----------------------------------------------------------------------------
   Storage_Exhausted    : exception;
   Implementation_Error : exception;

private

   --  The storage pool free-list uses 16-bit version tags to avoid
   --  ABA-problems.
   type Version_Number is mod 2**16;
   type Block_Index is mod 2**16;

   type Pool_Block_Ref is
      record
         Index   : Block_Index    := Block_Index'Last;
         Version : Version_Number := 0;
      end record;
   for Pool_Block_Ref'Size use 32;
   pragma Atomic (Pool_Block_Ref);

   Null_Ref : constant Pool_Block_Ref := (Block_Index'Last, 0);

   type Pool_Block is
      record
         Next    : aliased Pool_Block_Ref := Null_Ref;
         pragma Atomic (Next);
      end record;
   pragma Atomic (Pool_Block);

   type Atomic_Storage_Array is new System.Storage_Elements.Storage_Array;
   pragma Atomic_Components (Atomic_Storage_Array);

   type Storage_Array_Access is access Atomic_Storage_Array;

   type Lock_Free_Storage_Pool
     (Pool_Size  : Block_Count;
      Block_Size : System.Storage_Elements.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with
      record
         Real_Block_Size : System.Storage_Elements.Storage_Count;
         Storage         : Storage_Array_Access;
         pragma Atomic (Storage);
         Free_List       : aliased Pool_Block_Ref;
         pragma Atomic (Free_List);
      end record;

   procedure Initialize (Pool : in out Lock_Free_Storage_Pool);
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool);

end NBAda.Lock_Free_Fixed_Size_Storage_Pools;
