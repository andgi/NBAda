-------------------------------------------------------------------------------
--  Lock-free fixed size storage pool.
--  Copyright (C) 2003 - 2005  Anders Gidenstam
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
--  Filename        : lock_free_fixed_size_storage_pools.ads
--  Description     : A lock-free fixed size storage pool implementation.
--  Author          : Anders Gidenstam
--  Created On      : Thu Apr  3 17:06:17 2003
--  $Id: nbada-lock_free_fixed_size_storage_pools.ads,v 1.3 2005/06/10 14:31:09 anders Exp $
-------------------------------------------------------------------------------

with System.Storage_Elements;
with System.Storage_Pools;

package Lock_Free_Fixed_Size_Storage_Pools is
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
   Storage_Exhausted : exception;
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

   Null_Ref : constant Pool_Block_Ref := (Block_Index'Last, 0);

   type Pool_Block is
      record
         Next    : aliased Pool_Block_Ref := Null_Ref;
         pragma Atomic (Next);
      end record;
   pragma Atomic (Pool_Block);

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   type Lock_Free_Storage_Pool
     (Pool_Size  : Block_Count;
      Block_Size : System.Storage_Elements.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with
      record
         Storage         : Storage_Array_Access;
         pragma Atomic (Storage);
         Real_Block_Size : System.Storage_Elements.Storage_Count;
         Free_List       : aliased Pool_Block_Ref;
         pragma Atomic (Free_List);
      end record;

   procedure Initialize (Pool : in out Lock_Free_Storage_Pool);
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool);

end Lock_Free_Fixed_Size_Storage_Pools;
