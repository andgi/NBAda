-------------------------------------------------------------------------------
--  Lock-free growing storage pool for fixed sized blocks.
--  Copyright (C) 2005  Anders Gidenstam
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_fixed_size_storage_pools.ads
--  Description     : A lock-free fixed storage pool implementation.
--  Author          : Anders Gidenstam
--  Created On      : Tue Jun 14 17:00:17 2005
--  $Id: nbada-lock_free_growing_storage_pools.ads,v 1.3 2006/03/03 17:43:22 anders Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (Modified_GPL);

with System.Storage_Elements;
with System.Storage_Pools;

with Lock_Free_Fixed_Size_Storage_Pools;

package Lock_Free_Growing_Storage_Pools is
   pragma Preelaborate (Lock_Free_Growing_Storage_Pools);

   type Lock_Free_Storage_Pool
     (Block_Size : System.Storage_Elements.Storage_Count) is
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
                     return Natural;

   ----------------------------------------------------------------------------
   Storage_Exhausted : exception;
   Implementation_Error : exception;

private

   type Element_Pool;
   type Element_Pool_Access is access Element_Pool;

   type Element_Pool is new
     Lock_Free_Fixed_Size_Storage_Pools.Lock_Free_Storage_Pool with
      record
         Next : aliased Element_Pool_Access;
         pragma Atomic (Next);
      end record;

   type Lock_Free_Storage_Pool
     (Block_Size : System.Storage_Elements.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with
      record
         Pool_List : aliased Element_Pool_Access;
         pragma Atomic (Pool_List);
      end record;

   procedure Initialize (Pool : in out Lock_Free_Storage_Pool);
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool);

end Lock_Free_Growing_Storage_Pools;
