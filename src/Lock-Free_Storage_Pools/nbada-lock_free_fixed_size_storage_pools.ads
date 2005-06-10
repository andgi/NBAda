-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lock_free_fixed_size_storage_pools.ads
-- Description     : A lock-free fixed size storage pool implementation.
-- Author          : Anders Gidenstam
-- Created On      : Thu Apr  3 17:06:17 2003
-- $Id: nbada-lock_free_fixed_size_storage_pools.ads,v 1.2 2005/06/10 08:58:21 andersg Exp $
-------------------------------------------------------------------------------

with System.Storage_Elements;
with System.Storage_Pools;

package Lock_Free_Fixed_Size_Storage_Pools is
   pragma Preelaborate (Lock_Free_Fixed_Size_Storage_Pools);

   ----------------------------------------------------------------------------
   -- The pool size is in blocks.
   type Lock_Free_Storage_Pool
     (Pool_Size  : Natural;
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
                     return Natural;

   ----------------------------------------------------------------------------
   Storage_Exhausted : exception;
   Implementation_Error : exception;

private

   type Pool_Block;

   type Pool_Block_Access is access all Pool_Block;
   pragma Atomic (Pool_Block_Access);

   type Pool_Block is
      record
         Next    : aliased Pool_Block_Access;
      end record;

   type Storage_Array_Access is access System.Storage_Elements.Storage_Array;

   type Lock_Free_Storage_Pool
     (Pool_Size  : Natural;
      Block_Size : System.Storage_Elements.Storage_Count) is
     new System.Storage_Pools.Root_Storage_Pool with
      record
         Storage         : Storage_Array_Access;
         Real_Block_Size : System.Storage_Elements.Storage_Count;
         Free_List       : aliased Pool_Block_Access;
      end record;

   procedure Initialize (Pool : in out Lock_Free_Storage_Pool);
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool);

end Lock_Free_Fixed_Size_Storage_Pools;
