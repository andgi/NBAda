-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lock_free_fixed_size_storage_pools.adb
-- Description     : A lock-free fixed size storage pool implementation.
-- Author          : Anders Gidenstam
-- Created On      : Thu Apr  3 17:50:52 2003
-- $Id: nbada-lock_free_fixed_size_storage_pools.adb,v 1.1 2003/04/03 17:09:06 andersg Exp $
-------------------------------------------------------------------------------

with Primitives;

package body Lock_Free_Fixed_Size_Storage_Pools is

   ----------------------------------------------------------------------------
   function CAS is new
     Primitives.Boolean_Compare_And_Swap_32 (Pool_Block_Access);

   ----------------------------------------------------------------------------
   procedure Allocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      use type System.Storage_Elements.Storage_Offset;

      Block : Pool_Block_Access;
   begin
      if Size_In_Storage_Elements > Block_Size then
         raise Storage_Error;
      end if;

      loop
         Block := Pool.Free_List;

         if Block = null then
            raise Storage_Exhausted;
         end if;

         exit when CAS (Target    => Pool.Free_List'Access,
                        Old_Value => Block,
                        New_Value => Block.Next);
      end loop;

      Storage_Address := Block.Storage'Address;
   end Allocate;

   ----------------------------------------------------------------------------
   procedure Deallocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is

      Block : Pool_Block_Access;
   begin
      -- Fix recovery of Block's address.
      null;

--       loop
--          Block.Next := Pool.Free_List;

--          exit when CAS (Target    => Pool.Free_List'Access,
--                         Old_Value => Block.Next,
--                         New_Value => Block);
--       end loop;
   end Deallocate;

   ----------------------------------------------------------------------------
   function Storage_Size (Pool : Lock_Free_Storage_Pool)
                         return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Count;
   begin
      return Block_Size *
        System.Storage_Elements.Storage_Count (Pool.Pool_Size);
   end Storage_Size;

   ----------------------------------------------------------------------------
   procedure Initialize (Pool : in out Lock_Free_Storage_Pool) is
   begin
      for I in Pool.Storage'Range loop
         declare
            Block : Pool_Block_Access := Pool.Storage (I)'Unchecked_Access;
         begin
            loop
               Block.Next := Pool.Free_List;

               exit when CAS (Target    => Pool.Free_List'Access,
                              Old_Value => Block.Next,
                              New_Value => Block);
            end loop;
         end;
      end loop;
   end Initialize;

end Lock_Free_Fixed_Size_Storage_Pools;
