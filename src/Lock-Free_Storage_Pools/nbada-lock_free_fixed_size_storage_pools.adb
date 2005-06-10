-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lock_free_fixed_size_storage_pools.adb
-- Description     : A lock-free fixed size storage pool implementation.
-- Author          : Anders Gidenstam
-- Created On      : Thu Apr  3 17:50:52 2003
-- $Id: nbada-lock_free_fixed_size_storage_pools.adb,v 1.2 2005/06/10 08:58:21 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;
with Primitives;

package body Lock_Free_Fixed_Size_Storage_Pools is

   ----------------------------------------------------------------------------
   procedure Free is
      new Ada.Unchecked_Deallocation (System.Storage_Elements.Storage_Array,
                                      Storage_Array_Access);

   ----------------------------------------------------------------------------
   package Pool_Blocks is
      new System.Address_To_Access_Conversions (Pool_Block);

   ----------------------------------------------------------------------------
   function CAS is new
     Primitives.Boolean_Compare_And_Swap_32 (Pool_Block_Access);

   ----------------------------------------------------------------------------
   -- Home made assert construction. Provides some degree of compile time
   -- checking.
   subtype Always_True is Boolean range True .. True;
   type Assertion (Assert : Always_True) is
     null record;

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
      if Size_In_Storage_Elements > Pool.Block_Size then
         raise Storage_Error;
      end if;

      loop
         Block := Pool.Free_List;

         if Block = null then
            raise Storage_Exhausted;
         end if;
         declare
            use type System.Address;
         begin
            if Block.all'Address < Pool.Storage (Pool.Storage'First)'Address or
              Block.all'Address > Pool.Storage (Pool.Storage'Last)'Address
            then
               raise Implementation_Error;
            end if;
         end;
         declare
            use type System.Address;
         begin
            if Block.Next.all'Address < Pool.Storage (Pool.Storage'First)'Address or
              Block.Next.all'Address > Pool.Storage (Pool.Storage'Last)'Address
            then
               raise Implementation_Error;
            end if;
         end;

         exit when CAS (Target    => Pool.Free_List'Access,
                        Old_Value => Block,
                        New_Value => Block.Next);
      end loop;
      declare
         use type System.Address;
      begin
         if Pool.Free_List.all'Address < Pool.Storage (Pool.Storage'First)'Address or
           Pool.Free_List.all'Address > Pool.Storage (Pool.Storage'Last)'Address
         then
            raise Implementation_Error;
         end if;
      end;

      Storage_Address := --Block.all'Address;
        Pool_Blocks.To_Address (Pool_Blocks.Object_Pointer (Block));

      declare
         use type System.Address;
      begin
         if Storage_Address /= Block.all'Address or
            Storage_Address mod Alignment /= 0
         then
            raise Implementation_Error;
         end if;
      end;
   end Allocate;

   ----------------------------------------------------------------------------
   procedure Deallocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      use type System.Address;

      Block : Pool_Block_Access;
   begin
      Block := Pool_Block_Access (Pool_Blocks.To_Pointer (Storage_Address));

      if Storage_Address < Pool.Storage (Pool.Storage'First)'Address or
         Storage_Address > Pool.Storage (Pool.Storage'Last)'Address
      then
         raise Storage_Error;
      end if;
      if Block.all'Address /= Storage_Address then
         raise Implementation_Error;
      end if;

      loop
         Block.Next := Pool.Free_List;

         exit when CAS (Target    => Pool.Free_List'Access,
                        Old_Value => Block.Next,
                        New_Value => Block);
      end loop;

      declare
         use type System.Address;
      begin
         if Pool.Free_List.all'Address < Pool.Storage (Pool.Storage'First)'Address or
           Pool.Free_List.all'Address > Pool.Storage (Pool.Storage'Last)'Address
         then
            raise Implementation_Error;
         end if;
      end;
   end Deallocate;

   ----------------------------------------------------------------------------
   function Storage_Size (Pool : Lock_Free_Storage_Pool)
                         return System.Storage_Elements.Storage_Count is
      use type System.Storage_Elements.Storage_Count;
   begin
      return Pool.Real_Block_Size *
        System.Storage_Elements.Storage_Count (Pool.Pool_Size);
   end Storage_Size;

   ----------------------------------------------------------------------------
   function Validate (Pool : Lock_Free_Storage_Pool)
                     return Natural is
      use type System.Address;
      Block      : Pool_Block_Access := Pool.Free_List;
      No_Of_Free : Natural := 0;
   begin
      while Block /= null loop
         if Block.all'Address < Pool.Storage (Pool.Storage'First)'Address or
            Block.all'Address > Pool.Storage (Pool.Storage'Last)'Address
         then
            raise Implementation_Error;
         end if;
         No_Of_Free := No_Of_Free + 1;
         Block := Block.Next;
      end loop;
      return No_Of_Free;
   end Validate;

   ----------------------------------------------------------------------------
   procedure Initialize (Pool : in out Lock_Free_Storage_Pool) is
      use System.Storage_Elements;
   begin
      Pool.Real_Block_Size :=
        Storage_Count'Max (Pool.Block_Size,
                           Pool_Block'Max_Size_In_Storage_Elements);
      Pool.Storage := new Storage_Array
        (0 .. Storage_Count (Pool.Pool_Size) * Pool.Real_Block_Size);
      Pool.Free_List := null;
      Primitives.Membar;

      for I in 0 .. Storage_Count (Pool.Pool_Size - 1) loop
         declare
            Block : Pool_Block_Access :=
              Pool_Block_Access
               (Pool_Blocks.To_Pointer
                (Pool.Storage (I * Pool.Real_Block_Size)'Address));
            use type System.Address;
         begin
            if Block.all'Address /=
               Pool.Storage (I * Pool.Real_Block_Size)'Address or
               Block.all'Address mod Pool_Block'Alignment /= 0
            then
               raise Implementation_Error;
            end if;

            loop
               Block.Next := Pool.Free_List;

               exit when CAS (Target    => Pool.Free_List'Access,
                              Old_Value => Block.Next,
                              New_Value => Block);
            end loop;
         end;
      end loop;

      if Validate (Pool) /= Pool.Pool_Size then
         raise Implementation_Error;
      end if;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Finalize (Pool : in out Lock_Free_Storage_Pool) is
   begin
      Primitives.Membar;
      Pool.Free_List := null;
      Free (Pool.Storage);
   end Finalize;

end Lock_Free_Fixed_Size_Storage_Pools;
