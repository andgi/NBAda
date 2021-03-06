-------------------------------------------------------------------------------
--  Lock-free growing storage pool for fixed sized blocks.
--  Copyright (C) 2005 - 2012  Anders Gidenstam
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
--  Filename        : lock_free_growing_storage_pools.adb
--  Description     : A lock-free fixed storage pool implementation.
--  Author          : Anders Gidenstam
--  Created On      : Tue Jun 14 17:46:13 2005
--  $Id: nbada-lock_free_growing_storage_pools.adb,v 1.6 2007/08/30 15:13:13 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with NBAda.Primitives;

package body NBAda.Lock_Free_Growing_Storage_Pools is

   ----------------------------------------------------------------------------
   package LFFSSP renames Lock_Free_Fixed_Size_Storage_Pools;

   function CAS is
      new Primitives.Standard_Boolean_Compare_And_Swap (Element_Pool_Access);

   ----------------------------------------------------------------------------
   procedure Allocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is

      function Allocate_Pool return Element_Pool_Access;
      procedure Add_Pool (New_Pool : Element_Pool_Access;
                          After    : Element_Pool_Access);

      function Allocate_Pool return Element_Pool_Access is
         subtype My_Element_Pool is
           Element_Pool (Pool_Size  => LFFSSP.Block_Count'Last,
                         Block_Size => Pool.Block_Size,
                         Alignment  => Alignment);
         New_Pool : constant Element_Pool_Access :=
           new My_Element_Pool;
      begin
         return New_Pool;
      end Allocate_Pool;

      procedure Add_Pool (New_Pool : Element_Pool_Access;
                          After    : Element_Pool_Access) is
         Current_Pool : Element_Pool_Access := After;
      begin
         loop
            declare
               Old_Next : constant Element_Pool_Access :=
                 Current_Pool.Next;
            begin
               if Old_Next = null then
                  exit when CAS (Current_Pool.Next'Access,
                                 Old_Value => Old_Next,
                                 New_Value => New_Pool);
               else
                  --  There is a new pool already.
                  --  Attatch this one behind it.
                  Current_Pool := Current_Pool.Next;
               end if;
            end;
         end loop;
      end Add_Pool;

      Current_Pool : Element_Pool_Access := Pool.Pool_List;
   begin
      if Current_Pool = null then
         Current_Pool := Allocate_Pool;
         if not CAS (Pool.Pool_List'Access,
                     Old_Value => null,
                     New_Value => Current_Pool)
         then
            Add_Pool (Current_Pool, Pool.Pool_List);
         end if;
      end if;
      loop
         begin
            Allocate (Current_Pool.all,
                      Storage_Address,
                      Size_In_Storage_Elements,
                      Alignment);

            return;

         exception
            when LFFSSP.Storage_Exhausted =>

               if Current_Pool.Next = null then
                  --  Allocate a new pool.
                  Add_Pool (Allocate_Pool, Current_Pool);
               end if;
         end;

         --  Move to the next pool.
         Current_Pool := Current_Pool.Next;
      end loop;
   end Allocate;

   ----------------------------------------------------------------------------
   procedure Deallocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          : in     System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is
      Current_Pool : Element_Pool_Access := Pool.Pool_List;
   begin
      while Current_Pool /= null loop
         if Belongs_To (Current_Pool.all, Storage_Address) then
            Deallocate (Current_Pool.all,
                        Storage_Address,
                        Size_In_Storage_Elements,
                        Alignment);
            return;
         end if;

         --  Move to the next pool.
         Current_Pool := Current_Pool.Next;
      end loop;
      raise Implementation_Error;
   end Deallocate;

   ----------------------------------------------------------------------------
   function Storage_Size (Pool : Lock_Free_Storage_Pool)
                         return System.Storage_Elements.Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return 0;
   end Storage_Size;

   ----------------------------------------------------------------------------
   function Validate (Pool : Lock_Free_Storage_Pool)
                     return Natural is
      pragma Unreferenced (Pool);
   begin
      return 0;
   end Validate;

   ----------------------------------------------------------------------------
   procedure Initialize (Pool : in out Lock_Free_Storage_Pool) is
   begin
      Pool.Pool_List := null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool) is
      procedure Free is new Ada.Unchecked_Deallocation (Element_Pool,
                                                        Element_Pool_Access);

      Current_Pool : Element_Pool_Access := Pool.Pool_List;
   begin
      Pool.Pool_List := null;
      Primitives.Membar;

      while Current_Pool /= null loop
         declare
            Tmp : Element_Pool_Access := Current_Pool;
         begin
            --  Move to the next pool.
            Current_Pool := Current_Pool.Next;

            Free (Tmp);
         end;
      end loop;
   end Finalize;

end NBAda.Lock_Free_Growing_Storage_Pools;
