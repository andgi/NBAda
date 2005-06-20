-------------------------------------------------------------------------------
--  NBmalloc based storage pool.
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
--                              -*- Mode: Ada -*-
--  Filename        : nbmalloc_storage_pools.adb
--  Description     : A NBmalloc based storage pool.
--  Author          : Anders Gidenstam
--  Created On      : Mon Jun 20 15:12:54 2005
--  $Id: nbmalloc_storage_pools.adb,v 1.1 2005/06/20 16:34:59 anders Exp $
-------------------------------------------------------------------------------

with Interfaces.C;

package body NBmalloc_Storage_Pools is

   ----------------------------------------------------------------------------
   type Void is null record;
   pragma Convention (C, Void);
   --  C convention void.

   ----------------------------------------------------------------------------
   procedure Allocate
     (Pool                     : in out Lock_Free_Storage_Pool;
      Storage_Address          :    out System.Address;
      Size_In_Storage_Elements : in     System.Storage_Elements.Storage_Count;
      Alignment                : in     System.Storage_Elements.Storage_Count)
   is

      ----------------------------------------------------------------------
      package IC renames Interfaces.C;

      type Void_Pointer is access all Void;
      pragma Convention (C, Void_Pointer);
      --  Corresponds to void * in C.

      ----------------------------------------------------------------------
      function NB_Memalign (Alignment : IC.size_t;
                            Size      : IC.size_t)
                           return Void_Pointer;
      --  void * nb_memalign(size_t alignment, size_t size);
      pragma Import (Convention    => C,
                     Entity        => NB_Memalign,
                     External_Name => "nb_memalign");

   begin
      declare
         X : constant Void_Pointer :=
           NB_Memalign (Alignment => IC.size_t (Alignment),
                        Size      => IC.size_t (Size_In_Storage_Elements));
      begin
         if X /= null then
            Storage_Address := X.all'Address;
         else
            raise Storage_Error;
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
      ----------------------------------------------------------------------
      package IC renames Interfaces.C;

      type Void_Pointer is access all Void;
      pragma Convention (C, Void_Pointer);
      --  Corresponds to void * in C.

      ----------------------------------------------------------------------
      procedure NB_Free (X : Void_Pointer);
      --  void nb_free(void * ptr);
      pragma Import (Convention    => C,
                     Entity        => NB_Free,
                     External_Name => "nb_free");

      X_All : aliased Void;
      for X_All'Address use Storage_Address;
      pragma Import (Ada, X_All);

   begin
      NB_Free (X_All'Access);
   end Deallocate;

   ----------------------------------------------------------------------------
   function Storage_Size (Pool : Lock_Free_Storage_Pool)
                         return System.Storage_Elements.Storage_Count
   is
   begin
      return 0;
   end Storage_Size;

   ----------------------------------------------------------------------------
   procedure Initialize (Pool : in out Lock_Free_Storage_Pool) is
   begin
      null;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Finalize   (Pool : in out Lock_Free_Storage_Pool) is
   begin
      null;
   end Finalize;

end NBmalloc_Storage_Pools;
