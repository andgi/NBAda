-------------------------------------------------------------------------------
--  Per-object thread local storage.
--  Copyright (C) 2011  Anders Gidenstam
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-per_thread_storage-local.ads
--  Description     : A simple implementation of thread local storage.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jun 07 19:55:00 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;

generic

   type Element_Type is limited private;
   --  Element type.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Per_Thread_Storage.Local is

   type Element_Access is access Element_Type;

   type Storage is limited private;

   function Get (Source : in Storage) return Element_Access;

private

   type Element_Array is array (Process_Ids.Process_ID_Type) of Element_Access;

   type Mutable_View (Self : access Storage) is
      limited null record;

   type Storage is
      limited record
         Element : Element_Array;
         Mutable : Mutable_View (Storage'Access);
      end record;

end NBAda.Per_Thread_Storage.Local;
