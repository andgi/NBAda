-------------------------------------------------------------------------------
--  Per-object per-task shared storage.
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
--  Filename        : nbada-per_task_storage-shared.adb
--  Description     : A simple implementation of per-task shared storage.
--  Author          : Anders Gidenstam
--  Created On      : Tue Aug 02 14:07:00 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

package body NBAda.Per_Task_Storage.Shared is

   ----------------------------------------------------------------------------
   function Get (Source : Storage) return Element_Access is
      ID : constant Process_Ids.Process_ID_Type := Process_Ids.Process_ID;
   begin
      if Source.Element (ID) = null then
         Source.Mutable.Self.Element (ID) := new Element_Type;
      end if;
      return Source.Element (ID);
   end Get;

   ----------------------------------------------------------------------------
   function Get (Source : Storage;
                 Owner  : Process_Ids.Process_ID_Type) return Element_Access is
   begin
      return Source.Element (Owner);
   end Get;

end NBAda.Per_Task_Storage.Shared;
