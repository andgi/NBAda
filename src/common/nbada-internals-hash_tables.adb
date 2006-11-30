-------------------------------------------------------------------------------
--  Hazard Pointers - An implementation of Maged Michael's hazard pointers.
--  Copyright (C) 2004, 2005  Anders Gidenstam
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
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : hash_tables.adb
--  Description     : A simple closed hash table.
--  Author          : Anders Gidenstam
--  Created On      : Thu Nov 25 21:56:38 2004
--  $Id: nbada-internals-hash_tables.adb,v 1.4 2006/11/30 18:23:09 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

package body Hash_Tables is

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   function Resolve_Hash_Index (Table : in Hash_Table;
                                Key   : in Element_Type)
                               return Hash_Index;

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Clear (Table :    out Hash_Table) is
   begin
      for I in Table.Table'Range loop
         Table.Table (I).Status := Empty;
      end loop;
   end Clear;

   ----------------------------------------------------------------------------
   procedure Insert (Key   : in     Element_Type;
                     Table : in out Hash_Table) is
      Current : constant Hash_Index := Resolve_Hash_Index (Table, Key);
   begin
      if Table.Table (Current).Status /= Valid then
         Table.Table (Current) := (Key, Valid);
      end if;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete (Key   : in     Element_Type;
                     Table : in out Hash_Table) is
      Current : constant Hash_Index := Resolve_Hash_Index (Table, Key);
      Slot    : Hash_Entry renames Table.Table (Current);
   begin
      if Slot.Status = Valid and Slot.Element = Key then
         Table.Table (Current).Status := Deleted;
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function Member (Key   : in Element_Type;
                    Table : in Hash_Table) return Boolean is
      Current : constant Hash_Index := Resolve_Hash_Index (Table, Key);
      Slot    : Hash_Entry renames Table.Table (Current);
   begin
      return Slot.Status = Valid and Slot.Element = Key;
   end Member;

   ----------------------------------------------------------------------------
   procedure Find (Key   : in out Element_Type;
                   Table : in     Hash_Table) is
      Current : constant Hash_Index := Resolve_Hash_Index (Table, Key);
      Slot    : Hash_Entry renames Table.Table (Current);
   begin
      if Slot.Status = Valid and Slot.Element = Key then
         Key := Slot.Element;
      end if;
      raise Item_Not_Found;
   end Find;

   ----------------------------------------------------------------------------
   function Find (Key   : in Element_Type;
                  Table : in Hash_Table) return Element_Type is
      Current : constant Hash_Index := Resolve_Hash_Index (Table, Key);
      Slot    : Hash_Entry renames Table.Table (Current);
   begin
      if Slot.Status = Valid and Slot.Element = Key then
         return Slot.Element;
      end if;
      raise Item_Not_Found;
   end Find;

   ----------------------------------------------------------------------------
   function To_Array (Table : in Hash_Table) return Element_Array is
      Result : Element_Array (1 .. Table.Size);
      N      : Natural := 0;
   begin
      for I in Table.Table'Range loop
         if Table.Table (I).Status = Valid then
            N := N + 1;
            Result (N) :=  Table.Table (I).Element;
         end if;
      end loop;
      return Result (1 .. N);
   end To_Array;


   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Resolve_Hash_Index (Table : in Hash_Table;
                                Key   : in Element_Type)
                               return Hash_Index is
      Current : Hash_Index := Hash (Key, Table.Size);
      I       : Hash_Index := 0;
   begin
      loop
         exit when Table.Table (Current).Status  = Empty;
         exit when Table.Table (Current).Element = Key;

         --  Quadratic hash resolution.

         I := I + 1;
         Current := Current + 2*I - 1;
         if Current >= Table.Size then
            Current := Current - Table.Size;
         end if;
      end loop;

      return Current;
   end Resolve_Hash_Index;

end Hash_Tables;
