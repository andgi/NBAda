-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : hash_tables.adb
-- Description     : A simple closed hash table.
-- Author          : Anders Gidenstam
-- Created On      : Thu Nov 25 21:56:38 2004
-- $Id: hash_tables.adb,v 1.1 2004/11/25 22:56:53 anders Exp $
-------------------------------------------------------------------------------

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
