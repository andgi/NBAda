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
--  Filename        : hazard_pointers.adb
--  Description     : Lock-Free Ada implementation of Maged Michael's
--                    Hazard Pointers for safe memory management.
--                    Based on Maged Michael, "Hazard Pointers: Safe Memory
--                    Reclamation for Lock-Free Objects", IEEE Transactions on
--                    Parallell and Distributed Systems, 15(6), 491--504,
--                    June 2004.
--  Author          : Anders Gidenstam
--  Created On      : Thu Nov 25 18:35:09 2004
--  $Id: nbada-hazard_pointers.adb,v 1.4 2005/02/25 16:26:04 anders Exp $
-------------------------------------------------------------------------------

with Primitives;
with Hash_Tables;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Ada.Text_IO;

package body Hazard_Pointers is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;
   type    HP_Index  is new Integer range 1 .. Max_Number_Of_Dereferences;

   type Node_Count   is new Primitives.Unsigned_32;

   procedure Scan (ID : in Processes);
   function Hash_Ref (Ref  : in Node_Access;
                      Size : in Natural) return Natural;
   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference);
   procedure Free is new Ada.Unchecked_Deallocation (Managed_Node'Class,
                                                     Node_Access);

   package HP_Sets is new Hash_Tables (Node_Access, "=", Hash_Ref);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Shared static data.
   Hazard_Pointer : array (Processes, HP_Index) of aliased Shared_Reference;
   pragma Volatile (Hazard_Pointer);
   pragma Atomic_Components (Hazard_Pointer);

   --  Process local static data.
   D_List  : array (Processes) of Node_Access;
   D_Count : array (Processes) of Node_Count := (others => 0);

   --  Shared statistics.
   Reclaimed : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Reclaimed);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function  Dereference (Shared : access Shared_Reference)
                         return Node_Access is
      ID    : constant Processes := Process_Ids.Process_ID;
      Index : HP_Index;
      Found : Boolean := False;
      Node  : Node_Access;
   begin
      --  Find a free hazard pointer.
      for I in Hazard_Pointer'Range (2) loop
         if Hazard_Pointer (ID, I) = null then
            --  Found a free hazard pointer.
            Index := I;
            Found := True;
            exit;
         end if;
      end loop;
      --  Dereference node iff there is a free hazard pointer.
      if not Found then
         raise Constraint_Error;
      else
         loop
            Node := Node_Access (Shared.all);
            Hazard_Pointer (ID, Index) := Shared_Reference (Node);
            exit when Node_Access (Shared.all) = Node;
         end loop;
      end if;
      return Node;
   end Dereference;

   ----------------------------------------------------------------------------
   procedure Release     (Local  : in Node_Access) is
      ID : constant Processes := Process_Ids.Process_ID;
   begin
      --  Find and reset hazard pointer.
      for I in Hazard_Pointer'Range (2) loop
         if Hazard_Pointer (ID, I) = Shared_Reference (Local) then
            Hazard_Pointer (ID, I) := null;
            exit;
         end if;
      end loop;
   end Release;

   ----------------------------------------------------------------------------
   procedure Delete      (Local  : in Node_Access) is
      ID : constant Processes := Process_Ids.Process_ID;
   begin
      Release (Local);
      Local.MM_Next := Shared_Reference (D_List (ID));
      D_List  (ID)  := Local;
      D_Count (ID)  := D_Count (ID) + 1;
      if D_Count (ID) <= (Node_Count (1.5) * Node_Count (Processes'Last) *
                          Node_Count (Max_Number_Of_Dereferences))
      then
         Scan (ID);
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function  Compare_And_Swap (Shared    : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean is
   begin
      return Compare_And_Swap (Shared,
                               Shared_Reference (Old_Value),
                               Shared_Reference (New_Value));
   end Compare_And_Swap;

   ----------------------------------------------------------------------------
   procedure Initialize (Shared    : access Shared_Reference;
                         New_Value : in     Node_Access) is
      Tmp : constant Node_Access := Node_Access (Shared.all);
   begin
      Shared.all := Shared_Reference (New_Value);

      if Tmp /= null then
         Delete (Tmp);
      end if;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line ("Hazard_Pointers.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Unsigned_32'Image (Reclaimed));
   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan (ID : in Processes) is
      use HP_Sets;

      P_Set : HP_Sets.Hash_Table
        (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
                              Max_Number_Of_Dereferences) - 1);
      New_D_List  : Node_Access := null;
      New_D_Count : Node_Count  := 0;
      Node        : Node_Access;
      Ref         : Shared_Reference;
   begin
      --  Snapshot all hazard pointers.
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Ref := Hazard_Pointer (P, I);
            if Ref /= null then
               Node := Node_Access (Ref);
               Insert (Node, P_Set);
            end if;
         end loop;
      end loop;

      while D_List (ID) /= null loop
         Node        := D_List (ID);
         D_List (ID) := Node_Access (Node.MM_Next);
         if Member (Node, P_Set) then
            Node.MM_Next := Shared_Reference (New_D_List);
            New_D_List   := Node;
            New_D_Count  := New_D_Count + 1;
         else
            --  Reclaim node storage.
            Free (Node);

            Primitives.Fetch_And_Add (Reclaimed'Access, 1);
         end if;
      end loop;
      D_List  (ID) := New_D_List;
      D_Count (ID) := New_D_Count;
   end Scan;

   ----------------------------------------------------------------------------
   function Hash_Ref (Ref  : in Node_Access;
                      Size : in Natural) return Natural is
      type Unsigned is mod 2**32;
      function To_Unsigned is new Ada.Unchecked_Conversion (Node_Access,
                                                            Unsigned);
   begin
      return Natural ((To_Unsigned (Ref)/4) mod Unsigned (Size));
   end Hash_Ref;

end Hazard_Pointers;
