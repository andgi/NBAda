-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : hazard_pointers.adb
-- Description     : Ada implementation of Maged Michael's Hazard Pointers.
-- Author          : Anders Gidenstam
-- Created On      : Thu Nov 25 18:35:09 2004
-- $Id: nbada-hazard_pointers.adb,v 1.1 2004/11/25 22:56:53 anders Exp $
-------------------------------------------------------------------------------

with Primitives;
with Hash_Tables;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

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

   Hazard_Pointer : array (Processes, HP_Index) of aliased Shared_Reference;
   pragma Volatile (Hazard_Pointer);
   pragma Atomic_Components (Hazard_Pointer);

   --  Static process local storage.
   D_List  : array (Processes) of Node_Access;
   D_Count : array (Processes) of Node_Count := (others => 0);

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
   begin
      --  Snapshot all hazard pointers.
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Node := Node_Access (Hazard_Pointer (P, I));
            if Node /= null then
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
