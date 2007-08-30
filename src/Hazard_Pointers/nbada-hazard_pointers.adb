-------------------------------------------------------------------------------
--  Hazard Pointers - An implementation of Maged Michael's hazard pointers.
--  Copyright (C) 2004 - 2007  Anders Gidenstam
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
--  $Id: nbada-hazard_pointers.adb,v 1.16 2007/08/30 16:14:05 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Internals.Hash_Tables;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Text_IO;

package body NBAda.Hazard_Pointers is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;
   type    HP_Index  is new Integer range 1 .. Max_Number_Of_Dereferences;

   type Node_Count   is new Primitives.Standard_Unsigned;

   procedure Scan (ID : in Processes);
   function Hash_Ref (Ref  : in Managed_Node_Access;
                      Size : in Natural) return Natural;

   package HP_Sets is
      new NBAda.Internals.Hash_Tables (Managed_Node_Access, "=", Hash_Ref);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Shared static data.
   Hazard_Pointer : array (Processes, HP_Index) of
     aliased Shared_Reference_Base;
   pragma Volatile (Hazard_Pointer);
   pragma Atomic_Components (Hazard_Pointer);

   --  Process local static data.
   D_List  : array (Processes) of Managed_Node_Access;
   D_Count : array (Processes) of Node_Count := (others => 0);

   --  Shared statistics.
   Reclaimed : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Reclaimed);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Release     (Local  : in Managed_Node_Access) is
      ID : constant Processes := Process_Ids.Process_ID;
   begin
      --  Find and reset hazard pointer.
      for I in Hazard_Pointer'Range (2) loop
         if Hazard_Pointer (ID, I) = Shared_Reference_Base (Local) then
            Hazard_Pointer (ID, I) := null;
            return;
         end if;
      end loop;
      --  Not found.
      if Integrity_Checking and Local /= null then
         Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Released a private references that had not " &
               "been dereferenced!");
      end if;
   end Release;

   ----------------------------------------------------------------------------
   package body Operations is

      ----------------------------------------------------------------------
      function Boolean_Compare_And_Swap is
         new Primitives.Standard_Boolean_Compare_And_Swap (Shared_Reference);
      procedure Value_Compare_And_Swap is
         new Primitives.Standard_Compare_And_Swap (Shared_Reference);
      procedure Void_Compare_And_Swap is
         new Primitives.Standard_Void_Compare_And_Swap (Shared_Reference);

      ----------------------------------------------------------------------
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
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            loop
               Node := Node_Access (Shared.all);
               Hazard_Pointer (ID, Index) := Shared_Reference_Base (Node);

               Primitives.Membar;
               --  The write to the hazard pointer must be visible before
               --  Link is read again.
               exit when Node_Access (Shared.all) = Node;
            end loop;
         end if;
         return Node;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Local  : in Node_Access) is
      begin
         Release (Managed_Node_Access (Local));
      end Release;

      ----------------------------------------------------------------------
      procedure Delete      (Local  : in Node_Access) is
         ID : constant Processes := Process_Ids.Process_ID;
      begin
         Release (Local);
         Managed_Node_Base (Local.all).MM_Next :=
           Shared_Reference_Base (D_List (ID));
         D_List  (ID)      := Managed_Node_Access (Local);
         D_Count (ID)      := D_Count (ID) + 1;
         if D_Count (ID) >=
           Node_Count (1.5 * Float (Node_Count (Processes'Last) *
                                    Node_Count (Max_Number_Of_Dereferences)))
         then
            Scan (ID);
         end if;
      end Delete;

      ----------------------------------------------------------------------
      function  Boolean_Compare_And_Swap (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in     Node_Access)
                                         return Boolean is
      begin
         return Boolean_Compare_And_Swap (Shared,
                                          Shared_Reference (Old_Value),
                                          Shared_Reference (New_Value));
      end Boolean_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Value_Compare_And_Swap   (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in out Node_Access) is
      begin
         Value_Compare_And_Swap (Shared,
                                 Shared_Reference (Old_Value),
                                 Shared_Reference (New_Value));
      end Value_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Void_Compare_And_Swap    (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in     Node_Access) is
      begin
         Void_Compare_And_Swap (Shared,
                                Shared_Reference (Old_Value),
                                Shared_Reference (New_Value));
      end Void_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Initialize (Shared    : access Shared_Reference;
                            New_Value : in     Node_Access) is
         Tmp : constant Node_Access := Node_Access (Shared.all);
      begin
         Shared.all := Shared_Reference (New_Value);

         if Tmp /= null then
            Delete (Tmp);
         end if;
      end Initialize;

   end Operations;
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   package body Reference_Operations is

      ----------------------------------------------------------------------
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Shared_Reference, Private_Reference);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access, Private_Reference);

      ----------------------------------------------------------------------
      function Boolean_Compare_And_Swap is
         new Primitives.Standard_Boolean_Compare_And_Swap (Private_Reference);
      procedure Value_Compare_And_Swap is
         new Primitives.Standard_Compare_And_Swap (Private_Reference);
      procedure Void_Compare_And_Swap is
         new Primitives.Standard_Void_Compare_And_Swap (Private_Reference);

      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference is
         ID    : constant Processes := Process_Ids.Process_ID;
         Index : HP_Index;
         Found : Boolean := False;
         Node  : Private_Reference;
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
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            loop
               Node := To_Private_Reference (Link.all);
               Hazard_Pointer (ID, Index) :=
                 Shared_Reference_Base (Deref (Node));

               Primitives.Membar;
               --  The write to the hazard pointer must be visible before
               --  Link is read again.
               exit when To_Private_Reference (Link.all) = Node;
            end loop;
         end if;
         return Node;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Node : in Private_Reference) is
      begin
         Release (Managed_Node_Access (Deref (Node)));
      end Release;

      ----------------------------------------------------------------------
      function  "+"     (Node : in Private_Reference)
                        return Node_Access renames Deref;

      ----------------------------------------------------------------------
      function  Deref   (Node : in Private_Reference)
                        return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Private_Reference,
                                          Node_Access);

      begin
         return To_Node_Access (Node and Ref_Mask);
      end Deref;

      ----------------------------------------------------------------------
      function  Boolean_Compare_And_Swap (Link      : access Shared_Reference;
                                          Old_Value : in Private_Reference;
                                          New_Value : in Private_Reference)
                                         return Boolean is

         type Shared_Reference_Access is access all Shared_Reference;
         type Private_Reference_Access is access all Private_Reference;
         function To_Private_Reference_Access is
            new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                          Private_Reference_Access);

      begin
         return Boolean_Compare_And_Swap
           (Target =>
              To_Private_Reference_Access (Shared_Reference_Access (Link)),
            Old_Value => Old_Value,
            New_Value => New_Value);
      end Boolean_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Void_Compare_And_Swap    (Link      : access Shared_Reference;
                                          Old_Value : in Private_Reference;
                                          New_Value : in Private_Reference) is

         type Shared_Reference_Access is access all Shared_Reference;
         type Private_Reference_Access is access all Private_Reference;
         function To_Private_Reference_Access is
            new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                          Private_Reference_Access);

      begin
         Void_Compare_And_Swap
           (Target =>
              To_Private_Reference_Access (Shared_Reference_Access (Link)),
            Old_Value => Old_Value,
            New_Value => New_Value);
      end Void_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Delete      (Node : in Private_Reference) is
         ID : constant Processes := Process_Ids.Process_ID;
         Deleted : constant Node_Access := Deref (Node);
      begin
         Release (Node);
         Managed_Node_Base (Deleted.all).MM_Next :=
           Shared_Reference_Base (D_List (ID));
         D_List  (ID)      := Managed_Node_Access (Deleted);
         D_Count (ID)      := D_Count (ID) + 1;
         if D_Count (ID) >=
           Node_Count (1.5 * Float (Node_Count (Processes'Last) *
                                    Node_Count (Max_Number_Of_Dereferences)))
         then
            Scan (ID);
         end if;
      end Delete;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is

         type Shared_Reference_Access is access all Shared_Reference;
         type Private_Reference_Access is access all Private_Reference;
         function To_Private_Reference_Access is
            new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                          Private_Reference_Access);

         Tmp : constant Private_Reference := To_Private_Reference (Link.all);
      begin
         To_Private_Reference_Access (Shared_Reference_Access (Link)).all :=
           Node;

         if "+"(Tmp) /= null then
            Delete (Tmp);
         end if;
      end Store;

      ----------------------------------------------------------------------
      function Create return Private_Reference is
         ID    : constant Processes        := Process_Ids.Process_ID;
         Index : HP_Index;
         Found : Boolean := False;
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
         if Found then
            declare
               UNode : constant User_Node_Access := new Managed_Node;
               Node  : constant Node_Access      := UNode.all'Unchecked_Access;
            begin
               Hazard_Pointer (ID, Index) :=
                 Shared_Reference_Base (Node);

               return To_Private_Reference (Node);
            end;
         else
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Maximum number of local dereferences exceeded!");
         end if;
      end Create;

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference) is
      begin
         Node := Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node := Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean is
      begin
         return (To_Private_Reference (Node) and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";

   end Reference_Operations;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line ("Hazard_Pointers.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Standard_Unsigned'Image (Reclaimed));
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
      New_D_List  : Managed_Node_Access := null;
      New_D_Count : Node_Count          := 0;
      Node        : Managed_Node_Access;
      Ref         : Shared_Reference_Base;
   begin
      --  Snapshot all hazard pointers.
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Ref := Hazard_Pointer (P, I);
            if Ref /= null then
               Node := Managed_Node_Access (Ref);
               Insert (Node, P_Set);
            end if;
         end loop;
      end loop;

      while D_List (ID) /= null loop
         Node        := D_List (ID);
         D_List (ID) := Managed_Node_Access (Node.MM_Next);
         if Member (Node, P_Set) then
            Node.MM_Next := Shared_Reference_Base (New_D_List);
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
   function Hash_Ref (Ref  : in Managed_Node_Access;
                      Size : in Natural) return Natural is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Managed_Node_Access,
                                       Primitives.Standard_Unsigned);
      use type Primitives.Standard_Unsigned;
   begin
      return Natural ((To_Unsigned (Ref) / 4) mod
                      Primitives.Standard_Unsigned (Size));
   end Hash_Ref;

end NBAda.Hazard_Pointers;
