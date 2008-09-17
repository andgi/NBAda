-------------------------------------------------------------------------------
--  Hazard Pointers - An implementation of Maged Michael's hazard pointers.
--  Copyright (C) 2004 - 2008  Anders Gidenstam
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
--  $Id: nbada-hazard_pointers.adb,v 1.22.2.1 2008/09/17 22:34:25 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Internals.Hash_Tables;
with NBAda.Internals.Cleanup_Tools;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;
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
   function  Image (Node : in Managed_Node_Access) return String;
   procedure Reclaim_All;
   --  Reclaim all reclaimable nodes in the deletion lists of all tasks.
   --  NOTE: Not thread-safe.

   package HP_Sets is
      new NBAda.Internals.Hash_Tables (Managed_Node_Access, "=", Hash_Ref);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Persistent shared variables.
   type Hazard_Pointer_Array is array (HP_Index) of
     aliased Managed_Node_Access;
   pragma Volatile (Hazard_Pointer_Array);
   pragma Atomic_Components (Hazard_Pointer_Array);
   type Persistent_Shared is
      record
         Hazard_Pointer : Hazard_Pointer_Array;
      end record;
   type Persistent_Shared_Access is access Persistent_Shared;

   Persistent_Shared_Variables : constant array (Processes) of
     Persistent_Shared_Access := (others => new Persistent_Shared);
   --  FIXME: Free these during finalization of the package.

   --  Process local static data.
   D_List  : array (Processes) of Managed_Node_Access;
   D_Count : array (Processes) of Node_Count := (others => 0);

   --  Shared statistics.
   Reclaimed : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Reclaimed);
   Created   : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Created);

   --  The P_Sets are preallocated from the heap as it can easily become
   --  too large to fit on the task stack.
   type HP_Set_Access is access HP_Sets.Hash_Table;
   P_Set : constant array (Processes) of HP_Set_Access :=
     (others => new HP_Sets.Hash_Table
      (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
                            Max_Number_Of_Dereferences) + 1));
   --  FIXME: Free these during finalization of the package.

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Release     (Local  : in Managed_Node_Access) is
      ID : constant Processes := Process_Ids.Process_ID;
      PS : Persistent_Shared renames Persistent_Shared_Variables (ID).all;
   begin
      if Local /= null then
         --  Find and reset hazard pointer.
         Primitives.Membar;
         --  Complete all preceding memory operations before releasing
         --  the hazard pointer.
         for I in PS.Hazard_Pointer'Range loop
            if PS.Hazard_Pointer (I) = Local then
               PS.Hazard_Pointer (I) := null;
               return;
            end if;
         end loop;
         --  Not found.
         if Integrity_Checking then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Released a private references that had not " &
               "been dereferenced!");
         end if;
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
         PS    : Persistent_Shared renames
           Persistent_Shared_Variables (ID).all;
         Index : HP_Index;
         Found : Boolean := False;
         Node  : Node_Access;
      begin
         --  Find a free hazard pointer.
         for I in PS.Hazard_Pointer'Range loop
            if PS.Hazard_Pointer (I) = null then
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
            if Integrity_Checking then
               declare
                  use type Primitives.Unsigned_32;
                  State : Primitives.Unsigned_32;
               begin
                  loop
                     Node := Node_Access (Shared.all);
                     PS.Hazard_Pointer (Index) := Managed_Node_Access (Node);
                     if Node /= null then
                        State := Node.MM_Magic;
                     end if;

                     Primitives.Membar;
                     --  The write to the hazard pointer must be visible
                     --  before Link is read again.
                     exit when Node_Access (Shared.all) = Node;
                  end loop;
                  if Node /= null and then State /= MM_Live then
                     Ada.Exceptions.Raise_Exception
                       (Constraint_Error'Identity,
                        "hazard_pointers.adb: " &
                        "Dereferenced a non-exsisting node!");
                  end if;
               end;
            else
               loop
                  Node := Node_Access (Shared.all);
                  PS.Hazard_Pointer (Index) := Managed_Node_Access (Node);

                  Primitives.Membar;
                  --  The write to the hazard pointer must be visible before
                  --  Link is read again.
                  exit when Node_Access (Shared.all) = Node;
               end loop;
            end if;
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
         if Local = null then
            return;
         end if;

         Release (Local);
         Managed_Node_Base (Local.all).MM_Next  := D_List (ID);
         D_List  (ID)      := Managed_Node_Access (Local);
         D_Count (ID)      := D_Count (ID) + 1;

         if Integrity_Checking then
            Managed_Node_Base (Local.all).MM_Magic := MM_Deleted;
         end if;

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
         new Ada.Unchecked_Conversion (Shared_Reference,
                                       Private_Reference_Impl);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Private_Reference_Impl);

      type Shared_Reference_Access is access all Shared_Reference;
      type Shared_Reference_Base_Access is access all
        Shared_Reference_Base_Impl;
      function To_Shared_Reference_Base_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Shared_Reference_Base_Access);

      ----------------------------------------------------------------------
      function Boolean_Compare_And_Swap_Impl is new
        Primitives.Standard_Boolean_Compare_And_Swap
        (Shared_Reference_Base_Impl);
      procedure Void_Compare_And_Swap_Impl is new
        Primitives.Standard_Void_Compare_And_Swap
        (Shared_Reference_Base_Impl);

      ----------------------------------------------------------------------
      function Image (R : Private_Reference) return String is
         type Node_Access is access all Managed_Node_Base'Class;
      begin
         if Deref (R) /= null then
            return
              "(" &
              Ada.Tags.External_Tag (Node_Access (Deref (R)).all'Tag) & "@" &
              Private_Reference_Impl'Image (R.Ref) & ", HP[" &
              Integer'Image (R.HP) & "])";

         else
            return "(" &
              "@" & Private_Reference_Impl'Image (R.Ref) & ", HP[" &
              Integer'Image (R.HP) & "])";
         end if;
      exception
         when Storage_Error =>
            return "(" &
              "@" & Private_Reference_Impl'Image (R.Ref) & ", HP[" &
              Integer'Image (R.HP) & "])";
      end Image;

      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference is
         ID    : constant Processes := Process_Ids.Process_ID;
         PS    : Persistent_Shared renames
           Persistent_Shared_Variables (ID).all;
         Index : HP_Index;
         Found : Boolean := False;
         Node  : Private_Reference;
      begin
         --  Find a free hazard pointer.
         for I in PS.Hazard_Pointer'Range loop
            if PS.Hazard_Pointer (I) = null then
               --  Found a free hazard pointer.
               Index   := I;
               Node.HP := Natural (I);
               Found   := True;
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
            if Integrity_Checking then
               declare
                  use type Primitives.Unsigned_32;
                  State : Primitives.Unsigned_32 := 0;
               begin
                  loop
                     Node.Ref := To_Private_Reference (Link.all);
                     PS.Hazard_Pointer (Index) :=
                       Managed_Node_Access (Deref (Node));

                     if Deref (Node) /= null then
                        if Node.Ref = 16#ffffffff# then
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "hazard_pointers.adb: " &
                              "Dereference found a nonsense reference value." &
                              Image (Node));
                        else
                           State := Deref (Node).MM_Magic;
                        end if;
                     end if;

                     Primitives.Membar;
                     --  The write to the hazard pointer must be visible
                     --  before Link is read again.
                     exit when To_Private_Reference (Link.all) = Node.Ref;
                  end loop;
                  if Deref (Node) /= null and then
                    not (State = MM_Live or State = MM_Deleted)  then
                     Ada.Exceptions.Raise_Exception
                       (Constraint_Error'Identity,
                        "hazard_pointers.adb: " &
                        "Dereferenced a non-existing node! " &
                        Image (Node));
                  end if;
               end;
            else
               loop
                  Node.Ref := To_Private_Reference (Link.all);
                  PS.Hazard_Pointer (Index) :=
                    Managed_Node_Access (Deref (Node));

                  Primitives.Membar;
                  --  The write to the hazard pointer must be visible before
                  --  Link is read again.
                  exit when To_Private_Reference (Link.all) = Node.Ref;
               end loop;
            end if;
         end if;

         return Node;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Node : in Private_Reference) is
         ID    : constant Processes := Process_Ids.Process_ID;
         PS    : Persistent_Shared renames
           Persistent_Shared_Variables (ID).all;
      begin
         Primitives.Membar;
         --  Complete all preceding memory operations before releasing
         --  the hazard pointer.
         if "+" (Node) /= null then
            if Integrity_Checking then
               if Node.HP < 1 or Node.HP > Max_Number_Of_Dereferences then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "hazard_pointers.adb: " &
                     "Attempt to release an invalid private reference, " &
                     Image (Node));
               end if;

               if PS.Hazard_Pointer (HP_Index (Node.HP)) =
                 Managed_Node_Access (Deref (Node))
               then
                  PS.Hazard_Pointer (HP_Index (Node.HP)) := null;
               else
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "hazard_pointers.adb: " &
                     "Released a private references that had not " &
                     "been dereferenced! " &
                     Image (Node));
               end if;
            else
               PS.Hazard_Pointer (HP_Index (Node.HP)) := null;
            end if;
         end if;
      end Release;

      ----------------------------------------------------------------------
      function  "+"     (Node : in Private_Reference)
                        return Node_Access renames Deref;

      ----------------------------------------------------------------------
      function  Deref   (Node : in Private_Reference)
                        return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Private_Reference_Impl,
                                          Node_Access);

      begin
         return To_Node_Access (Node.Ref and Ref_Mask);
      end Deref;

      ----------------------------------------------------------------------
      function Compare_And_Swap (Link      : access Shared_Reference;
                                 Old_Value : in Private_Reference;
                                 New_Value : in Private_Reference)
                                return Boolean is
      begin
         if Integrity_Checking then
            if New_Value.Ref = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "hazard_pointers.adb: " &
                  "Error: Attempting to CAS a nonsense reference value!");
            end if;
         end if;
         return Boolean_Compare_And_Swap_Impl
           (Target =>
              To_Shared_Reference_Base_Access (Link.all'Unchecked_Access),
            Old_Value => Shared_Reference_Base_Impl (Old_Value.Ref),
            New_Value => Shared_Reference_Base_Impl (New_Value.Ref));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference) is
      begin
         if Integrity_Checking then
            if New_Value.Ref = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "hazard_pointers.adb: " &
                  "Error: Attempting to CAS a nonsense reference value!");
            end if;
         end if;
         Void_Compare_And_Swap_Impl
           (Target =>
              To_Shared_Reference_Base_Access (Link.all'Unchecked_Access),
            Old_Value => Shared_Reference_Base_Impl (Old_Value.Ref),
            New_Value => Shared_Reference_Base_Impl (New_Value.Ref));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Delete      (Node : in Private_Reference) is
         ID : constant Processes := Process_Ids.Process_ID;
         Deleted : constant Node_Access := Deref (Node);
         use type Primitives.Unsigned_32;
      begin
         if Deref (Node) = null then
            return;
         end if;

         if Integrity_Checking then
            if Deleted.MM_Magic /= MM_Live then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "hazard_pointers.adb: " &
                  "Error: Deleting an already deleted or non-existing node! " &
                  Image (Node));
            end if;
            Managed_Node_Base (Deleted.all).MM_Magic := MM_Deleted;
         end if;

         Release (Node);
         Managed_Node_Base (Deleted.all).MM_Next := D_List (ID);
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
         type Private_Reference_Access is access all Private_Reference_Impl;
         function To_Private_Reference_Access is
            new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                          Private_Reference_Access);
      begin
         if Integrity_Checking then
            if Node.Ref = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Error: Storing a nonsense reference value! " &
               Image (Node));
            end if;
         end if;

         To_Private_Reference_Access (Shared_Reference_Access (Link)).all :=
           Node.Ref;
      end Store;

      ----------------------------------------------------------------------
      function Create return Private_Reference is
         ID    : constant Processes        := Process_Ids.Process_ID;
         PS    : Persistent_Shared renames
           Persistent_Shared_Variables (ID).all;
         Index : HP_Index;
         Found : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in PS.Hazard_Pointer'Range loop
            if PS.Hazard_Pointer (I) = null then
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
               PS.Hazard_Pointer (Index) :=
                 Managed_Node_Access (Node);

               Primitives.Fetch_And_Add (Created'Access, 1);

               if Verbose_Debug then
                  Ada.Text_IO.Put_Line ("Created node " &
                                        Image (Managed_Node_Access (Node)));
               end if;

               return (Ref => To_Private_Reference (Node),
                       HP  => Natural (Index));
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
         Node.Ref := Node.Ref or 1;
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return (Ref => Node.Ref or 1, HP => Node.HP);
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node.Ref := Node.Ref and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return (Ref => Node.Ref and Ref_Mask, HP => Node.HP);
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node.Ref and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean is
      begin
         return (To_Private_Reference (Node) and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function "=" (Left, Right : in     Private_Reference) return Boolean is
      begin
         return Left.Ref = Right.Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref.Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref.Ref;
      end "=";

   end Reference_Operations;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
      use type Primitives.Standard_Unsigned;

      function Count_Unreclaimed return Primitives.Standard_Unsigned;
      function Count_HPs_Set return Natural;

      -----------------------------------------------------------------
      function Count_Unreclaimed return Primitives.Standard_Unsigned is
         Count : Primitives.Standard_Unsigned := 0;
      begin
         --  Note: This is not thread safe.
         for P in D_Count'Range loop
            Count := Count + Primitives.Standard_Unsigned (D_Count (P));
         end loop;
         return Count;
      end Count_Unreclaimed;

      -----------------------------------------------------------------
      function Count_HPs_Set return Natural is
         Count : Natural := 0;
      begin
         --  Note: This is not thread safe.
         for P in Persistent_Shared_Variables'Range loop
            for I in Persistent_Shared_Variables (P).Hazard_Pointer'Range loop
               if
                 Persistent_Shared_Variables (P).Hazard_Pointer (I) /= null
               then
                  Count := Count + 1;
               end if;
            end loop;
         end loop;
         return Count;
      end Count_HPs_Set;

      -----------------------------------------------------------------
   begin
      Ada.Text_IO.Put_Line ("Hazard_Pointers.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Created = " &
                            Primitives.Standard_Unsigned'Image (Created));
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Standard_Unsigned'Image (Reclaimed));
      Ada.Text_IO.Put_Line ("  #Awaiting reclamation = " &
                            Primitives.Standard_Unsigned'Image
                            (Count_Unreclaimed));
      Ada.Text_IO.Put_Line ("  #Not accounted for = " &
                            Primitives.Standard_Unsigned'Image
                            (Created - Reclaimed - Count_Unreclaimed));
      Ada.Text_IO.Put_Line ("  #Hazard pointers set = " &
                            Integer'Image (Count_HPs_Set));
   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan (ID : in Processes) is
      use HP_Sets;
      use type Primitives.Unsigned_32;

      New_D_List  : Managed_Node_Access;
      New_D_Count : Node_Count          := 0;
      Node        : Managed_Node_Access;
      Ref         : Managed_Node_Access;
   begin
      Clear (P_Set (ID).all);
      --  Snapshot all hazard pointers.
      for P in Processes'Range loop
         for I in HP_Index'Range loop
            Ref := Persistent_Shared_Variables (P).Hazard_Pointer (I);
            if Ref /= null then
               Insert (Ref, P_Set (ID).all);
            end if;
         end loop;
      end loop;

      Primitives.Membar;
      --  Make sure the memory operations of the algorithm's phases are
      --  separated.

      while D_List (ID) /= null loop
         Node        := D_List (ID);

         if Verbose_Debug then
            Ada.Text_IO.Put_Line ("Scanning node " & Image (Node));
         end if;
         if Integrity_Checking then
            begin
               if Node /= null and then Node.MM_Next /= null then
                  --  Just to force Node to be dereferenced.
                  null;
               end if;
            exception
               when Storage_Error =>
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "hazard_pointers.adb: " &
                     "Error: Invalid node pointer (" &
                     Image (Node) &
                     " ) in deletion list!");
            end;
         end if;

         D_List (ID) := Managed_Node_Access (Node.MM_Next);
         if Member (Node, P_Set (ID).all) then
            Node.MM_Next := New_D_List;
            New_D_List   := Node;
            New_D_Count  := New_D_Count + 1;
         else
            --  Reclaim node storage.
            if Integrity_Checking then
               if Node.MM_Magic /= MM_Deleted then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "hazard_pointers.adb: " &
                     "Error: Reclaiming an undeleted node!");
               end if;
               Node.MM_Magic := MM_Reclaimed;
            end if;
            Node.MM_Next  := null;

            if Verbose_Debug then
               Ada.Text_IO.Put_Line ("Reclaiming node " & Image (Node));
            end if;
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

   ----------------------------------------------------------------------------
   function Image (Node : in Managed_Node_Access) return String is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Managed_Node_Access,
                                       Primitives.Standard_Unsigned);
   begin
      return Primitives.Standard_Unsigned'Image (To_Unsigned (Node));
   end Image;

   ----------------------------------------------------------------------------
   procedure Reclaim_All is
   begin
      for P in D_List'Range loop
         Scan (P);
      end loop;
   end Reclaim_All;

   ----------------------------------------------------------------------------
   procedure Finalize;

   procedure Finalize is
   begin
      if Integrity_Checking or Verbose_Debug then
         Print_Statistics;
      end if;
--        Reclaim_All;
--        if Integrity_Checking then
--           Print_Statistics;
--        end if;
   end Finalize;

   type Local_Action is access procedure;
   function Lope_Hole is new Ada.Unchecked_Conversion
     (Local_Action,
      NBAda.Internals.Cleanup_Tools.Action);

   Finally :
     NBAda.Internals.Cleanup_Tools.On_Exit (Lope_Hole (Finalize'Access));
--  NOTE: This is a really really dangerous idea!
--        Finally might be destroyed AFTER the node storage pool is destroyed!

end NBAda.Hazard_Pointers;
