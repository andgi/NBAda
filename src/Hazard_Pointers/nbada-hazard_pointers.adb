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
--  $Id: nbada-hazard_pointers.adb,v 1.19 2007/12/14 15:48:24 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Internals.Hash_Tables;

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
         Release (Local);
         Managed_Node_Base (Local.all).MM_Next  := D_List (ID);
         Managed_Node_Base (Local.all).MM_Magic := MM_Deleted;
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
              Ada.Tags.External_Tag (Node_Access (Deref (R)).all'Tag) & "@" &
              Private_Reference_Impl'Image (R.Ref);
         else
            return "@" & Private_Reference_Impl'Image (R.Ref);
         end if;
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
                              "Dereference found a nonsense reference value.");
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
                        "Dereferenced a non-existing node!");
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
         if Integrity_Checking then
            if PS.Hazard_Pointer (HP_Index (Node.HP)) =
              Managed_Node_Access (Deref (Node))
            then
               PS.Hazard_Pointer (HP_Index (Node.HP)) := null;
            else
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "hazard_pointers.adb: " &
                  "Released a private references that had not " &
                  "been dereferenced!");
            end if;
         else
            PS.Hazard_Pointer (HP_Index (Node.HP)) := null;
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
      function  Boolean_Compare_And_Swap (Link      : access Shared_Reference;
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
      end Boolean_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Void_Compare_And_Swap    (Link      : access Shared_Reference;
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
      end Void_Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Delete      (Node : in Private_Reference) is
         ID : constant Processes := Process_Ids.Process_ID;
         Deleted : constant Node_Access := Deref (Node);
         use type Primitives.Unsigned_32;
      begin
         if Integrity_Checking and then Deleted.MM_Magic /= MM_Live then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Error: Deleting an already deleted or non-existing node!");
         end if;

         Release (Node);
         Managed_Node_Base (Deleted.all).MM_Next  := D_List (ID);
         Managed_Node_Base (Deleted.all).MM_Magic := MM_Deleted;
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

         Old : constant Private_Reference :=
           (Ref => To_Private_Reference (Link.all), HP => 0);
      begin
         if Integrity_Checking then
            if Node.Ref = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "hazard_pointers.adb: " &
               "Error: Storing a nonsense reference value!");
            end if;
         end if;

         To_Private_Reference_Access (Shared_Reference_Access (Link)).all :=
           Node.Ref;

         --  This will crash and burn since Old.HP = 0.
         --  Does it even make sense to delete the old value?
         if "+"(Old) /= null then
            Delete (Old);
         end if;
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
   begin
      Ada.Text_IO.Put_Line ("Hazard_Pointers.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Created = " &
                            Primitives.Standard_Unsigned'Image (Created));
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Standard_Unsigned'Image (Reclaimed));
   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan (ID : in Processes) is
      use HP_Sets;
      use type Primitives.Unsigned_32;

      New_D_List  : Managed_Node_Access := null;
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
         D_List (ID) := Managed_Node_Access (Node.MM_Next);
         if Member (Node, P_Set (ID).all) then
            Node.MM_Next := New_D_List;
            New_D_List   := Node;
            New_D_Count  := New_D_Count + 1;
         else
            --  Reclaim node storage.
            if Node.MM_Magic /= MM_Deleted then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "hazard_pointers.adb: " &
                  "Error: Reclaiming an undeleted node!");
            end if;
            Node.MM_Magic := MM_Reclaimed;
            Node.MM_Next  := null;

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
