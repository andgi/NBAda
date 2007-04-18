-------------------------------------------------------------------------------
--  Epoch-based memory reclamation.
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : epoch_based_memory_reclamation.adb
--  Description     : Implementation of Keir Fraser's epoch-based memory
--                    reclamation scheme.
--                    See Keir Fraser, "Practical lock-freedom",
--                    Technical Report 579, Computer Laboratory,
--                    University of Cambridge, 2004.
--  Author          : Anders Gidenstam
--  Created On      : Wed Mar  8 12:28:31 2006
--  $Id: nbada-epoch_based_memory_reclamation.adb,v 1.6 2007/04/18 13:38:08 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (Modified_GPL);

with Primitives;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Text_IO;

package body Epoch_Based_Memory_Reclamation is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;

   type Epoch_ID is mod 2**(Primitives.Standard_Unsigned'Size - 1);
   for Epoch_ID'Size use Primitives.Standard_Unsigned'Size - 1;
   type Epoch is
      record
         ID     : Epoch_ID;
         Active : Boolean;
      end record;
   pragma Pack (Epoch);
   for Epoch'Size use Primitives.Standard_Unsigned'Size;
   pragma Atomic (Epoch);

   type Node_Count   is new Primitives.Standard_Unsigned;

   procedure Enter_Critical_Section (ID : in Processes);
   procedure Exit_Critical_Section  (ID : in Processes);

   procedure Update_Global_Epoch;
   procedure Cleanup (ID    : in Processes;
                      Epoch : in Epoch_ID);
   procedure CAS is
      new Primitives.Standard_Void_Compare_And_Swap (Epoch);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Shared static data.
   Global_Epoch  : aliased Epoch := (0, True);
   pragma Atomic (Global_Epoch);

   Current_Epoch : array (Processes) of aliased Epoch :=
     (others => (0, False));
   --  Note:
   pragma Volatile (Current_Epoch);
   pragma Atomic_Components (Current_Epoch);

   --  Process local static data.
   Dereferenced_Count : array (Processes) of Natural := (others => 0);
   D_List             : array (Processes, Epoch_ID range 0 .. 2) of
     Managed_Node_Access;
   CS_Count           : array (Processes) of Node_Count := (others => 0);

   --  Shared statistics.
   Reclaimed : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Reclaimed);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

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
         ID    : constant Processes   := Process_Ids.Process_ID;
         Node  : constant Node_Access := Node_Access (Shared.all);
      begin
         if Node /= null then
            --  Check whether this task is already in a critical section.
            if Dereferenced_Count (ID) = 0 then
               Enter_Critical_Section (ID);
            end if;
            Dereferenced_Count (ID) := Dereferenced_Count (ID) + 1;
         end if;

         return Node;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Local  : in Node_Access) is
         ID    : constant Processes := Process_Ids.Process_ID;
      begin
         if Local /= null then
            if Dereferenced_Count (ID) = 0 then
               Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "epoch_based_memory_reclamation.adb: " &
               "Fatal error: More private references released than " &
               "dereferenced!");
            end if;
            Dereferenced_Count (ID) := Dereferenced_Count (ID) - 1;
            if Dereferenced_Count (ID) = 0 then
               Exit_Critical_Section (ID);
            end if;
         end if;
      end Release;

      ----------------------------------------------------------------------
      procedure Delete      (Local  : in Node_Access) is
         ID    : constant Processes := Process_Ids.Process_ID;
         Epoch : constant Epoch_ID  := Current_Epoch (ID).ID mod 3;
      begin
         Release (Local);
         Managed_Node_Base (Local.all).MM_Next :=
           Shared_Reference_Base (D_List (ID, Epoch));
         D_List  (ID, Epoch) := Managed_Node_Access (Local);
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
   --  Reference Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   package body  Reference_Operations is

      ----------------------------------------------------------------------
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Shared_Reference, Private_Reference);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access, Private_Reference);

      ----------------------------------------------------------------------
      function To_Shared_Reference is
         new Ada.Unchecked_Conversion (Private_Reference, Shared_Reference);

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
         ID   : constant Processes := Process_Ids.Process_ID;
         Node : constant Private_Reference := To_Private_Reference (Link.all);
      begin
         if Deref (Node) /= null then
            --  Check whether this task is already in a critical section.
            if Dereferenced_Count (ID) = 0 then
               Enter_Critical_Section (ID);
            end if;
            Dereferenced_Count (ID) := Dereferenced_Count (ID) + 1;
         end if;

         return Node;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
         ID    : constant Processes := Process_Ids.Process_ID;
      begin
         if "+"(Node) /= null then
            if Dereferenced_Count (ID) = 0 then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "epoch_based_memory_reclamation.adb: " &
                  "Fatal error: More private references released than " &
                  "dereferenced!");
            end if;
            Dereferenced_Count (ID) := Dereferenced_Count (ID) - 1;
            if Dereferenced_Count (ID) = 0 then
               Exit_Critical_Section (ID);
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
      procedure Delete  (Node : in Private_Reference) is
         ID      : constant Processes := Process_Ids.Process_ID;
         Epoch   : constant Epoch_ID  := Current_Epoch (ID).ID mod 3;
         Deleted : constant Node_Access := Deref (Node);
      begin
         Release (Node);
         Managed_Node_Base (Deleted.all).MM_Next :=
           Shared_Reference_Base (D_List (ID, Epoch));
         D_List  (ID, Epoch) := Managed_Node_Access (Deleted);
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
         UNode : constant User_Node_Access := new Managed_Node;
         Node  : constant Node_Access      := UNode.all'Unchecked_Access;
      begin
         --  Check whether this task is already in a critical section.
         if Dereferenced_Count (ID) = 0 then
            Enter_Critical_Section (ID);
         end if;
         Dereferenced_Count (ID) := Dereferenced_Count (ID) + 1;

         return To_Private_Reference (Node);
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

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line
        ("Epoch_Based_Memory_Reclamation.Print_Statistics:");
      Ada.Text_IO.Put_Line
        ("  #Reclaimed = " &
         Primitives.Standard_Unsigned'Image (Reclaimed));
      Ada.Text_IO.Put_Line
        ("  #Epochs = " &
         Epoch_ID'Image (Global_Epoch.ID));
   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Enter_Critical_Section (ID : in Processes) is
   begin
      --  Look for epoch change.
--      Ada.Text_IO.Put ("l");
      declare
         Last_Epoch_ID : constant Epoch_ID := Current_Epoch (ID).ID;
      begin
         Current_Epoch (ID) := Global_Epoch;
         Primitives.Membar;
         if Current_Epoch (ID).ID /= Last_Epoch_ID then
            --  Clean up the delete list of the epoch before
            --  Last_Epoch.
            Cleanup (ID, Last_Epoch_ID - 1);
            if Last_Epoch_ID /= Current_Epoch (ID).ID - 1 then
               --  Last epoch is also safe to clean.
               Cleanup (ID, Last_Epoch_ID);
               null;
            end if;
            CS_Count (ID) := 0;
         end if;

         --  Attempt to update the global epoch.
         --  The above step should probably be redone if successful.
         if Natural (CS_Count (ID)) < Epoch_Update_Threshold then
            CS_Count (ID) :=  CS_Count (ID) + 1;
         else
            Update_Global_Epoch;
         end if;
      end;
   end Enter_Critical_Section;

   ----------------------------------------------------------------------------
   procedure Exit_Critical_Section  (ID : in Processes) is
   begin
      --  Look for epoch change.
--      Ada.Text_IO.Put ("u");
      declare
         Last_Epoch_ID : constant Epoch_ID := Current_Epoch (ID).ID;
      begin
         Primitives.Membar;
         --  Clear the active flag.
         Current_Epoch (ID) := (Global_Epoch.ID, False);

         if Current_Epoch (ID).ID /= Last_Epoch_ID then
            --  Clean up the delete list of the epoch before
            --  Last_Epoch.
            Cleanup (ID, Last_Epoch_ID - 1);
            if Last_Epoch_ID /= Current_Epoch (ID).ID - 1 then
               --  Last epoch is also safe to clean.
               Cleanup (ID, Last_Epoch_ID);
               null;
            end if;
            CS_Count (ID) := 0;
         end if;
      end;
   end Exit_Critical_Section;

   ----------------------------------------------------------------------------
   procedure Update_Global_Epoch is
      Current_Epoch_ID : constant Epoch_ID := Global_Epoch.ID;
   begin
      for P in Current_Epoch'Range loop
         declare
            Current_Epoch_P : constant Epoch := Current_Epoch (P);
         begin
            if Current_Epoch_P.Active and
              Current_Epoch_P.ID /= Current_Epoch_ID
            then
               --  Process P is lagging behind.
               return;
            end if;
         end;
      end loop;

      CAS (Target    => Global_Epoch'Access,
           Old_Value => (Current_Epoch_ID, True),
           New_Value => (Current_Epoch_ID + 1, True));
   end Update_Global_Epoch;

   ----------------------------------------------------------------------------
   procedure Cleanup (ID    : in Processes;
                      Epoch : in Epoch_ID) is
      Node : Managed_Node_Access;
   begin
      while D_List (ID, Epoch mod 3) /= null loop
         Node        := D_List (ID, Epoch mod 3);
         D_List (ID, Epoch mod 3) := Managed_Node_Access (Node.MM_Next);

         --  Reclaim node storage.
         Free (Node);

         Primitives.Fetch_And_Add (Reclaimed'Access, 1);
      end loop;
   end Cleanup;

end Epoch_Based_Memory_Reclamation;
