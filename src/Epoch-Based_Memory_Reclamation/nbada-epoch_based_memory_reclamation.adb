-------------------------------------------------------------------------------
--  Epoch-based memory reclamation.
--  Copyright (C) 2005 - 2006  Anders Gidenstam
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
--                    Technical Report 579, Computer Laboratorym,
--                    University of Cambridge, 2004.
--  Author          : Anders Gidenstam
--  Created On      : Wed Mar  8 12:28:31 2006
--  $Id: nbada-epoch_based_memory_reclamation.adb,v 1.2 2006/03/09 17:23:44 anders Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (Modified_GPL);

with Primitives;

with Ada.Text_IO;

package body Epoch_Based_Memory_Reclamation is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;

   type Epoch_ID is mod 2**31;
   for Epoch_ID'Size use 31;
   type Epoch is
      record
         ID     : Epoch_ID;
         Active : Boolean;
      end record;
   pragma Pack (Epoch);
   for Epoch'Size use 32;
   pragma Atomic (Epoch);

   type Node_Count   is new Primitives.Unsigned_32;

   procedure Update_Global_Epoch;
   procedure Cleanup (ID    : in Processes;
                      Epoch : in Epoch_ID);
   procedure CAS is
      new Primitives.Void_Compare_And_Swap_32 (Epoch);

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
   Reclaimed : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Reclaimed);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   package body Operations is

      ----------------------------------------------------------------------
      function Boolean_Compare_And_Swap is
         new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference);
      procedure Value_Compare_And_Swap is
         new Primitives.Compare_And_Swap_32 (Shared_Reference);
      procedure Void_Compare_And_Swap is
         new Primitives.Void_Compare_And_Swap_32 (Shared_Reference);

      ----------------------------------------------------------------------
      function  Dereference (Shared : access Shared_Reference)
                            return Node_Access is
         ID    : constant Processes := Process_Ids.Process_ID;
      begin
         --  Check whether this task is already in a critical section.
         if Dereferenced_Count (ID) = 0 then
            --  Look for epoch change.
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
               end if;

               --  Attempt to update the global epoch.
               --  The above step should probably be redone if successful.
               if Natural (CS_Count (ID)) < Epoch_Update_Threshold then
                  CS_Count (ID) :=  CS_Count (ID) + 1;
               else
                  Update_Global_Epoch;
                  CS_Count (ID) := 0;
               end if;
            end;
         end if;
         Dereferenced_Count (ID) := Dereferenced_Count (ID) + 1;

         return Node_Access (Shared.all);
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Local  : in Node_Access) is
         ID    : constant Processes := Process_Ids.Process_ID;
      begin
         Dereferenced_Count (ID) := Dereferenced_Count (ID) - 1;
         if Dereferenced_Count (ID) = 0 then
            --  Look for epoch change.
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
               end if;
            end;
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
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line
        ("Epoch_Based_Memory_Reclamation.Print_Statistics:");
      Ada.Text_IO.Put_Line
        ("  #Reclaimed = " &
         Primitives.Unsigned_32'Image (Reclaimed));
      Ada.Text_IO.Put_Line
        ("  #Epochs = " &
         Epoch_ID'Image (Global_Epoch.ID));
   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

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
