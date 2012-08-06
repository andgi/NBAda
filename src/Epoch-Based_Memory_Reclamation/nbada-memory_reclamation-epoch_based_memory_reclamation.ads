-------------------------------------------------------------------------------
--  Epoch-based memory reclamation.
--  Copyright (C) 2011 - 2012  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-epoch_based_memory_reclamation.ads
--  Description     : Implementation of Keir Fraser's epoch-based memory
--                    reclamation scheme.
--                    See Keir Fraser, "Practical lock-freedom",
--                    Technical Report 579, Computer Laboratory,
--                    University of Cambridge, 2004.
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;
with NBAda.Memory_Reclamation.Reference_Operations;
with NBAda.Per_Task_Storage.Shared;
with NBAda.Per_Task_Storage.Local;
with NBAda.Configuration;

generic

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

   Epoch_Update_Threshold : Natural := 100;
   --  The number of critical sections entered between attempts to update
   --  the global epoch.

   Integrity_Checking : Boolean := NBAda.Configuration.Integrity_Checking;
   --  Enable strong integrity checking.

package NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation is

   pragma Elaborate_Body (Epoch_Based_Memory_Reclamation);

   ----------------------------------------------------------------------------
   type Managed_Node_Base is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

   procedure Free (Object : access Managed_Node_Base) is abstract;
   --  Note: Due to some peculiarities of the Ada storage pool
   --        management managed nodes need to have a dispatching primitive
   --        operation that calls the instance of Unchecked_Deallocation
   --        appropriate for the specific node type at hand. Without
   --        this the wrong instance of Unchecked_Deallocation might get
   --        called - often with disastrous consequences.
   --        This workaround is not very nice but I have not found any
   --        better way.

   ----------------------------------------------------------------------------
   --  Reference enabled operations on shared references.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   generic

      type Managed_Node is
        new Managed_Node_Base with private;

      type Shared_Reference is new Shared_Reference_Base;
      --  All shared variables of type Shared_Reference MUST be declared
      --  atomic by 'pragma Atomic (Variable_Name);' .

   package Reference_Operations is

      type Node_Access is access all Managed_Node;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Node_Access.

      package Basic_Reference_Operations is
         new NBAda.Memory_Reclamation.Reference_Operations
        (Shared_Reference,
         Managed_Node        => Managed_Node,
         Private_Node_Access => Node_Access);

      type Private_Reference is
        new Basic_Reference_Operations.Private_Reference_Base with private;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Private_Reference.

      function Null_Reference return Private_Reference;
      --  Note: A marked null reference is not equal to Null_Reference.

      function Image (R : Private_Reference) return String;

      type Memory_Manager is limited private;

      ----------------------------------------------------------------------
      --  Operations.
      ----------------------------------------------------------------------
      function  Dereference (MM   : in     Memory_Manager;
                             Link : access Shared_Reference)
                            return Private_Reference;
      --  Note: Dereference preservs any mark on Link.all.
      --        In particular Mark (Null_Reference) /= Null_Reference.

      procedure Release (Node : in Private_Reference);

      function  "+"     (Node : in Private_Reference)
                        return Node_Access;

      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean;

      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference);

      procedure Delete  (Node : in Private_Reference);

      procedure Rescan  (Node : in Private_Reference);
      --  For interface compatibility only. Does nothing.

      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference);
      --  Note: Store is only safe to use when there cannot be any
      --        concurrent updates to Link.

      generic
         type User_Node_Access is access Managed_Node;
         --  Select an appropriate (preferably non-blocking) storage
         --  pool by the "for User_Node_Access'Storage_Pool use ..."
         --  construct.
         --  Note: The nodes allocated in this way must have an
         --        implementation of Free that use the same storage pool.
      function Create (MM : in Memory_Manager) return Private_Reference;
      --  Creates a new User_Node and returns a safe reference to it.

      function "=" (Left, Right : in Private_Reference) return Boolean;

      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Basic_Reference_Operations.
                      Private_Reference_Base'Class) return Boolean
        renames Basic_Reference_Operations."=";
      function "=" (Ref  : in     Basic_Reference_Operations.
                      Private_Reference_Base'Class;
                    Link : in     Shared_Reference) return Boolean
        renames Basic_Reference_Operations."=";
      --  It is possible to compare a reference to the current value of a link.

   private

      pragma No_Strict_Aliasing (Node_Access);

      procedure Validate (Node  : in Private_Reference;
                          Where : in String);

      type Memory_Manager_Access is access all Memory_Manager;
      type Private_Reference is
        new Basic_Reference_Operations.Private_Reference_Base with
         record
            MM : Memory_Manager_Access;
         end record;

      type Epoch_ID is mod 2**(Primitives.Standard_Unsigned'Size - 1);
      for Epoch_ID'Size use Primitives.Standard_Unsigned'Size - 1;
      type Epoch is
         record
            ID     : Epoch_ID;
            Active : Boolean;
         end record;
      pragma Atomic (Epoch);
      pragma Pack (Epoch);
      for Epoch'Object_Size use Primitives.Standard_Unsigned'Size;

      --  Persistent shared per-task data.
      type Task_Shared is
         record
            Current_Epoch : Epoch;
            pragma Atomic (Current_Epoch);
         end record;

      package TSS is new NBAda.Per_Task_Storage.Shared (Task_Shared,
                                                        Process_Ids);

      --  Task local static data.
      type Deleted_List is array (Epoch_ID range 0 .. 2) of Node_Access;
      type Node_Count   is new Primitives.Standard_Unsigned;
      type Task_Local is
        record
           Dereferenced_Count : Natural    := 0;
           D_List             : Deleted_List;
           D_Count            : Natural    := 0;
           CS_Count           : Node_Count := 0;
        end record;

      package TLS is new NBAda.Per_Task_Storage.Local (Task_Local,
                                                       Process_Ids);

      type Mutable_View (Self : access Memory_Manager) is
         limited null record;

      type Memory_Manager is limited
         record
            Global_Epoch : aliased Epoch := (0, True);
            pragma Atomic (Global_Epoch);
            Shared       : TSS.Storage;
            Local        : TLS.Storage;
            --  Shared statistics.
            Reclaimed    : aliased Primitives.Standard_Unsigned := 0;
            pragma Atomic (Reclaimed);
            Created      : aliased Primitives.Standard_Unsigned := 0;
            pragma Atomic (Created);
            Mutable      : Mutable_View (Memory_Manager'Access);
         end record;

   end Reference_Operations;

private

   type Managed_Node_Access is access all Managed_Node_Base'Class;
   --  Note: There SHOULD NOT be any shared variables of type
   --        Managed_Node_Access.

   type Managed_Node_Base is abstract tagged limited
      record
         MM_Next : aliased Managed_Node_Access;
         pragma Atomic (MM_Next);
      end record;

end NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation;
