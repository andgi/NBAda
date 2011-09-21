-------------------------------------------------------------------------------
--  Hazard Pointers - An implementation of Maged Michael's hazard pointers.
--  Copyright (C) 2011  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-hazard_pointers.ads
--  Description     : Lock-free Ada implementation of Maged Michael's
--                    Hazard Pointers for safe memory management.
--                    Based on Maged Michael, "Hazard Pointers: Safe Memory
--                    Reclamation for Lock-Free Objects", IEEE Transactions on
--                    Parallell and Distributed Systems, 15(6), 491--504,
--                    June 2004.
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;
with NBAda.Memory_Reclamation.Reference_Operations;
with NBAda.Per_Task_Storage.Shared;
with NBAda.Per_Task_Storage.Local;
with NBAda.Internals.Hash_Tables;

generic
   Max_Number_Of_Dereferences : Natural;
   --  Maximum number of simultaneously dereferenced links per thread.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

   Integrity_Checking : Boolean := False;
   --  Enable strong integrity checking.
   Verbose_Debug : Boolean := False;
   --  Enable verbose debug output.
package NBAda.Memory_Reclamation.Hazard_Pointers is

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
      --  atomic by 'pragma Atomic (Variable_Name);'.

   package Reference_Operations is

      type Node_Access is access all Managed_Node;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Node_Access.

      type Memory_Manager is limited private;

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
      --  Tag the node for rescanning to make sure that hazard pointers to
      --  it that have moved are detected. This is an extension over the
      --  original algorithm.

      procedure Store   (Link : access Shared_Reference;
                         Node : in     Private_Reference);
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

      procedure Scan (MM : in Memory_Manager);
      procedure Validate (Node  : in Private_Reference;
                          Where : in String);

      type Memory_Manager_Access is access all Memory_Manager;

      subtype Index is Natural range 0 .. Max_Number_Of_Dereferences;
      type Private_Reference is
        new Basic_Reference_Operations.Private_Reference_Base with
         record
            HP  : Index := 0;
            MM  : Memory_Manager_Access;
         end record;

      type HP_Index   is new Integer range 1 .. Max_Number_Of_Dereferences;
      type Node_Count is new Primitives.Standard_Unsigned;

      function Hash_Ref (Ref  : in Node_Access;
                         Size : in Natural) return Natural;
      type Hazard_Pointer_Array is array (HP_Index) of
        aliased Node_Access;
      pragma Atomic_Components (Hazard_Pointer_Array);

      --  Persistent shared per-task variables.
      type Task_Shared is
         record
            Hazard_Pointer : Hazard_Pointer_Array;
            pragma Volatile (Hazard_Pointer);
         end record;

      package TSS is new NBAda.Per_Task_Storage.Shared (Task_Shared,
                                                        Process_Ids);

      package P_Sets is
         new NBAda.Internals.Hash_Tables (Node_Access, "=", Hash_Ref);
      subtype P_Set_Type is P_Sets.Hash_Table
        (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
                              Max_Number_Of_Dereferences) + 1);

      --  Task local static data.
      type Task_Local is
        record
           D_List  : Node_Access;
           D_Count : Node_Count   := 0;
           P_Set   : P_Set_Type;
        end record;

      package TLS is new NBAda.Per_Task_Storage.Local (Task_Local,
                                                       Process_Ids);

      type Mutable_View (Self : access Memory_Manager) is
         limited null record;

      type Memory_Manager is limited
        record
           Shared    : TSS.Storage;
           Local     : TLS.Storage;
           --  Shared statistics.
           Reclaimed : aliased Primitives.Standard_Unsigned := 0;
           pragma Atomic (Reclaimed);
           Created   : aliased Primitives.Standard_Unsigned := 0;
           pragma Atomic (Created);
           Mutable   : Mutable_View (Memory_Manager'Access);
        end record;

   end Reference_Operations;
   ----------------------------------------------------------------------------

private

   type Managed_Node_Access is access all Managed_Node_Base'Class;
   --  Note: There SHOULD NOT be any shared variables of type
   --        Managed_Node_Access.

   MM_Live      : constant := 12121212;
   MM_Deleted   : constant := 21212121;
   MM_Reclaimed : constant := 88888888;

   type Managed_Node_Base is abstract tagged limited
      record
         MM_Next   : aliased Managed_Node_Access;
         pragma Atomic (MM_Next);
         MM_Rescan : Boolean := False;
         pragma Atomic (MM_Rescan);
         MM_Magic  : Primitives.Unsigned_32 := MM_Live;
         pragma Atomic (MM_Magic);
         --  NOTE: MM_Magic is only used when Integrity_Checking is set.
      end record;

end NBAda.Memory_Reclamation.Hazard_Pointers;
