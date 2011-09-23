-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
--  Copyright (C) 2004 - 2011  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-beware_and_cleanup.ads
--  Description     : Ada implementation of the lock-free garbage reclamation
--                    Scheme from "Efficient and Reliable Lock-Free Memory
--                    Reclamation Based on Reference Counting",
--                    Anders Gidenstam, Marina Papatriantafilou,
--                    Håkan Sundell and Philippas Tsigas,
--                    Proceedings of the 8th International Symposium on
--                    Parallel Architectures, Algorithms and Networks (I-SPAN),
--                    pages 202 - 207, IEEE Computer Society, 2005.
--  Author          : Anders Gidenstam
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

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

   Max_Number_Of_Links_Per_Node : Natural;
   --  Maximum number of links in a shared node.

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

   Max_Delete_List_Size         : Natural :=
     Process_Ids.Max_Number_Of_Processes ** 2 *
       (Max_Number_Of_Dereferences + Max_Number_Of_Links_Per_Node +
        Max_Number_Of_Links_Per_Node + 1);
   --  Note: Do not change Max_Delete_List_Size unless you really know what
   --        you are doing! The bound is derived in the paper.

   Clean_Up_Threshold           : Natural := Max_Delete_List_Size;
   --  The threshold on the delete list size for Clean_Up to be done.

   Scan_Threshold               : Natural := Clean_Up_Threshold;
   --  The threshold on the delete list size for Scan to be done.

   Integrity_Checking : Boolean := False;
   --  Enable strong integrity checking.

   Collect_Statistics : Boolean := True;
   --  Enable some statics gathering.

package NBAda.Memory_Reclamation.Beware_And_Cleanup is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   type Managed_Node_Base is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

   type Memory_Manager_Base is abstract tagged limited null record;

   procedure Dispose  (Node       : access Managed_Node_Base;
                       Concurrent : in     Boolean) is abstract;
   --  Dispose should set all shared references inside the node to null.

   procedure Clean_Up (MM   : in     Memory_Manager_Base'Class;
                       Node : access Managed_Node_Base) is abstract;
   --  Clean_Up should make sure that none of the shared references
   --  inside the node points to a node that was deleted at the point
   --  in time when Clean_Up was called.

   function Is_Deleted (Node : access Managed_Node_Base)
                       return Boolean;
   --  Returns true if Delete (see below) has been called on the node.

   procedure Free (Object : access Managed_Node_Base) is abstract;
   --  Note: Due to some peculiarities of the Ada storage pool
   --        management managed nodes need to have a dispatching primitive
   --        operation that calls the instance of Unchecked_Deallocation
   --        appropriate for the specific node type at hand. Without
   --        this the wrong instance of Unchecked_Deallocation might get
   --        called - often with disastrous consequences as it tries return
   --        the memory to the wrong storage pool.
   --        This workaround is not very nice but I have not found any
   --        better way.

   --  For type separation between shared references to different
   --  managed types derive your own shared reference types from
   --  Shared_Reference_Base and instantiate the memory management
   --  operation package below for each of them.

   type Shared_Reference_Base_Access is access all Shared_Reference_Base;
   type Reference_Set is array (Integer range <>) of
     Shared_Reference_Base_Access;
   --  These two types are defined for compatibility with the
   --  Lock_Free_Reference_Counting package.

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

      type Memory_Manager is new Memory_Manager_Base with private;

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
      function  Dereference (MM   : in     Memory_Manager'Class;
                             Link : access Shared_Reference)
                            return Private_Reference;

      procedure Release (Node : in Private_Reference);

      function  "+"     (Node : in Private_Reference)
                        return Node_Access;

      function  Copy (Node : in Private_Reference) return Private_Reference;
      --  Creates a new Private Reference to Node. Both will need to be
      --  released.

      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean;

      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Private_Reference;
                                  New_Value : in     Private_Reference);

      procedure Delete  (Node : in Private_Reference);

      procedure Rescan  (Node : in Private_Reference);
      --  For API compatability with
      --  NBAda.Memory_Reclamation.Reference_Operations.

      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference);

      generic
         type User_Node_Access is access Managed_Node;
         --  Select an appropriate (preferably non-blocking) storage
         --  pool by the "for User_Node_Access'Storage_Pool use ..."
         --  construct.
         --  Note: The nodes allocated in this way must have an
         --        implementation of Free that use the same storage pool.
      function Create (MM : in Memory_Manager) return Private_Reference;
      --  Creates a new User_Node and returns a safe reference to it.

      --  Private (and shared) references can be tagged with a mark using
      --  the Reference_Mark_Operations package in Basic_Reference_Operations.
      --  NOTE: A marked Null_Reference is not equal (=) to an unmarked.

      function "=" (Left, Right : in     Private_Reference) return Boolean;

      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Basic_Reference_Operations.
                      Private_Reference_Base'Class) return Boolean
        renames Basic_Reference_Operations."=";
      function "=" (Ref  : in     Basic_Reference_Operations.
                      Private_Reference_Base'Class;
                    Link : in     Shared_Reference) return Boolean
        renames Basic_Reference_Operations."=";
      --  It is possible to compare a reference to the current value of a link.


      ------------------------------------------------------------------------
      --  Unsafe operations inherited from the generic interface class.
      --  These SHOULD only be used when the user algorithm guarantees
      --  the absence of premature reclamation or ABA-problems.
      --  In such algorithms the use of these operations in some particular
      --  situations could allow some performance improving optimizations.
      ------------------------------------------------------------------------

      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in Private_Reference)
        return Boolean;
      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Private_Reference;
         New_Value : in Basic_Reference_Operations.Unsafe_Reference_Value)
        return Boolean;
      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in Basic_Reference_Operations.
         Unsafe_Reference_Value)
        return Boolean;
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in     Private_Reference);
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Private_Reference;
         New_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value);
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value);
      --  These operations are overrided to manage the reference count
      --  correctly.

   private

      pragma No_Strict_Aliasing (Node_Access);

      -------------------------------------------------------------------------
      --  Types.
      -------------------------------------------------------------------------
      type Memory_Manager_Access is access all Memory_Manager;

      subtype Index is Natural range 0 .. Max_Number_Of_Dereferences;
      type Private_Reference is
        new Basic_Reference_Operations.Private_Reference_Base with
         record
            HP  : Index := 0;
            MM  : Memory_Manager_Access;
         end record;

      subtype HP_Index is Index range 1 .. Max_Number_Of_Dereferences;
      type    Node_Index is new Natural range 0 .. Max_Delete_List_Size;
      subtype Valid_Node_Index is
        Node_Index range 1 .. Node_Index (Max_Delete_List_Size);

      type Managed_Node_Access is
        access all Managed_Node_Base'Class;
      subtype Atomic_Node_Access is Managed_Node_Access;

      subtype Node_Count is Natural;
      type Claim_Count is new Primitives.Standard_Unsigned;

      type Hazard_Pointer_Array is array (HP_Index) of
        aliased Atomic_Node_Access;
      pragma Atomic_Components (Hazard_Pointer_Array);

      type Node_Array is array (Valid_Node_Index) of
        aliased Atomic_Node_Access;
      pragma Volatile (Node_Array);
      pragma Atomic_Components (Node_Array);

      type Claim_Array is array (Valid_Node_Index) of
        aliased Claim_Count;
      --  pragma Volatile (Claim_Array);
      --  pragma Atomic_Components (Claim_Array);

      type Done_Array is array (Valid_Node_Index) of aliased Boolean;
      --  pragma Volatile (Done_Array);
      --  pragma Atomic_Components (Done_Array);

      --  Persistent shared per-task variables.
      type Task_Shared is
         record
            Hazard_Pointer : Hazard_Pointer_Array;
            pragma Volatile (Hazard_Pointer);
            DL_Nodes       : Node_Array;
            pragma Volatile (DL_Nodes);
            DL_Claims      : Claim_Array := (others => 0);
            pragma Volatile (DL_Claims);
            DL_Done        : Done_Array  := (others => False);
            pragma Volatile (DL_Done);
         end record;

      package TSS is new NBAda.Per_Task_Storage.Shared (Task_Shared,
                                                        Process_Ids);

      type DL_Nexts_Array is array (Valid_Node_Index) of Node_Index;

      function Hash_Ref (Ref  : in Managed_Node_Access;
                         Size : in Natural) return Natural;
      package P_Sets is
         new NBAda.Internals.Hash_Tables (Managed_Node_Access, "=", Hash_Ref);
      subtype P_Set_Type is P_Sets.Hash_Table
        (Size => 2 * Natural (Max_Delete_List_Size) + 1);

      --  Task local static data.
      type Task_Local is
         record
            D_List   : Node_Index := 0;
            D_Count  : Node_Count := 0;
            DL_Nexts : DL_Nexts_Array := (others => 0);
            P_Set    : P_Set_Type;
         end record;

      package TLS is new NBAda.Per_Task_Storage.Local (Task_Local,
                                                       Process_Ids);

      type Mutable_View (Self : access Memory_Manager) is
         limited null record;

      type Memory_Manager is new Memory_Manager_Base with
         record
            Shared          : TSS.Storage;
            Local           : TLS.Storage;
            --  Statistics counters.
            Nodes_Created   : aliased Primitives.Unsigned_32 := 0;
            pragma Atomic (Nodes_Created);
            Nodes_Reclaimed : aliased Primitives.Unsigned_32 := 0;
            pragma Atomic (Nodes_Reclaimed);
            Mutable         : Mutable_View (Memory_Manager'Access);
         end record;

   end Reference_Operations;

private

   subtype Reference_Count is Primitives.Unsigned_32;
   subtype MM_Magic_Type   is NBAda.Memory_Reclamation.MM_Magic_Type;

   type Managed_Node_Base is abstract tagged limited
      record
         MM_RC    : aliased Reference_Count := 0;
         pragma Atomic (MM_RC);
         MM_Trace : aliased Boolean := False;
         pragma Atomic (MM_Trace);
         MM_Del   : aliased Boolean := False;
         pragma Atomic (MM_Del);
         MM_Magic : aliased MM_Magic_Type := MM_Live;
         pragma Atomic (MM_Magic);
      end record;

   type Managed_Node_Access is
     access all Managed_Node_Base'Class;

end NBAda.Memory_Reclamation.Beware_And_Cleanup;
