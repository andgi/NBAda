-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - Lock-Free Reference Counting based on the
--  algorithm by Herlihy et al.
--  Copyright (C) 2006 - 2011  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-lock_free_reference_counting.ads
--  Description     : Ada implementation of lock-free reference counting.
--                    Based on M. Herlihy, V. Luchango, P. Martin, M. Moir,
--                    "Nonblocking Memory Management Support for Dynamic-Sized
--                     Data Structures",  ACM Transactions on Computer Systems,
--                    23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;
with NBAda.Memory_Reclamation.Reference_Operations;

generic

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

   Integrity_Checking : Boolean := False;
   --  Enable strong integrity checking.

   Collect_Statistics : Boolean := False;
   --  Enable some statics gathering.

package NBAda.Memory_Reclamation.Lock_Free_Reference_Counting is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   type Managed_Node_Base is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

   type Memory_Manager_Base is abstract tagged limited null record;

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

   function All_References (Node : access Managed_Node_Base)
                           return Reference_Set is abstract;
   --  All_References is used to remove all references from a node when
   --  it is being reclaimed.

   function Is_Deleted (Node : access Managed_Node_Base)
                       return Boolean;
   --  For API compatability with NBAda.Lock_Free_Memory_Reclamation.
   --  Always return False.

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
      --  Note: Dereference preservs any mark on Link.all.
      --        In particular Mark (Null_Reference) /= Null_Reference.

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

      procedure Delete  (Node : in Private_Reference)
        renames Release;
      --  For API compatability with
      --  NBAda.Memory_Reclamation.Reference_Operations.
      --  In the future LFRC could use the knowledge that a node is
      --  logically deleted to improve garbage management.

      procedure Rescan  (Node : in Private_Reference);
      --  For API compatability with
      --  NBAda.Memory_Reclamation.Reference_Operations.

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
      function Create (MM : in Memory_Manager'Class) return Private_Reference;
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

      type Memory_Manager_Access is access all Memory_Manager;

      type Private_Reference is
        new Basic_Reference_Operations.Private_Reference_Base with
         record
            MM  : Memory_Manager_Access;
         end record;

      type Mutable_View (Self : access Memory_Manager) is
         limited null record;

      type Memory_Manager is new Memory_Manager_Base with
         record
            --  Statistics counters.
            No_Nodes_Created   : aliased Primitives.Unsigned_32 := 0;
            pragma Atomic (No_Nodes_Created);
            No_Nodes_Reclaimed : aliased Primitives.Unsigned_32 := 0;
            pragma Atomic (No_Nodes_Reclaimed);
            Mutable            : Mutable_View (Memory_Manager'Access);
         end record;

   end Reference_Operations;

private

   subtype Reference_Count is Primitives.Unsigned_32;
   subtype MM_Magic_Type   is NBAda.Memory_Reclamation.MM_Magic_Type;

   type Managed_Node_Base is abstract tagged limited
      record
         MM_RC    : aliased Reference_Count := 1;
         pragma Atomic (MM_RC);
         MM_Magic : aliased MM_Magic_Type := MM_Live;
         pragma Atomic (MM_Magic);
      end record;

   type Managed_Node_Access is
     access all Managed_Node_Base'Class;

end NBAda.Memory_Reclamation.Lock_Free_Reference_Counting;
