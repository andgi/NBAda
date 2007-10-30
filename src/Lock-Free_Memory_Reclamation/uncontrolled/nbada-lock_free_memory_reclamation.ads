-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_memory_reclamation.ads
--  Description     : Ada implementation of the lock-free garbage reclamation
--                    Scheme from "Efficient and Reliable Lock-Free Memory
--                    Reclamation Based on Reference Counting",
--                    Anders Gidenstam, Marina Papatriantafilou,
--                    Håkan Sundell and Philippas Tsigas,
--                    Proceedings of the 8th International Symposium on
--                    Parallel Architectures, Algorithms and Networks (I-SPAN),
--                    pages 202 - 207, IEEE Computer Society, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Fri Nov 19 13:54:45 2004
--  $Id: nbada-lock_free_memory_reclamation.ads,v 1.21 2007/10/30 15:13:21 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;

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

   Debug : Boolean := False;
   --  Enables some profiling.

package NBAda.Lock_Free_Memory_Reclamation is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   type Managed_Node_Base is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

   procedure Dispose  (Node       : access Managed_Node_Base;
                       Concurrent : in     Boolean) is abstract;
   --  Dispose should set all shared references inside the node to null.

   procedure Clean_Up (Node : access Managed_Node_Base) is abstract;
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

   ----------------------------------------------------------------------------
   type Shared_Reference_Base is limited private;
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

   package Operations is

      type Node_Access is access all Managed_Node;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Node_Access.

      type Private_Reference is private;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Private_Reference.
      Null_Reference : constant Private_Reference;
      function Image (R : Private_Reference) return String;

      ----------------------------------------------------------------------
      --  Operations.
      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference;

      procedure Release (Node : in Private_Reference);

      function  "+"     (Node : in Private_Reference)
                        return Node_Access;
      pragma Inline_Always ("+");
      function  Deref   (Node : in Private_Reference)
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


      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference);

      generic
         type User_Node_Access is access Managed_Node;
         --  Select an appropriate (preferably non-blocking) storage
         --  pool by the "for User_Node_Access'Storage_Pool use ..."
         --  construct.
         --  Note: The nodes allocated in this way must have an
         --        implementation of Free that use the same storage pool.
      function Create return Private_Reference;
      --  Creates a new User_Node and returns a safe reference to it.

      --  Private (and shared) references can be tagged with a mark.
      --  NOTE: A marked Null_Reference is not equal (=) to an unmarked.
      procedure Mark      (Node : in out Private_Reference);
      pragma Inline_Always (Mark);
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference;
      pragma Inline_Always (Mark);
      procedure Unmark    (Node : in out Private_Reference);
      pragma Inline_Always (Unmark);
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference;
      pragma Inline_Always (Unmark);
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function "=" (Left  : in     Private_Reference;
                    Right : in     Private_Reference) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean;
      pragma Inline_Always ("=");
      --  It is possible to compare a reference to the current value of a link.

      ------------------------------------------------------------------------
      --  Unsafe operations.
      --  These SHOULD only be use when the user algorithm guarantees
      --  the absence of ABA-problems.
      --  In such algorithms the use of these operations in some particular
      --  situations could allow some performance improving optimizations.
      ------------------------------------------------------------------------

      type Unsafe_Reference_Value is private;
      --  Note: An Unsafe_Reference_Value does not keep a claim to any
      --        node and can therefore only be used where ABA safety is
      --        ensured by other means. It cannot be dereferenced.

      function  Unsafe_Read (Link : access Shared_Reference)
                            return Unsafe_Reference_Value;
      pragma Inline_Always (Unsafe_Read);

      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Unsafe_Reference_Value;
                                  New_Value : in Private_Reference)
                                 return Boolean;
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Unsafe_Reference_Value;
                                  New_Value : in Unsafe_Reference_Value)
                                 return Boolean;
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Unsafe_Reference_Value;
                                  New_Value : in     Private_Reference);
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Unsafe_Reference_Value;
                                  New_Value : in     Unsafe_Reference_Value);

      function  Is_Marked (Node : in     Unsafe_Reference_Value)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function  Mark      (Node : in     Unsafe_Reference_Value)
                          return Unsafe_Reference_Value;
      pragma Inline_Always (Mark);

      function "=" (Val : in     Unsafe_Reference_Value;
                    Ref : in     Private_Reference) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Ref : in     Private_Reference;
                    Val : in     Unsafe_Reference_Value) return Boolean;
      pragma Inline_Always ("=");

      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Unsafe_Reference_Value) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Ref  : in     Unsafe_Reference_Value;
                    Link : in     Shared_Reference) return Boolean;
      pragma Inline_Always ("=");

   private

      type Private_Reference_Impl is new Primitives.Standard_Unsigned;
      type Unsafe_Reference_Value is new Private_Reference_Impl;

      subtype Index is Natural range 0 .. Max_Number_Of_Dereferences;
      type Private_Reference is
         record
            Ref : Private_Reference_Impl := 0;
            HP  : Index := 0;
         end record;

      Null_Reference : constant Private_Reference := (0, 0);

   end Operations;

   procedure Print_Statistics;

private

   subtype Reference_Count is Primitives.Unsigned_32;

   type Managed_Node_Base is abstract tagged limited
      record
         MM_RC    : aliased Reference_Count := 0;
         pragma Atomic (MM_RC);
         MM_Trace : aliased Boolean := False;
         pragma Atomic (MM_Trace);
         MM_Del   : aliased Boolean := False;
         pragma Atomic (MM_Del);
      end record;

   type Managed_Node_Access is
     access all Managed_Node_Base'Class;

   type Shared_Reference_Base_Impl is new Primitives.Standard_Unsigned;
   type Shared_Reference_Base is
      record
         Ref : Shared_Reference_Base_Impl := 0;
      end record;
   for Shared_Reference_Base'Size use Primitives.Standard_Unsigned'Size;
   pragma Atomic (Shared_Reference_Base);

   Null_Reference : constant Shared_Reference_Base := (Ref => 0);

   Mark_Bits  : constant := 1;
   --  Note: Reference_Counted_Node_Base'Alignment >= 2 ** Mark_Bits MUST hold.
   Mark_Mask  : constant Shared_Reference_Base_Impl := 2 ** Mark_Bits - 1;
   Ref_Mask   : constant Shared_Reference_Base_Impl := -(2 ** Mark_Bits);

end NBAda.Lock_Free_Memory_Reclamation;
