-------------------------------------------------------------------------------
--  Epoch-based memory reclamation.
--  Copyright (C) 2006 - 2008  Anders Gidenstam
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
--  Filename        : epoch_based_memory_reclamation.ads
--  Description     : Implementation of Keir Fraser's epoch-based memory
--                    reclamation scheme.
--                    See Keir Fraser, "Practical lock-freedom",
--                    Technical Report 579, Computer Laboratory,
--                    University of Cambridge, 2004.
--  Author          : Anders Gidenstam
--  Created On      : Wed Mar  8 12:04:29 2006
--  $Id: nbada-epoch_based_memory_reclamation.ads,v 1.12 2008/05/14 12:36:51 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Primitives;

generic

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

   Epoch_Update_Threshold : Natural := 100;
   --  The number of critical sections entered between attempts to update
   --  the global epoch.

   Debug : Boolean := False;
   --  Enable debug output and extra checks.

package NBAda.Epoch_Based_Memory_Reclamation is

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
   generic
      type Managed_Node is new Managed_Node_Base with private;
   package Operations is

      type Shared_Reference is limited private;
      --  Note: All shared variables of type Shared_Reference MUST be
      --        declared atomic by 'pragma Atomic (Variable_Name);' .

      type Node_Access is access all Managed_Node;
      --  Select an appropriate (preferably non-blocking) storage pool
      --  by the "for My_Node_Access'Storage_Pool use ..." construct.
      --  Note: There SHOULD NOT be any shared variables of type
      --        Node_Access.


      ----------------------------------------------------------------------
      --  Operations on shared references.
      ----------------------------------------------------------------------

      function  Dereference (Shared : access Shared_Reference)
                            return Node_Access;
      --  Note:

      procedure Release     (Local  : in Node_Access);
      --  Note: Each dereferenced shared pointer MUST be released
      --        eventually.

      procedure Delete      (Local  : in Node_Access);
      --  Note: Delete may only be called when the caller can
      --        guarantee that there are NO and WILL NOT BE any more shared
      --        references to the node. The memory management scheme makes
      --        sure the node is not freed until all local references have
      --        been released.

      function  Boolean_Compare_And_Swap (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in     Node_Access)
                                         return Boolean;

      procedure Value_Compare_And_Swap   (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in out Node_Access);

      procedure Void_Compare_And_Swap    (Shared    : access Shared_Reference;
                                          Old_Value : in     Node_Access;
                                          New_Value : in     Node_Access);


      procedure Initialize (Shared    : access Shared_Reference;
                            New_Value : in     Node_Access);
      procedure Store   (Link : access Shared_Reference;
                         Node : in     Node_Access)
        renames Initialize;
      --  Note: Initialize and Store are only safe to use when there are no
      --        concurrent updates.

   private

      type Shared_Reference is new Node_Access;
      --   pragma Atomic (Shared_Reference);
      --   pragma Volatile (Shared_Reference);
      --  Note: All shared variables of type Shared_Reference MUST be
      --        declared atomic by 'pragma Atomic (Variable_Name);' .

   end Operations;

   ----------------------------------------------------------------------------
   --  Reference enabled operations on shared references.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   type Shared_Reference_Base is limited private;
   --  For type separation between shared references to different
   --  managed types derive your own shared reference types from
   --  Shared_Reference_Base and instantiate the memory management
   --  operation package below for each of them.

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

      type Private_Reference is private;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Private_Reference.
      Null_Reference : constant Private_Reference;
      --  Note: A marked null reference is not equal to Null_Reference.
      function Image (R : Private_Reference) return String;

      ----------------------------------------------------------------------
      --  Operations.
      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference;
      --  Note: Dereference preservs any mark on Link.all.
      --        In particular Mark (Null_Reference) /= Null_Reference.

      procedure Release (Node : in Private_Reference);

      function  "+"     (Node : in Private_Reference)
                        return Node_Access;
      pragma Inline_Always ("+");
      function  Deref   (Node : in Private_Reference)
                        return Node_Access;

      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean;

      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference);

      procedure Delete  (Node : in Private_Reference);

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
      function Create return Private_Reference;
      --  Creates a new User_Node and returns a safe reference to it.

      --  Private (and shared) references can be tagged with a mark.
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

      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean;
      pragma Inline_Always ("=");
      --  It is possible to compare a reference to the current value of a link.

   private

      type Private_Reference is new Primitives.Standard_Unsigned;

      Null_Reference : constant Private_Reference := 0;

      Mark_Bits  : constant := 2;
      --  Note: Reference_Counted_Node_Base'Alignment >= 2 ** Mark_Bits
      --        MUST hold.
      Mark_Mask  : constant Private_Reference := 2 ** Mark_Bits - 1;
      Ref_Mask   : constant Private_Reference := -(2 ** Mark_Bits);

   end Reference_Operations;

   procedure Print_Statistics;
   --  NOTE: Not thread-safe.

private

   type Managed_Node_Access is access all Managed_Node_Base'Class;

   type Shared_Reference_Base is new Managed_Node_Access;
   --   pragma Atomic (Shared_Reference);
   --   pragma Volatile (Shared_Reference);

   type Managed_Node_Base is abstract tagged limited
      record
         MM_Next : aliased Shared_Reference_Base;
         pragma Atomic (MM_Next);
      end record;

end NBAda.Epoch_Based_Memory_Reclamation;
