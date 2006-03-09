pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : epoch_based_memory_reclamation.ads
--  Description     : Implementation of Keir Fraser's epoch-based memory
--                    reclamation scheme.
--  Author          : Anders Gidenstam
--  Created On      : Wed Mar  8 12:04:29 2006
--  $Id: nbada-epoch_based_memory_reclamation.ads,v 1.1 2006/03/09 17:17:08 anders Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (Modified_GPL);

with Process_Identification;

generic

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

   Epoch_Update_Threshold : Natural := 100;
   --  The number of critical sections entered between attempts to update
   --  the global epoch.

package Epoch_Based_Memory_Reclamation is

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
      --  Note: Initialize is only safe to use when there are no
      --        concurrent updates.

   private

      type Shared_Reference is new Node_Access;
      --   pragma Atomic (Shared_Reference);
      --   pragma Volatile (Shared_Reference);
      --  Note: All shared variables of type Shared_Reference MUST be
      --        declared atomic by 'pragma Atomic (Variable_Name);' .

   end Operations;

   procedure Print_Statistics;

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

end Epoch_Based_Memory_Reclamation;
