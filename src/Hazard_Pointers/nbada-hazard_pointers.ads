-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : hazard_pointers.ads
-- Description     : Lock-Free Ada implementation of Maged Michael's
--                   Hazard Pointers.
-- Author          : Anders Gidenstam
-- Created On      : Thu Nov 25 18:10:15 2004
-- $Id: nbada-hazard_pointers.ads,v 1.1 2004/11/25 22:56:53 anders Exp $
-------------------------------------------------------------------------------

with Process_Identification;

generic
   Max_Number_Of_Dereferences : Natural;
   --  Maximum number of simultaneously dereferenced links per thread.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Hazard_Pointers is

   type Managed_Node is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

   type Shared_Reference is limited private;

   type Node_Access is access Managed_Node'Class;
   --  Select an appropriate (preferably non-blocking) storage pool
   --  by the "for  Node_Access'Storage_Pool use ..." construct.
   --  Note: There should not be any shared variables of type Node_Access.

   ----------------------------------------------------------------------------
   --  Operations on shared references.
   ----------------------------------------------------------------------------

   function  Dereference (Shared : access Shared_Reference)
                         return Node_Access;
   --  Note:
   procedure Release     (Local  : in Node_Access);
   --  Note: Each dereferenced shared pointer MUST be Released eventually.

   procedure Delete      (Local  : in Node_Access);
   --  Note: Delete may only be called when the caller can guarantee
   --        that there are NO and will not be any more shared references to
   --        the element.


   function  Compare_And_Swap (Shared    : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean;

private

   type Managed_Node is abstract tagged limited
      record
         MM_Next : aliased Shared_Reference;
         pragma Atomic (MM_Next);
      end record;

   type Shared_Reference is new Node_Access;
   pragma Atomic (Shared_Reference);

end Hazard_Pointers;
