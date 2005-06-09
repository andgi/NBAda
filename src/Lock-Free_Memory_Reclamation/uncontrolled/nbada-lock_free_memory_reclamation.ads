-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lockfree_reference_counting.ads
-- Description     : Lock-free reference counting.
-- Author          : Anders Gidenstam and Håkan Sundell
-- Created On      : Fri Nov 19 13:54:45 2004
-- $Id: nbada-lock_free_memory_reclamation.ads,v 1.6 2005/06/09 13:00:47 anders Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Primitives;

generic
   Max_Number_Of_Dereferences : Natural;
   --  Maximum number of simultaneously dereferenced links per thread.
   Max_Number_Of_Links_Per_Node : Natural;
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Lockfree_Reference_Counting is

   pragma Elaborate_Body;

   type Reference_Counted_Node is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.
   procedure Dispose  (Node       : access Reference_Counted_Node;
                       Concurrent : in     Boolean) is abstract;
   procedure Clean_Up (Node : access Reference_Counted_Node) is abstract;
   function Is_Deleted (Node : access Reference_Counted_Node)
                       return Boolean;

   type Shared_Reference is limited private;
   --  All shared variables of type Shared_Reference MUST be declared
   --  atomic by 'pragma Atomic (Variable_Name);' .

   type Node_Access is access all Reference_Counted_Node'Class;
   --  Select an appropriate (preferably non-blocking) storage pool
   --  by the "for My_Node_Access'Storage_Pool use ..." construct.
   --  Note: There should not be any shared variables of type Node_Access.

   --  Operations.
   function  Deref   (Link : access Shared_Reference) return Node_Access;
   procedure Release (Node : in Node_Access);

   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean;

   procedure Delete  (Node : in Node_Access);


   procedure Store   (Link : access Shared_Reference;
                      Node : in Node_Access);

   generic
      type User_Node is new Reference_Counted_Node with private;
      type User_Node_Access is access User_Node;
   function Create return Node_Access;
   --  Creates a new User_Node and returns a safe reference to it.

private

   --  Clean-up threshold.
   Threshold_1 : Natural :=
     --  The derived bound in the paper.
     Process_Ids.Max_Number_Of_Processes ** 2 *
     (Max_Number_Of_Dereferences + Max_Number_Of_Links_Per_Node +
      Max_Number_Of_Links_Per_Node + 1);

   --  Scan threshold.
   Threshold_2 : Natural := Threshold_1;

   subtype Reference_Count is Primitives.Unsigned_32;

   type Reference_Counted_Node is abstract tagged limited
      record
         MM_RC    : aliased Reference_Count := 0;
         pragma Atomic (MM_RC);
         MM_Trace : aliased Boolean := False;
         pragma Atomic (MM_Trace);
         MM_Del   : aliased Boolean := False;
         pragma Atomic (MM_Del);
      end record;

   type Shared_Reference is new Node_Access;
   --pragma Atomic (Shared_Reference);

end Lockfree_Reference_Counting;
