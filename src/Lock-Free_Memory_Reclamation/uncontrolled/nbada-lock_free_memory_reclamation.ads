-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lockfree_reference_counting.ads
-- Description     : Lock-free reference counting.
-- Author          : Anders Gidenstam and Håkan Sundell
-- Created On      : Fri Nov 19 13:54:45 2004
-- $Id: nbada-lock_free_memory_reclamation.ads,v 1.1 2004/11/25 15:55:26 andersg Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Primitives;

generic
   Max_Number_Of_Dereferences : Natural;
   --  Maximum number of simultaneously dereferenced links per thread.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Lockfree_Reference_Counting is

   type Reference_Counted_Node is abstract tagged limited private;

   type Shared_Reference is limited private;

   type Operation_On_Reference is access
     procedure (Link_X : access Shared_Reference);
   procedure For_Each_Reference_Of (Node      : access Reference_Counted_Node;
                                    Operation : Operation_On_Reference) is
      abstract;

   type Node_Access is access Reference_Counted_Node'Class;

   --  Operations.
   function  Deref   (Link : access Shared_Reference) return Node_Access;
   procedure Release (Node : in Node_Access);
   procedure Delete  (Node : in Node_Access);
   procedure Store   (Link : access Shared_Reference;
                      Node : in Node_Access);

   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean;

private

   --  Clean up and scan thresholds.
   --  These are yet to be derived!
   Threshold_1 : constant := 100;
   Threshold_2 : constant := Threshold_1 / 2;

   subtype Reference_Count is Primitives.Unsigned_32;

   type Reference_Counted_Node is abstract tagged limited
      record
         MM_RC   : aliased Reference_Count;
         pragma Atomic (MM_RC);
         MM_Next : aliased Shared_Reference;
         pragma Atomic (MM_Next);
      end record;

   type Shared_Reference is new Node_Access;
   pragma Atomic (Shared_Reference);

end Lockfree_Reference_Counting;
