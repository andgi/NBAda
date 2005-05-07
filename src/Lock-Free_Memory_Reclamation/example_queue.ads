-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : example_queue.ads
--  Description     : Simple example ADT for lock-free garbage collector.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $ID$
-------------------------------------------------------------------------------

with Lockfree_Reference_Counting;
with Process_Identification;

generic
   type Value_Type is private;
   --  Value type.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Example_Queue is

   ----------------------------------------------------------------------------
   --  Lock-free Queue.
   ----------------------------------------------------------------------------
   type Queue_Type is limited private;

   Queue_Empty : exception;

   procedure Init    (Queue : in out Queue_Type);
   function  Dequeue (Queue : access Queue_Type) return Value_Type;
   procedure Enqueue (Queue : in out Queue_Type;
                      Value : in     Value_Type);

private

   package LFRC is new Lockfree_Reference_Counting
     (Max_Number_Of_Dereferences   => 4,
      Max_Number_Of_Links_Per_Node => 1,
      Process_Ids                  => Process_Ids);
   use LFRC;


   type Queue_Node_Reference is new LFRC.Shared_Reference;

   type Queue_Node is new LFRC.Reference_Counted_Node with
      record
         Next  : aliased Queue_Node_Reference;
         Value : Value_Type;
      end record;
   type Queue_Node_Access is access all Queue_Node;

   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Queue_Node);


   type Queue_Type is
      record
         Head : aliased Queue_Node_Reference;
         Tail : aliased Queue_Node_Reference;
      end record;

end Example_Queue;
