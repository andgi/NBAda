
with Lockfree_Reference_Counting;
with Process_Identification;

generic
   type Element_Type is private;
   --  The element type.
   with package Process_Ids is
    new Process_Identification (<>);
   --  Process identification.
package Lock_Free_Priority_Queues is

   type Lock_Free_Priority_Queue is limited private;

   procedure Insert     (Queue   : in out Lock_Free_Priority_Queue;
			 Element : in     Element_Type);

   function  Delete_Min (Queue   : access Lock_Free_Priority_Queue)
			 return Element_Type;

   Queue_Empty : exception;

private

   Max_Levels : constant := 7;

   package LFRC is new Lockfree_Reference_Counting
     (Max_Number_Of_Dereferences   => 4,
      --  Remember to account for the dereferences in the
      --  callbacks Clean_Up and Dispose (which are invoked by Delete).
      --  Here: Engueue  <= ?
      --        Dequeue  <= ?
      --        Dispose  <= 1
      --        Clean_up <= 2
      Max_Number_Of_Links_Per_Node => Max_Levels + 1,
      Clean_Up_Threshold           => 256,
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids);

   type Queue_Node_Reference is new LFRC.Shared_Reference_Base;

   type Key_Type is new Integer;
   type Level is range 0 .. Max_Levels;

   type Queue_Node_Reference_Array is
      array (Level range 1 .. Max_Levels) of aliased Queue_Node_Reference;
   --pragma Atomic_Components (Queue_Node_Reference_Array);

   type Queue_Node is --  (Max_Level : Level) is
     new LFRC.Reference_Counted_Node_Base with
      record
	 Max_Level : Level;
	 pragma Atomic (Max_Level);
         Next  : Queue_Node_Reference_Array;
--	 pragma Atomic_Components (Next);
	 Prev  : aliased Queue_Node_Reference;
         pragma Atomic (Prev);
         Item  : Element_Type;
      end record;

   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Queue_Node);
   procedure Free     (Node : access Queue_Node);

   package LFRC_Ops is new LFRC.Operations (Queue_Node,
                                            Queue_Node_Reference);

   subtype Queue_Node_Access is LFRC_Ops.Private_Reference;

   type Lock_Free_Priority_Queue is limited
      record
         Head : aliased Queue_Node_Reference;
         pragma Atomic (Head);
         Tail : aliased Queue_Node_Reference;
         pragma Atomic (Tail);
      end record;

end Lock_Free_Priority_Queues;
