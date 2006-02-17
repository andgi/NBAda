-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_deques.ads
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:46:02 2006
--  $Id: nbada-lock_free_deques.ads,v 1.1 2006/02/17 15:43:09 anders Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Lock_Free_Reference_Counting;
with Process_Identification;

pragma Elaborate_All (Lock_Free_Reference_Counting);

generic
   type Value_Type is private;
   --  Value type.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Lock_Free_Deques is

   ----------------------------------------------------------------------------
   --  Lock-free double-ended queue (a.k.a deque).
   ----------------------------------------------------------------------------
   type Deque_Type is limited private;

   Deque_Empty : exception;

   procedure Init    (Deque : in out Deque_Type);

   function  Pop_Right  (Deque : access Deque_Type) return Value_Type;
   procedure Push_Right (Deque : in out Deque_Type;
                         Value : in     Value_Type);

   function  Pop_Left  (Deque : access Deque_Type) return Value_Type;
   procedure Push_Left (Deque : in out Deque_Type;
                        Value : in     Value_Type);

private

   package LFRC is new Lock_Free_Reference_Counting
     (Max_Number_Of_Dereferences   => 7,
      --  Remember to account for the dereferences in the
      --  callbacks Clean_Up and Dispose (which are invoked by Delete).
      --  Here: PushRight <= ?
      --        PopRight  <= ?
      --        PushLeft  <= ?
      --        PopLeft   <= ?
      --        Dispose   <= ?
      --        Clean_up  <= ?
      --  Delete is called from Dequeue on a dereferenced node so the
      --  maximum number of simultaneous dereferences is ?.
      Max_Number_Of_Links_Per_Node => 2,
      Clean_Up_Threshold           => 256,
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids);

   type Deque_Node_Reference is new LFRC.Shared_Reference_Base;

   type Deque_Node is new LFRC.Managed_Node_Base with
      record
         Next     : aliased Deque_Node_Reference;
         pragma Atomic (Next);
         Previous : aliased Deque_Node_Reference;
         pragma Atomic (Previous);
         Value    : Value_Type;
      end record;
   --  Note:

   procedure Dispose  (Node       : access Deque_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Deque_Node);
   procedure Free     (Node : access Deque_Node);

   package LFRC_Ops is new LFRC.Operations (Deque_Node,
                                            Deque_Node_Reference);

   subtype Deque_Node_Access is LFRC_Ops.Private_Reference;

   type Deque_Type is limited
      record
         Head : aliased Deque_Node_Reference;
         pragma Atomic (Head);
         Tail : aliased Deque_Node_Reference;
         pragma Atomic (Tail);
      end record;

end Lock_Free_Deques;
