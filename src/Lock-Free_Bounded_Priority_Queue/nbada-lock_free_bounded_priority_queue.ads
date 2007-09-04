-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.ads
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 11:52:12 2002
-- $Id: nbada-lock_free_bounded_priority_queue.ads,v 1.6 2007/09/04 09:42:38 andersg Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Large_Primitives;
with Primitives;

generic
   type Element_Type is private;
   --  The Element_Type must be atomic and Element_Type'Object_Size must be 32.
   with function ">" (Left, Rigth : Element_Type) return Boolean;
   with function Image (Key : Element_Type) return String;

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package Lock_Free_Bounded_Priority_Queue is

   type Priority_Queue_Type (Max_Size : Positive) is limited private;

   procedure Initialize (Queue   : in out Priority_Queue_Type);
   --  Must be called before any other operation on the Queue.

   procedure Insert (Queue   : in out Priority_Queue_Type;
                     Element : in     Element_Type);
   --  Insert an element in the queue.
   --  Raises Queue_Full if the queue is full.

   procedure Delete_Min (Queue   : in out Priority_Queue_Type;
                         Element :    out Element_Type);
   --  Remove the minimum element from the queue.
   --  Raises Queue_Empty if the queue is empty.

--   function  Get_Min  (Queue : Priority_Queue_Type) return Element_Type;
   --  Get the minumum element in the queue.
   --  Raises Queue_Empty if the queue is empty.

--   function  Is_Empty (Queue : Priority_Queue_Type) return Boolean;
   --  Return true if the queue is empty.

--   function  Is_Full  (Queue : Priority_Queue_Type) return Boolean;
   --  Returns true if the queue is full.

--   pragma Inline (Get_Min);
--   pragma Inline (Is_Empty);
--   pragma Inline (Is_Full);

   function Image (Queue : Priority_Queue_Type) return String;
   --  Image.

   procedure Stabilize_Heap (Queue : in out Priority_Queue_Type);
   --  For debugging use only.

   --  Exceptions
   Queue_Full  : exception;
   Queue_Empty : exception;

private

   package LL_SC is new Large_Primitives (Max_Number_Of_Links => 2,
                                          Process_Ids         => Process_Ids);


   type Element_Access is access all Element_Type;
   type Entry_Status is (SIFTING_1, SIFTING_2,
                         SWAP_WITH_PARENT, SWAP_WITH_ANC,
                         STABLE, DELETED, NUL);
   subtype Heap_Index is Positive;
   type Operation_ID is new Primitives.Unsigned_32;
   type Operation_Type is (INSERT, DELETE_MIN, NONE);

   ----------------------------------------------------------------------------
   type Heap_Entry is
      record
         Key      : Element_Type;
         Status   : Entry_Status := NUL;
         Old_Key  : Element_Type;
         Op_ID    : Operation_ID := 0;
         Sift_Pos : Heap_Index   := 1;
      end record;
   function Is_Null (HE : Heap_Entry) return Boolean;

   package Heap_Entry_LL_SC is
      new LL_SC.Load_Linked_Store_Conditional (Element => Heap_Entry);

   subtype Shared_Heap_Entry is Heap_Entry_LL_SC.Shared_Element;

   type Heap_Array is array (Heap_Index range <>) of Shared_Heap_Entry;
--   pragma Atomic_Components (Heap_Array);

   ----------------------------------------------------------------------------
   type Heap_Status is
      record
         Size    : Natural        := 0;
         Op_ID   : Operation_ID   := 0;
         Op_Type : Operation_Type := NONE;
         Op_Arg  : Element_Access;
      end record;

   package Heap_Status_LL_SC is
      new LL_SC.Load_Linked_Store_Conditional (Element => Heap_Status);

   subtype Shared_Heap_Status is Heap_Status_LL_SC.Shared_Element;

   type Priority_Queue_Type (Max_Size : Positive) is
      record
         Status : Shared_Heap_Status;
         pragma Atomic (Status);
         Heap   : Heap_Array (1 .. Max_Size);
      end record;
   --pragma Volatile (Priority_Queue_Type);

end Lock_Free_Bounded_Priority_Queue;
