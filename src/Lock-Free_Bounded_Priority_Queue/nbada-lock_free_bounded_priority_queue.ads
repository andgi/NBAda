-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.ads
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 11:52:12 2002
-- $Id: nbada-lock_free_bounded_priority_queue.ads,v 1.2 2003/01/19 13:04:38 andersg Exp $
-------------------------------------------------------------------------------

with Primitives;

generic
   -- The Element_Type must be atomic and Element_Type'Object_Size must be 32.
   type Element_Type is private;
   with function ">" (Left, Rigth : Element_Type) return Boolean;
   with function Image (Key : Element_Type) return String;
   -- The minimum key.
   Min_Key : in Element_Type;

package Non_Blocking_Priority_Queue is

   type Priority_Queue_Type (Max_Size : Positive) is limited private;

   -- Insert an element in the queue.
   -- Raises Queue_Full if the queue is full.
   procedure Insert (Queue   : in out Priority_Queue_Type;
                     Element : in     Element_Type);

   -- Remove the minimum element from the queue.
   -- Raises Queue_Empty if the queue is empty.
   procedure Delete_Min (Queue   : in out Priority_Queue_Type;
                         Element :    out Element_Type);

   -- Get the minumum element in the queue.
   -- Raises Queue_Empty if the queue is empty.
   --function  Get_Min  (Queue : Priority_Queue_Type) return Element_Type;

   -- Return true if the queue is empty.
   --function  Is_Empty (Queue : Priority_Queue_Type) return Boolean;

   -- Returns true if the queue is full.
   --function  Is_Full  (Queue : Priority_Queue_Type) return Boolean;

   --pragma Inline (Get_Min);
   --pragma Inline (Is_Empty);
   --pragma Inline (Is_Full);

   -- Image.
   function Image (Queue : Priority_Queue_Type) return String;

   -- Exceptions
   Queue_Full  : exception;
   Queue_Empty : exception;

private

   type Element_Access is access all Element_Type;
   type Entry_Status is (SIFTING_1, SIFTING_2, SWAP_WITH_PARENT, SWAP_WITH_ANC, STABLE, EMPTY);
   subtype Heap_Index is Positive;
   type Operation_ID is new Primitives.Unsigned_32;
   type Operation_Type is (INSERT, DELETE_MIN, NONE);

   ----------------------------------------------------------------------------
   type Heap_Entry is
      record
         Key      : Element_Type := Min_Key;
         Status   : Entry_Status;
         Old_Key  : Element_Type;
         Op_ID    : Operation_ID;
         Sift_Pos : Heap_Index;
      end record;

   type Heap_Entry_Access is access Heap_Entry;
   pragma Atomic (Heap_Entry_Access);

   type Heap_Array is array (Heap_Index range <>) of aliased Heap_Entry_Access;
   pragma Atomic_Components (Heap_Array);

   ----------------------------------------------------------------------------
   type Heap_Status is
      record
         Size    : Natural;
         Op_ID   : Operation_ID;
         Op_Type : Operation_Type;
         Op_Arg  : Element_Access;
      end record;

   type Heap_Status_Access is access Heap_Status;
   pragma Atomic (Heap_Status_Access);

   type Priority_Queue_Type (Max_Size : Positive) is
      record
         Status : aliased Heap_Status_Access :=
           new Heap_Status'(0, 0, NONE, null);
         Heap   : Heap_Array (1 .. Max_Size);
      end record;
   pragma Volatile (Priority_Queue_Type);

end Non_Blocking_Priority_Queue;
