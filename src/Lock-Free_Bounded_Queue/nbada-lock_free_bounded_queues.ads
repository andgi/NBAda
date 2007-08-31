-------------------------------------------------------------------------------
--  An implementation of P. Tsigas and Y. Zhangs's lock-free FIFO queue.
--  Copyright (C) 2005 - 2007  Anders Gidenstam
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_bounded_queues.ads
--  Description     : Lock-Free Ada implementation of P. Tsigas and Y. Zhang's
--                    non-blocking bounded FIFO queue.
--                    Based on P. Tsigas and Y. Zhang, "A Simple, Fast and
--                    Scalable Non-Blocking Concurrent FIFO Queue for Shared
--                    Memory Multiprocessor Systems", Proceedings of the 13th
--                    annual ACM symposium on Parallel algorithms and
--                    architectures (SPAA), 134--143, ACM, July 2001.
--  Author          : Anders Gidenstam
--  Created On      : Mon Jun 27 17:21:50 2005
--  $Id: nbada-lock_free_bounded_queues.ads,v 1.7 2007/08/31 09:45:21 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

generic
   type Element_Type is private;
   --  The Element_Type must be atomic and Element_Type'Object_Size must be
   --  equal to System.Word_Size.
   Null_0 : Element_Type;
   Null_1 : Element_Type;
   --  NOTE: These two values MUST be different and MUST NOT appear as
   --        data values in the queue.
package Lock_Free_Bounded_Queues is
   pragma Pure (Lock_Free_Bounded_Queues);

   type Queue_Size is mod 2**32;

   type Lock_Free_Queue (Max_Size : Queue_Size) is limited private;

   procedure Enqueue (Queue   : in out Lock_Free_Queue;
                      Element : in     Element_Type);

   procedure Dequeue (Queue   : in out Lock_Free_Queue;
                      Element :    out Element_Type);

   function  Dequeue (Queue : access Lock_Free_Queue) return Element_Type;

   function Is_Empty (Queue : access Lock_Free_Queue) return Boolean;

   procedure Make_Empty (Queue : in out Lock_Free_Queue);
   --  NOTE: Make_Empty SHOULD NOT be used when concurrent access is possible.

   Queue_Full  : exception;
   Queue_Empty : exception;

private

   subtype Queue_Index is Queue_Size;

   type Queue_Array is array (Queue_Index range <>) of aliased Element_Type;
--   pragma Atomic_Components (Queue_Array);

   type Lock_Free_Queue (Max_Size : Queue_Size) is limited
      record
         Head    : aliased Queue_Index := 0;
         pragma Atomic (Head);
         Tail    : aliased Queue_Index := 1;
         pragma Atomic (Tail);
         Element : Queue_Array (0 .. Max_Size) := (0      => Null_1,
                                                   others => Null_0);
         pragma Volatile (Element);
         Vnull   : aliased Element_Type := Null_1;
         pragma Atomic (Vnull);
      end record;

end Lock_Free_Bounded_Queues;
