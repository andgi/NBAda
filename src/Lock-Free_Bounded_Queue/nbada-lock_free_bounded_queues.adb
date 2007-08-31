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
--  Filename        : lock_free_bounded_queues.adb
--  Description     : Lock-Free Ada implementation of P. Tsigas and Y. Zhang's
--                    non-blocking bounded FIFO queue.
--                    Based on P. Tsigas and Y. Zhang, "A Simple, Fast and
--                    Scalable Non-Blocking Concurrent FIFO Queue for Shared
--                    Memory Multiprocessor Systems", Proceedings of the 13th
--                    annual ACM symposium on Parallel algorithms and
--                    architectures (SPAA), 134--143, ACM, July 2001.
--  Author          : Anders Gidenstam
--  Created On      : Mon Jun 27 17:55:38 2005
--  $Id: nbada-lock_free_bounded_queues.adb,v 1.7 2007/08/31 09:45:21 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Primitives;

package body Lock_Free_Bounded_Queues is

   ----------------------------------------------------------------------------
   procedure CAS is
      new Primitives.Standard_Void_Compare_And_Swap (Element_Type);
   function  CAS is
      new Primitives.Standard_Boolean_Compare_And_Swap (Element_Type);

   procedure CAS is
      new Primitives.Void_Compare_And_Swap_32 (Queue_Index);

   ----------------------------------------------------------------------------
   procedure Enqueue (Queue   : in out Lock_Free_Queue;
                      Element : in     Element_Type) is

   begin
      if Element = Null_0 or Element = Null_1 then
         raise Constraint_Error;
      end if;

      loop
         <<Retry>>
         declare
            Old_Tail_Index : constant Queue_Index := Queue.Tail;
            Tail_Index     : Queue_Index          := Old_Tail_Index;
            Tail_Elem      : Element_Type         :=
              Queue.Element (Tail_Index);
            Next_Index     : Queue_Index          :=
              (Tail_Index + 1) mod Queue.Max_Size;
         begin
            while Tail_Elem /= Null_0 and Tail_Elem /= Null_1 loop

               --  Check tail consistency.
               if Old_Tail_Index /= Queue.Tail then
                  goto Retry;
               end if;

               --  If Next_Index reaches Head then the queue might be full.
               exit when Next_Index = Queue.Head;

               --  Look at the next cell.
               Tail_Elem  := Queue.Element (Next_Index);
               Tail_Index := Next_Index;
               Next_Index := (Tail_Index + 1) mod Queue.Max_Size;
            end loop;

            --  Check tail consistency.
            if Old_Tail_Index /= Queue.Tail then
               goto Retry;
            end if;

            --  Check whether the queue is full.
            if Next_Index = Queue.Head then

               Tail_Index := (Next_Index + 1) mod Queue.Max_Size;
               Tail_Elem  := Queue.Element (Tail_Index);

               if Tail_Elem /= Null_0 and Tail_Elem /= Null_1 then
                  --  The queue is full.
                  raise Queue_Full;
               end if;

               if Tail_Index = 0 then
                  --  Head rewind.
                  Queue.Vnull := Tail_Elem;
               end if;

               --  Help dequeue by updating head.
               CAS (Queue.Head'Access,
                    Old_Value => Next_Index,
                    New_Value => Tail_Index);

            else

               --  Check tail consistency.
               if Old_Tail_Index /= Queue.Tail then
                  goto Retry;
               end if;

               --  Get the actual tail and attempt to enqueue the data.
               if
                 CAS (Queue.Element (Tail_Index)'Access,
                      Old_Value => Tail_Elem,
                      New_Value => Element)
               then
                  --  The enqueue succeeded.
                  if Next_Index mod 2 = 0 then
                     CAS (Queue.Tail'Access,
                          Old_Value => Old_Tail_Index,
                          New_Value => Next_Index);
                  end if;
                  return;
               end if;
            end if;
         end;
      end loop;
   end Enqueue;

   ----------------------------------------------------------------------------
   procedure Dequeue (Queue   : in out Lock_Free_Queue;
                      Element :    out Element_Type) is

      ----------------------------------------------------------------------
      function Switch (A_Null : Element_Type) return Element_Type;

      ----------------------------------------------------------------------
      function Switch (A_Null : Element_Type) return Element_Type is
      begin
         if A_Null = Null_0 then
            return Null_1;
         elsif A_Null = Null_1 then
            return Null_0;
         else
            raise Constraint_Error;
         end if;
      end Switch;

   begin
      loop
         <<Retry>>
         declare
            Head_Index : constant Queue_Index := Queue.Head;
            Next_Index : Queue_Index          :=
              (Head_Index + 1) mod Queue.Max_Size;
            Head_Elem  : Element_Type         :=
              Queue.Element (Next_Index);
         begin
            --  Find the actual head.
            while Head_Elem = Null_0 or Head_Elem = Null_1 loop
               --  Check head consistency.
               if Head_Index /= Queue.Head then
                  goto Retry;
               end if;

               --  Two consecutive NULL means empty.
               if Next_Index = Queue.Tail then
                  raise Queue_Empty;
               end if;

               Next_Index := (Next_Index + 1) mod Queue.Max_Size;
               Head_Elem  := Queue.Element (Next_Index);
            end loop;

            --  Check head consistency.
            if Head_Index /= Queue.Head then
               goto Retry;
            end if;

            --  Check whether the queue is empty.
            if Next_Index = Queue.Tail then

               --  Help the dequeue by updating tail.
               CAS (Queue.Tail'Access,
                    Old_Value => Next_Index,
                    New_Value => (Next_Index + 1) mod Queue.Max_Size);

            else

               --  If dequeue rewind to 0, switch NULL to avoid ABA problems.
               declare
                  Tmp_Null : Element_Type;
               begin
                  if Next_Index /= 0 then
                     if Next_Index < Head_Index then
                        Tmp_Null := Queue.Element (0);
                     else
                        Tmp_Null := Queue.Vnull;
                     end if;
                  else
                     Tmp_Null := Switch (Queue.Vnull);
                  end if;

                  --  Check head consistency.
                  if Head_Index /= Queue.Head then
                     goto Retry;
                  end if;

                  --  Get the actual head. Null value means empty.
                  if
                    CAS (Queue.Element (Next_Index)'Access,
                         Old_Value => Head_Elem,
                         New_Value => Tmp_Null)
                  then
                     --  If dequeue rewind to 0, switch NULL to avoid
                     --  ABA problems.
                     if Next_Index /= 0 then
                        Queue.Vnull := Tmp_Null;
                     end if;

                     if Next_Index mod 2 = 0 then
                        CAS (Queue.Head'Access,
                             Old_Value => Head_Index,
                             New_Value => Next_Index);
                     end if;

                     Element := Head_Elem;
                     return;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Dequeue;

   ----------------------------------------------------------------------------
   function  Dequeue (Queue : access Lock_Free_Queue) return Element_Type is
      Tmp : Element_Type;
   begin
      Dequeue (Queue.all, Tmp);
      return Tmp;
   end Dequeue;

   ----------------------------------------------------------------------------
   function Is_Empty (Queue : access Lock_Free_Queue) return Boolean is
   begin

      loop
         <<Retry>>
         declare
            Head_Index : constant Queue_Index := Queue.Head;
            Next_Index : Queue_Index          :=
              (Head_Index + 1) mod Queue.Max_Size;
            Head_Elem  : Element_Type         :=
              Queue.Element (Next_Index);
         begin
            --  Find the actual head.
            while Head_Elem = Null_0 or Head_Elem = Null_1 loop
               --  Check head consistency.
               if Head_Index /= Queue.Head then
                  goto Retry;
               end if;

               --  Two consecutive NULL means empty.
               if Next_Index = Queue.Tail then
                  return True;
               end if;

               Next_Index := (Next_Index + 1) mod Queue.Max_Size;
               Head_Elem  := Queue.Element (Next_Index);
            end loop;

            --  Check head consistency.
            if Head_Index /= Queue.Head then
               goto Retry;
            end if;

            --  Check whether the queue is empty.
            if Next_Index = Queue.Tail then

               --  Help the dequeue by updating tail.
               CAS (Queue.Tail'Access,
                    Old_Value => Next_Index,
                    New_Value => (Next_Index + 1) mod Queue.Max_Size);

            else

               --  Check head consistency.
               if Head_Index /= Queue.Head then
                  goto Retry;
               end if;

               --  Check the actual head. Null value means empty.
               --  But Head_Elem can't be null here, can it?
               if Queue.Element (Next_Index) = Head_Elem then
                  return Head_Elem = Null_0 or Head_Elem = Null_1;
               end if;

            end if;
         end;
      end loop;

   end Is_Empty;

   ----------------------------------------------------------------------------
   procedure Make_Empty (Queue : in out Lock_Free_Queue) is
   begin
      Queue.Head    := 0;
      Queue.Tail    := 1;
      Queue.Element := (0      => Null_1,
                        others => Null_0);
      Queue.Vnull   := Null_1;
   end Make_Empty;

end Lock_Free_Bounded_Queues;
