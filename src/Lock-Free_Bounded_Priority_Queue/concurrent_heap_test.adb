-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : concurrent_heap_test.adb
-- Description     : Test of non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Feb 20 16:39:08 2003
-- $Id: concurrent_heap_test.adb,v 1.2 2003/02/21 16:34:20 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with Non_Blocking_Priority_Queue;
with Primitives;

procedure Concurrent_Heap_Test is

   type My_Int is new Integer;
   pragma Atomic (My_Int);

   package My_Int_Queue is new
     Non_Blocking_Priority_Queue (Element_Type => My_Int,
                                  ">"          => ">",
                                  Image        => My_Int'Image,
                                  Min_Key      => My_Int'First);
   use My_Int_Queue;

   Queue_Size : constant := 100;
   My_Q : Priority_Queue_Type (Queue_Size);

   task type Producer (Max    : My_Int;
                       Factor : My_Int;
                       Offset : My_Int);

   task body Producer is
   begin
      for I in reverse My_Int(0) .. Max loop
         Ada.Text_IO.Put_Line ("Insert (" &
                               My_Int'Image (Factor * I + Offset) & ")");
         Insert (My_Q, Factor * I + Offset);
      end loop;

   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Producer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Producer;

begin
   for X in 1 .. 1 loop
      declare
         P1 : Producer (Queue_Size/4, 10, 0);
         P2 : Producer (Queue_Size/4, 10, 5);
      begin
         null;
      end;

--      Ada.Text_IO.Put_Line (Image (My_Q));
--      Ada.Text_IO.Skip_Line;

      declare
         Expected : My_Int := 0;
         Min      : My_Int;
      begin
         loop
            Delete_Min (My_Q, Min);
            Ada.Text_IO.Put_Line ("Heap_Test: Delete_Min: " &
                                  My_Int'Image (Min));

            if Min /= Expected then
               Ada.Text_IO.Put_Line ("Heap_Test: Heap property failure!");
               Ada.Text_IO.Put_Line (Image (My_Q));
               return;
            end if;
            Expected := Min + 5;

            --exit when Min > Queue_Size/10;

         end loop;

      exception
         when QUEUE_EMPTY =>
            null;
      end;
   end loop;

   -- Make the queue empty.
   declare
      Min      : My_Int;
   begin
      loop
         Delete_Min (My_Q, Min);
         Ada.Text_IO.Put_Line ("Heap_Test: Delete_Min: " &
                               My_Int'Image (Min));
      end loop;

   exception
      when QUEUE_EMPTY =>
         null;
   end;
end Concurrent_Heap_Test;
