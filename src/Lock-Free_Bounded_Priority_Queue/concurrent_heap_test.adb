-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : concurrent_heap_test.adb
--  Description     : Test of non-blocking priority queue.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 20 16:39:08 2003
--  $Id: concurrent_heap_test.adb,v 1.4 2007/09/04 09:42:38 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with Lock_Free_Bounded_Priority_Queue;
with Process_Identification;
with Primitives;

procedure Concurrent_Heap_Test is

   type My_Int is new Integer;
   pragma Atomic (My_Int);

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   package My_Int_Queue is new
     Lock_Free_Bounded_Priority_Queue
     (Element_Type => My_Int,
      ">"          => ">",
      Image        => My_Int'Image,
      Process_Ids  => PID);

   use My_Int_Queue;

   Queue_Size : constant := 1000;
   My_Q : Priority_Queue_Type (Queue_Size);

   task type Producer (Max    : My_Int;
                       Factor : My_Int;
                       Offset : My_Int);
   task type Consumer;

   procedure Produce (Max    : My_Int;
                      Factor : My_Int;
                      Offset : My_Int);
   procedure Produce (Max    : My_Int;
                      Factor : My_Int;
                      Offset : My_Int) is
   begin
      for I in reverse My_Int (0) .. Max - 1 loop
         Ada.Text_IO.Put_Line ("Insert (" &
                               My_Int'Image (Factor * I + Offset) & ")");
         Insert (My_Q, Factor * I + Offset);
      end loop;
   end Produce;

   task body Producer is
   begin
      PID.Register;
      for I in reverse My_Int (0) .. Max - 1 loop
         Ada.Text_IO.Put_Line ("Insert (" &
                               My_Int'Image (Factor * I + Offset) & ")");
         Insert (My_Q, Factor * I + Offset);
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Producer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Image (My_Q));
   end Producer;

   task body Consumer is
      Min : My_Int;
   begin
      PID.Register;
      loop
         Delete_Min (My_Q, Min);
         Ada.Text_IO.Put_Line ("Consumer: Delete_Min: " &
                               My_Int'Image (Min));
      end loop;

   exception
      when E : others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Consumer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Consumer;

begin
   PID.Register;
   Initialize (My_Q);

   for X in 1 .. 1 loop
      declare
--         P1 : Producer (Queue_Size, 5, 0);

--         P1 : Producer (Queue_Size / 3, 10, 0);
--         P2 : Producer (Queue_Size / 3, 10, 5);
         P1 : Producer (Queue_Size / 5, 20, 0);
         P2 : Producer (Queue_Size / 5, 20, 5);
         P3 : Producer (Queue_Size / 5, 20, 10);
         P4 : Producer (Queue_Size / 5, 20, 15);
         C  : Consumer;
      begin
         null;
      end;

--       Produce (Queue_Size / 3, 10, 0);
--       Produce (Queue_Size / 3, 10, 5);

--        Produce (Queue_Size / 5, 20, 0);
--        Produce (Queue_Size / 5, 20, 5);
--        Produce (Queue_Size / 5, 20, 10);
--        Produce (Queue_Size / 5, 20, 15);


--      Ada.Text_IO.Put_Line (Image (My_Q));
--      Ada.Text_IO.Skip_Line;
--      Primitives.Membar;

      declare
         Expected : My_Int := 0;
         Min      : My_Int;
      begin
         loop
--            Ada.Text_IO.Put_Line (Image (My_Q));
            Delete_Min (My_Q, Min);
            Ada.Text_IO.Put_Line ("Heap_Test: Delete_Min: " &
                                  My_Int'Image (Min));

            if Min /= Expected then
               Ada.Text_IO.Put_Line ("Heap_Test: Heap property failure! ");
               Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                     "Heap_Test: Heap property failure! ");
               Ada.Text_IO.Put_Line ("Heap_Test: Expected " &
                                     My_Int'Image (Expected) &
                                     " got " &
                                     My_Int'Image (Min) &
                                     ".");
               Ada.Text_IO.Put_Line (Image (My_Q));
               return;
            end if;
            Expected := Min + 5;

--            exit when Min > Queue_Size/10;
--             if Expected >= 0 then
--                Ada.Text_IO.Put_Line (Image (My_Q));
--                Ada.Text_IO.Skip_Line;
--             end if;
         end loop;

      exception
         when Queue_Empty =>
            null;
      end;
   end loop;


   --  Make the queue empty.
--    declare
--       Min      : My_Int;
--    begin
--       loop
--          Delete_Min (My_Q, Min);
--          Ada.Text_IO.Put_Line ("Heap_Test: Delete_Min: " &
--                                My_Int'Image (Min));
--       end loop;

--    exception
--       when QUEUE_EMPTY =>
--          null;
--    end;
end Concurrent_Heap_Test;
