-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : concurrent_heap_test3.adb
-- Description     : Test of non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Mon Mar 17 17:09:08 2003
-- $Id: concurrent_heap_test3.adb,v 1.1 2003/03/24 15:30:54 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with Ada.Real_Time;

with Non_Blocking_Priority_Queue;
with Priority_Queue;
with Primitives;

procedure Concurrent_Heap_Test3 is

   type My_Int is new Integer;
   pragma Atomic (My_Int);

   package My_NB_Queue is new
     Non_Blocking_Priority_Queue (Element_Type => My_Int,
                                  ">"          => ">",
                                  Image        => My_Int'Image);
   use My_NB_Queue;

   package My_B_Queue is new
     Priority_Queue (Element_Type => My_Int,
                     ">"          => ">",
                     Min_Element  => My_Int'First);
   use My_B_Queue;

   type Unsigned_32_Array is array (Integer range <>) of
     aliased Primitives.Unsigned_32;
   pragma Atomic_Components (Unsigned_32_Array);

   ----------------------------------------------------------------------------
   Iterations   : constant := 5;
   Queue_Size   : constant := 500;
   Max_Number   : constant := Queue_Size * 5;

   Debug        : constant Boolean := False;

   ----------------------------------------------------------------------------
   My_NB_PQ : My_NB_Queue.Priority_Queue_Type (Queue_Size);

   Inserted : Unsigned_32_Array (0 .. Max_Number) := (others => 0);
   Consumed : Unsigned_32_Array (0 .. Max_Number) := (others => 0);

   Finished : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Finished);

   ----------------------------------------------------------------------------
   protected My_Blocking_PQ is
      procedure Insert     (Element : in     My_Int);
      procedure Delete_Min (Element :    out My_Int);

   private
      Priority_Queue : My_B_Queue.Priority_Queue_Type (Queue_Size);
   end My_Blocking_PQ;

   task type NB_Producer (Max    : My_Int;
                          Factor : My_Int;
                          Offset : My_Int);
   task type NB_Consumer (No_Producers : Natural);

   task type B_Producer (Max    : My_Int;
                         Factor : My_Int;
                         Offset : My_Int);
   task type B_Consumer (No_Producers : Natural);

   ----------------------------------------------------------------------------
   protected body My_Blocking_PQ is
      procedure Insert     (Element : in     My_Int) is
      begin
         Insert (Priority_Queue,
                 Element);
      end Insert;

      procedure Delete_Min (Element :    out My_Int) is
      begin
         Delete_Min (Priority_Queue,
                     Element);
      end Delete_Min;
   end My_Blocking_PQ;

   task body NB_Producer is
   begin
      for X in 1 .. Iterations loop
         begin
            for I in reverse My_Int(0) .. Max - 1 loop
               declare
                  Item : constant My_Int := Factor * I + Offset;
               begin
                  if Debug then
                     Ada.Text_IO.Put_Line ("NB_Producer: Insert (" &
                                           My_Int'Image (Item) & ")");
                  end if;
                  Insert (My_NB_PQ, Item);
                  Primitives.Fetch_And_Add (Inserted (Integer (Item))'Access,
                                            1);
               end;
            end loop;

         exception
            when Queue_Full =>
               delay 0.0;
         end;
      end loop;

      Primitives.Fetch_And_Add (Finished'Access,
                                1);
      Ada.Text_IO.Put_Line ("NB_Producer exited.");
   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "NB_Producer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end NB_Producer;

   task body NB_Consumer is
      use type Primitives.Unsigned_32;

      Min      : My_Int;
   begin
      loop
         begin
            Delete_Min (My_NB_PQ, Min);
            Primitives.Fetch_And_Add (Consumed (Integer (Min))'Access,
                                      1);
            if Debug then
               Ada.Text_IO.Put_Line ("NB_Consumer: Delete_Min: " &
                                     My_Int'Image (Min));
            end if;

         exception
            when Queue_Empty =>
               exit when Natural (Finished) = No_Producers;
               delay 0.0;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("NB_Consumer exited.");

   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "NB_Consumer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end NB_Consumer;

   task body B_Producer is
   begin
      for X in 1 .. Iterations loop
         begin
            for I in reverse My_Int(0) .. Max - 1 loop
               declare
                  Item : constant My_Int := Factor * I + Offset;
               begin
                  if Debug then
                     Ada.Text_IO.Put_Line ("B_Producer: Insert (" &
                                           My_Int'Image (Item) & ")");
                  end if;
                  My_Blocking_PQ.Insert (Item);
                  Primitives.Fetch_And_Add (Inserted (Integer (Item))'Access,
                                            1);
               end;
            end loop;

         exception
            when Overflow =>
               delay 0.0;
         end;
      end loop;

      Primitives.Fetch_And_Add (Finished'Access,
                                1);
      Ada.Text_IO.Put_Line ("B_Producer exited.");
   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "B_Producer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         --Ada.Text_IO.Put_Line (Image (My_Q));
   end B_Producer;

   task body B_Consumer is
      use type Primitives.Unsigned_32;

      Min      : My_Int;
   begin
      loop
         begin
            My_Blocking_PQ.Delete_Min (Min);
            Primitives.Fetch_And_Add (Consumed (Integer (Min))'Access,
                                      1);
            if Debug then
               Ada.Text_IO.Put_Line ("B_Consumer: Delete_Min: " &
                                     My_Int'Image (Min));
            end if;

         exception
            when Underflow =>
               exit when Natural (Finished) = No_Producers;
               delay 0.0;
         end;
      end loop;
      Ada.Text_IO.Put_Line ("Consumer exited.");

   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "B_Consumer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end B_Consumer;

   use Primitives;
   use type Ada.Real_Time.Time;

   T1, T2 : Ada.Real_Time.Time;
begin
   -- Blocking run.
   for X in 1 .. 1 loop
      Primitives.Membar;
      Finished := 0;
      Inserted := (others => 0);
      Consumed := (others => 0);
      Primitives.Membar;
      T1 := Ada.Real_Time.Clock;
      declare
         B_P1  : B_Producer (Queue_Size/5, 20, 0);
         B_P2  : B_Producer (Queue_Size/5, 20, 1);
         B_P3  : B_Producer (Queue_Size/5, 20, 2);
         B_P4  : B_Producer (Queue_Size/5, 20, 3);
         B_P5  : B_Producer (Queue_Size/5, 20, 4);
         B_P6  : B_Producer (Queue_Size/5, 20, 5);
         B_P7  : B_Producer (Queue_Size/5, 20, 6);
         B_P8  : B_Producer (Queue_Size/5, 20, 7);
         B_P9  : B_Producer (Queue_Size/5, 20, 8);
         B_P10 : B_Producer (Queue_Size/5, 20, 9);
         B_P11 : B_Producer (Queue_Size/5, 20, 10);
         B_P12 : B_Producer (Queue_Size/5, 20, 11);
         B_P13 : B_Producer (Queue_Size/5, 20, 12);
         B_P14 : B_Producer (Queue_Size/5, 20, 13);
         B_P15 : B_Producer (Queue_Size/5, 20, 14);
         B_P16 : B_Producer (Queue_Size/5, 20, 15);

         B_C1  : B_Consumer (16);
         B_C2  : B_Consumer (16);
         B_C3  : B_Consumer (16);
         B_C4  : B_Consumer (16);
      begin
         null;
      end;
      Primitives.Membar;
      T2 := Ada.Real_Time.Clock;


      -- Evaluate run.
      for I in Inserted'Range loop
         if Inserted (I) /= Consumed (I) then
            Ada.Text_IO.Put_Line ("B: Inserts vs deletes mismatch for " &
                                  Integer'Image (I) & ":" &
                                  Unsigned_32'Image (Inserted (I)) & " /=" &
                                  Unsigned_32'Image (Consumed (I)) & ".");
         end if;
      end loop;
      Ada.Text_IO.Put_Line
        ("B: Total time: " &
         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));

   end loop;

   -- Non-blocking run.
   for X in 1 .. 1 loop
      Primitives.Membar;
      Finished := 0;
      Inserted := (others => 0);
      Consumed := (others => 0);
      Primitives.Membar;
      T1 := Ada.Real_Time.Clock;
      declare
         NB_P1  : NB_Producer (Queue_Size/5, 20, 0);
         NB_P2  : NB_Producer (Queue_Size/5, 20, 1);
         NB_P3  : NB_Producer (Queue_Size/5, 20, 2);
         NB_P4  : NB_Producer (Queue_Size/5, 20, 3);
         NB_P5  : NB_Producer (Queue_Size/5, 20, 4);
         NB_P6  : NB_Producer (Queue_Size/5, 20, 5);
         NB_P7  : NB_Producer (Queue_Size/5, 20, 6);
         NB_P8  : NB_Producer (Queue_Size/5, 20, 7);
         NB_P9  : NB_Producer (Queue_Size/5, 20, 8);
         NB_P10 : NB_Producer (Queue_Size/5, 20, 9);
         NB_P11 : NB_Producer (Queue_Size/5, 20, 10);
         NB_P12 : NB_Producer (Queue_Size/5, 20, 11);
         NB_P13 : NB_Producer (Queue_Size/5, 20, 12);
         NB_P14 : NB_Producer (Queue_Size/5, 20, 13);
         NB_P15 : NB_Producer (Queue_Size/5, 20, 14);
         NB_P16 : NB_Producer (Queue_Size/5, 20, 15);

         NB_C1  : NB_Consumer (16);
         NB_C2  : NB_Consumer (16);
         NB_C3  : NB_Consumer (16);
         NB_C4  : NB_Consumer (16);
      begin
         null;
      end;
      Primitives.Membar;
      T2 := Ada.Real_Time.Clock;

      -- Evaluate run.
      for I in Inserted'Range loop
         if Inserted (I) /= Consumed (I) then
            Ada.Text_IO.Put_Line ("NB: Inserts vs deletes mismatch for " &
                                  Integer'Image (I) & ":" &
                                  Unsigned_32'Image (Inserted (I)) & " /=" &
                                  Unsigned_32'Image (Consumed (I)) & ".");
         end if;
      end loop;
      Ada.Text_IO.Put_Line
        ("NB: Total time: " &
         Duration'Image (Ada.Real_Time.To_Duration (T2 - T1)));
   end loop;


end Concurrent_Heap_Test3;
