-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : concurrent_heap_test2.adb
-- Description     : Test of non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Mar 13 10:09:08 2003
-- $Id: concurrent_heap_test2.adb,v 1.1 2003/03/13 14:08:15 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Text_IO;
with Ada.Exceptions;

with Non_Blocking_Priority_Queue;
with Primitives;

procedure Concurrent_Heap_Test2 is

   type My_Int is new Integer;
   pragma Atomic (My_Int);

   package My_Int_Queue is new
     Non_Blocking_Priority_Queue (Element_Type => My_Int,
                                  ">"          => ">",
                                  Image        => My_Int'Image);
   use My_Int_Queue;
   
   type Unsigned_32_Array is array (Integer range <>) of
     aliased Primitives.Unsigned_32;
   pragma Atomic_Components (Unsigned_32_Array);
   
   Iterations   : constant := 5;
   Queue_Size   : constant := 100;
   Max_Number   : constant := Queue_Size * 5;
   No_Producers : constant := 4;
   
   My_Q : Priority_Queue_Type (Queue_Size);
   
   Inserted : Unsigned_32_Array (0 .. Max_Number) := (others => 0);
   Consumed : Unsigned_32_Array (0 .. Max_Number) := (others => 0);
   
   Finished : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Finished);
   
   task type Producer (Max    : My_Int;
                       Factor : My_Int;
                       Offset : My_Int);
   task type Consumer;

   task body Producer is
   begin
      for X in 1 .. Iterations loop
	 begin
	    for I in reverse My_Int(0) .. Max - 1 loop
	       declare
		  Item : constant My_Int := Factor * I + Offset;
	       begin
		  Ada.Text_IO.Put_Line ("Insert (" &
					My_Int'Image (Item) & ")");
		  Insert (My_Q, Item);
		  Primitives.Fetch_And_Add (Inserted (Integer (Item))'Access,
					    1);
	       end;
	    end loop;
	    
	 exception
	    when Queue_Full =>
	       delay 0.1;
	 end;
      end loop;
      
      Primitives.Fetch_And_Add (Finished'Access,
				1);
      
   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Producer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         --Ada.Text_IO.Put_Line (Image (My_Q));
   end Producer;

   task body Consumer is
      use type Primitives.Unsigned_32;
      
      Min      : My_Int;
   begin
      loop
	 begin
	    Delete_Min (My_Q, Min);
	    Primitives.Fetch_And_Add (Consumed (Integer (Min))'Access,
				      1);
	    Ada.Text_IO.Put_Line ("Consumer: Delete_Min: " &
				  My_Int'Image (Min));
	 
	 exception
	    when Queue_Empty =>
	       exit when Finished = No_Producers;
	       delay 0.01;	       
	 end;
      end loop;
      
   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "Consumer: raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
   end Consumer;
   
   use Primitives;
begin
   for X in 1 .. 1 loop
      declare
--         P1 : Producer (Queue_Size, 5, 0);

--         P1 : Producer (Queue_Size/3, 10, 0);
--         P2 : Producer (Queue_Size/3, 10, 5);
         P1 : Producer (Queue_Size/5, 20, 0);
         P2 : Producer (Queue_Size/5, 20, 5);
         P3 : Producer (Queue_Size/5, 20, 10);
         P4 : Producer (Queue_Size/5, 20, 15);
	 C1 : Consumer;
	 C2 : Consumer;
      begin
         null;
      end;
      
      -- Evaluate run.
      for I in Inserted'Range loop
	 if Inserted (I) /= Consumed (I) then
	    Ada.Text_IO.Put_Line ("Inserts vs deletes mismatch for " &
				  Integer'Image (I) & ":" &
				  Unsigned_32'Image (Inserted (I)) & " /=" &
				  Unsigned_32'Image (Consumed (I)) & ".");
	 end if;
      end loop;
   end loop;
end Concurrent_Heap_Test2;
