-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : queue_test.adb
-- Description     : Example application for lock-free reference counting.
-- Author          : Anders Gidenstam
-- Created On      : Wed Apr 13 22:09:40 2005
-------------------------------------------------------------------------------

with Lockfree_Reference_Counting;
with Process_Identification;
with Primitives;

with Ada.Text_IO;
with Ada.Exceptions;

procedure Queue_Test is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 10);

   package LFRC is
      new Lockfree_Reference_Counting (Max_Number_Of_Dereferences   => 4,
                                       Max_Number_Of_Links_Per_Node => 1,
                                       Process_Ids                  => PID);
   use LFRC;

   type Queue_Node_Reference is new LFRC.Shared_Reference;

   type Queue_Node is new LFRC.Reference_Counted_Node with
      record
         Next  : aliased Queue_Node_Reference;
         Value : Integer;
      end record;
   type Queue_Node_Access is access all Queue_Node;

   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Queue_Node);

   Head : aliased Queue_Node_Reference;
   Tail : aliased Queue_Node_Reference;

   Queue_Empty : exception;

   procedure Init;
   function  Dequeue return Integer;
   procedure Enqueue (Value : in     Integer);

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      Next : Node_Access;
   begin
      if not Concurrent then
         Store (Node.Next'Access, null);
      else
         loop
            Next := Deref (Node.Next'Access);
            exit when Compare_And_Swap (Link      => Node.Next'Access,
                                        Old_Value => Next,
                                        New_Value => null);
            Release (Next);
         end loop;
         Release (Next);
      end if;
   end Dispose;

   procedure Clean_Up (Node : access Queue_Node) is
      Node1, Node2 : Queue_Node_Access;
   begin
      loop
         Node1 := Queue_Node_Access (Deref (Node.Next'Access));
         if Node1 /= null and then Is_Deleted (Node1) then
            Node2 := Queue_Node_Access (Deref (Node1.Next'Access));
            if Compare_And_Swap (Link      => Node.Next'Access,
                                 Old_Value => Node_Access (Node1),
                                 New_Value => Node_Access (Node2))
            then
              null;
            end if;
            Release (Node_Access (Node1));
            Release (Node_Access (Node2));
         else
            Release (Node_Access (Node1));
            exit;
         end if;
      end loop;
   end Clean_Up;

   procedure Init is
      Node : Node_Access := new Queue_Node;
   begin
      Store (Head'Access, Node);
      Store (Tail'Access, Node);
   end Init;

   function  Dequeue return Integer is
      Node : Queue_Node_Access;
      Next : Queue_Node_Access;
      Res  : Integer;
   begin
      loop
         Node := Queue_Node_Access (Deref (Head'Access));
         Next := Queue_Node_Access (Deref (Node.Next'Access));
         if Next = null then
            Release (Node_Access (Node));
            raise Queue_Empty;
         end if;
         exit when Compare_And_Swap (Link      => Head'Access,
                                     Old_Value => Node_Access (Node),
                                     New_Value => Node_Access (Next));
         Release (Node_Access (Node));
         Release (Node_Access (Next));
      end loop;
      Delete (Node_Access (Node));
      Res := Next.Value;
      Release (Node_Access (Next));
      return Res;
   end Dequeue;

   procedure Enqueue (Value : in     Integer) is
      Node : Queue_Node_Access := new Queue_Node;
      Old, Prev, Prev2 : Queue_Node_Access;
   begin
      Node.Value := Value;
      Old  := Queue_Node_Access (Deref (Tail'Access));
      Prev := Old;
      loop
         loop
            Prev2 := Queue_Node_Access (Deref (Prev.Next'Access));
            exit when Prev2 = null;

            if Old /= Prev then
               Release (Node_Access (Prev));
            end if;

            Prev := Prev2;
         end loop;

         exit when Compare_And_Swap (Link      => Prev.Next'Access,
                                     Old_Value => null,
                                     New_Value => Node_Access (Node));
      end loop;
      declare
         Dummy : Boolean;
      begin
         Dummy := Compare_And_Swap (Link      => Tail'Access,
                                    Old_Value => Node_Access (Old),
                                    New_Value => Node_Access (Node));
      end;
      if Old /= Prev then
         Release (Node_Access (Prev));
      end if;
      Release (Node_Access (Old));
      Release (Node_Access (Node));
   end Enqueue;


   ----------------------------------------------------------------------------
   task type Producer;
   task type Consumer;

   Enqueue_Count        : aliased Primitives.Unsigned_32 := 0;
   Dequeue_Count        : aliased Primitives.Unsigned_32 := 0;
   No_Producers_Running : aliased Primitives.Unsigned_32 := 0;

   ----------------------------------------------------------------------------
   task body Producer is
   begin
      PID.Register;
      Primitives.Fetch_And_Add (No_Producers_Running'Access, 1);
      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
      begin
         for I in 1 .. 10_000 loop
            --Ada.Text_IO.Put_Line ("Enqueue(" & Integer'Image (I) & ")");
            Enqueue (I);
            Primitives.Fetch_And_Add (Enqueue_Count'Access, 1);
         end loop;

      exception
         when E: others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Producer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;
      declare
         use type Primitives.Unsigned_32;
      begin
         Primitives.Fetch_And_Add (No_Producers_Running'Access, -1);
      end;
   end Producer;

   ----------------------------------------------------------------------------
   task body Consumer is
   begin
      PID.Register;

      declare
         ID  : constant PID.Process_ID_Type := PID.Process_ID;
         I : Integer := 1;
      begin
         loop

            begin
               if Dequeue /= I then
                  null;
                  --Ada.Text_IO.Put_Line ("Queue FIFO property violated!");
               else
                  I := I + 1;
               end if;
               Primitives.Fetch_And_Add (Dequeue_Count'Access, 1);

            exception
               when Queue_Test.Queue_Empty =>
                  delay 0.1;
                  declare
                     use type Primitives.Unsigned_32;
                  begin
                     exit when No_Producers_Running = 0;
                  end;
            end;
         end loop;

      exception
         when E: others =>
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
            Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                  "Consumer (" &
                                  PID.Process_ID_Type'Image (ID) &
                                  "): raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E));
            Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
      end;
   end Consumer;

begin
   PID.Register;

   Ada.Text_IO.Put ("Initializing: ");
   Init;
   Ada.Text_IO.Put_Line (" Queue ");

--     for I in 1 ..10 loop
--        Ada.Text_IO.Put_Line ("Enqueue(" & Integer'Image (I) & ")");
--        Enqueue (I);
--     end loop;
--     for I in 1 ..10 loop
--         Ada.Text_IO.Put_Line ("Dequeue() = " & Integer'Image (Dequeue));
--     end loop;

   Ada.Text_IO.Put_Line ("Testing with producer/consumer tasks.");
   declare
      P1, P2, P3, P4, P5, P6, P7, P8 : Producer;
      C1, C2, C3, C4 : Consumer;
   begin
      null;
   end;
   Ada.Text_IO.Put_Line ("Emptying queue.");

   begin
      loop
         Ada.Text_IO.Put_Line ("Dequeue() = " & Integer'Image (Dequeue));
         Primitives.Fetch_And_Add (Dequeue_Count'Access, 1);
      end loop;
   exception
      when E: others =>
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);
         Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                               "raised " &
                               Ada.Exceptions.Exception_Name (E) &
                               " : " &
                               Ada.Exceptions.Exception_Message (E));
         Ada.Text_IO.New_Line (Ada.Text_IO.Standard_Error);

         Ada.Text_IO.Put_Line ("Final enqueue count: " &
                               Primitives.Unsigned_32'Image (Enqueue_Count));
         Ada.Text_IO.Put_Line ("Final dequeue count: " &
                               Primitives.Unsigned_32'Image (Dequeue_Count));
   end;
end Queue_Test;
