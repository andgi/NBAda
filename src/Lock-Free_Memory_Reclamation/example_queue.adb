-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : example_queue.adb
--  Description     : Simple example ADT for lock-free garbage collector.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $Id: example_queue.adb,v 1.4 2005/06/20 16:49:17 anders Exp $
-------------------------------------------------------------------------------

with Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Example_Queue is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function Create_Queue_Node is new LFRC.Create (Queue_Node,
                                                  New_Queue_Node_Access);

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

   ----------------------------------------------------------------------------
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
            Release (Node_Access (Node2));
            Release (Node_Access (Node1));
         else
            Release (Node_Access (Node1));
            exit;
         end if;
      end loop;
   end Clean_Up;

   -------------------------------------------------------------------------
   procedure Free (Node : access Queue_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Queue_Node,
                                    New_Queue_Node_Access);
      function To_New_Queue_Node_Access is new
        Ada.Unchecked_Conversion (Queue_Node_Access,
                                  New_Queue_Node_Access);

      X : New_Queue_Node_Access :=
        To_New_Queue_Node_Access (Queue_Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Object_Value type from the Object_Value_Access2 pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   procedure Init (Queue : in out Queue_Type) is
      Node : constant Node_Access := Create_Queue_Node;
   begin
      Store (Queue.Head'Access, Node);
      Store (Queue.Tail'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   function  Dequeue (Queue : access Queue_Type) return Value_Type is
      Node : Queue_Node_Access;
      Next : Queue_Node_Access;
      Res  : Value_Type;
   begin
      loop
         Node := Queue_Node_Access (Deref (Queue.Head'Access));
         Next := Queue_Node_Access (Deref (Node.Next'Access));
         if Next = null then
            Release (Node_Access (Node));
            raise Queue_Empty;
         end if;
         exit when Compare_And_Swap (Link      => Queue.Head'Access,
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

   ----------------------------------------------------------------------------
   procedure Enqueue (Queue : in out Queue_Type;
                      Value : in     Value_Type) is
      Node : Queue_Node_Access := Queue_Node_Access (Create_Queue_Node);
      Old, Prev, Prev2 : Queue_Node_Access;
   begin
      Node.Value := Value;
      Old  := Queue_Node_Access (Deref (Queue.Tail'Access));
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
         Dummy := Compare_And_Swap (Link      => Queue.Tail'Access,
                                    Old_Value => Node_Access (Old),
                                    New_Value => Node_Access (Node));
      end;

      if Old /= Prev then
         Release (Node_Access (Prev));
      end if;

      Release (Node_Access (Old));
      Release (Node_Access (Node));
   end Enqueue;

end Example_Queue;
