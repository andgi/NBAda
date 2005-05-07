-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : example_queue.adb
--  Description     : Simple example ADT for lock-free garbage collector.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $ID$
-------------------------------------------------------------------------------

package body Example_Queue is

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

   procedure Init (Queue : in out Queue_Type) is
      Node : Node_Access := new Queue_Node;
   begin
      Store (Queue.Head'Access, Node);
      Store (Queue.Tail'Access, Node);
   end Init;

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

   procedure Enqueue (Queue : in out Queue_Type;
                      Value : in     Value_Type) is
      Node : Queue_Node_Access := new Queue_Node;
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
