
with Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Lock_Free_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function Create_Queue_Node is new LFRC_Ops.Create (New_Queue_Node_Access);

   ----------------------------------------------------------------------------
   procedure Insert     (Queue   : in out Lock_Free_Priority_Queue;
                         Element : in     Element_Type) is
   begin
      null;
   end Insert;

   ----------------------------------------------------------------------------
   function  Delete_Min (Queue   : access Lock_Free_Priority_Queue)
                         return Element_Type is
   begin
      null;
   end Delete_Min;

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      use LFRC_Ops;
      Next : Queue_Node_Access;
   begin
      for L in 1 .. Node.Max_Level loop
         if not Concurrent then
            Store (Node.Next (L)'Access, Null_Reference);
         else
            loop
               Next := Deref (Node.Next (L)'Access);
               exit when Compare_And_Swap (Link      => Node.Next (L)'Access,
                                           Old_Value => Next,
                                           New_Value => Null_Reference);
               Release (Next);
            end loop;
            Release (Next);
         end if;
      end loop;
      if not Concurrent then
         Store (Node.Prev'Access, Null_Reference);
      else
         loop
            Next := Deref (Node.Prev'Access);
            exit when Compare_And_Swap (Link      => Node.Prev'Access,
                                        Old_Value => Next,
                                        New_Value => Null_Reference);
            Release (Next);
         end loop;
         Release (Next);
      end if;
   end Dispose;



   ----------------------------------------------------------------------------
   procedure Clean_Up (Node : access Queue_Node) is
      use LFRC_Ops;
      Node1, Node2 : Queue_Node_Access;
   begin
      --  Cleanup next links.
      for L in 1 .. Node.Max_Level loop
        Bypass_Deleted :
        loop
           Node1 := Deref (Node.Next (L)'Access);
           if Node1 /= Null_Reference and then Is_Deleted (+Node1) then
              Node2 := Deref ("+" (Node1).Next (L)'Access);
              if Compare_And_Swap (Link      => Node.Next (L)'Access,
                                   Old_Value => Node1,
                                   New_Value => Node2)
              then
                 null;
              end if;
              Release (Node2);
              Release (Node1);
           else
              Release (Node1);
              exit Bypass_Deleted;
           end if;
        end loop Bypass_Deleted;
      end loop;
      --  Cleanup previous link.
      Bypass_Deleted2 :
      loop
         Node1 := Deref (Node.Prev'Access);
         if Node1 /= Null_Reference and then Is_Deleted (+Node1) then
            Node2 := Deref ("+" (Node1).Prev'Access);
            if Compare_And_Swap (Link      => Node.Prev'Access,
                                 Old_Value => Node1,
                                 New_Value => Node2)
            then
               null;
            end if;
            Release (Node2);
            Release (Node1);
         else
            Release (Node1);
            exit Bypass_Deleted2;
         end if;
      end loop Bypass_Deleted2;
   end Clean_Up;

   ----------------------------------------------------------------------------
   procedure Free     (Node : access Queue_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Queue_Node,
                                    New_Queue_Node_Access);
      function To_New_Queue_Node_Access is new
        Ada.Unchecked_Conversion (LFRC_Ops.Node_Access,
                                  New_Queue_Node_Access);

      X : New_Queue_Node_Access :=
        To_New_Queue_Node_Access (LFRC_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Object_Value type from the New_Queue_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end Lock_Free_Priority_Queues;
