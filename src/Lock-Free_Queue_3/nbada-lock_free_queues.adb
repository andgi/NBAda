-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of  the cache-aware lock-free queue
--  algorithm by A. Gidenstam, H. Sundell and P. Tsigas.
--  Copyright (C) 2011  Anders Gidenstam
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
-------------------------------------------------------------------------------
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_queues.adb
--  Description     : A lock-free queue algorithm based on
--                    A. Gidenstam, H. Sundell and P. Tsigas,
--                    "Cache-Aware Lock-Free Queues for Multiple
--                     Producers/Consumers and Weak Memory Consistency",
--                    The 14th International Conference
--                    On the Principles Of Distributed Systems (OPODIS'10),
--                    LNCS 6490, pp. 302-317, 2011.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jun 07 19:49:28 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

with Ada.Text_IO;

package body NBAda.Lock_Free_Queues is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Queue_Node'Max_Size_In_Storage_Elements);

   type New_Queue_Node_Access is access Queue_Node;
   for New_Queue_Node_Access'Storage_Pool use Node_Pool;

   function New_Node is new MR_Ops.Create
     (User_Node_Access => New_Queue_Node_Access);

   procedure Init_Thread (Queue : in out Queue_Type;
                          Local : access Thread_Local);

   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap
      (Element => Element_Type);

   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap
      (Element => Atomic_Boolean);

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Queue : in out Queue_Type) is
      use MR_Ops;
      Node :  constant Private_Reference := New_Node;
   begin
      Store (Queue.Head_Block'Access, Node);
      Store (Queue.Tail_Block'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   function  Dequeue (From : access Queue_Type) return Element_Type is
      use MR_Ops;
      Thread : constant Thread_Local_Access :=
        Thread_Local_Access (TLS.Get (From.Thread_Local));
   begin
      if ("+"(Thread.Head_Block) = null) then
         Init_Thread (From.all, Thread);
      end if;

      declare
         Block : Private_Reference := Thread.Tail_Block;
         Tail  : Element_Index     := Thread.Tail;
      begin
         loop
            if Tail = Block_Size then
               declare
                  Old_Block : constant Private_Reference := Block;
               begin
                  "+"(Old_Block).Tail := Tail;
                  Block := Dereference ("+"(Old_Block).Next'Access);

                  if "+"(Block) = null then
                     Release (Block);
                     raise Queue_Empty;
                  end if;

                  if not "+"(Old_Block).Deleted then
                     while
                       From.Tail_Block /= Old_Block and
                       not Boolean ("+"(Old_Block).Deleted)
                     loop
                        declare
                           Tail : constant Private_Reference :=
                             Dereference (From.Tail_Block'Access);
                        begin
                           if
                             "+"(Tail).Next = Old_Block and then
                             Compare_And_Swap (From.Tail_Block'Access,
                                               Old_Value => Tail,
                                               New_Value => Old_Block)
                           then
                              Delete (Tail);
                           else
                              Release (Tail);
                           end if;
                        end;
                     end loop;
                     if
                       Compare_And_Swap ("+"(Old_Block).Deleted'Access,
                                         Old_Value => False,
                                         New_Value => True) and then
                       Compare_And_Swap (From.Tail_Block'Access,
                                         Old_Value => Old_Block,
                                         New_Value => Block)
                     then
                        Delete (Old_Block);
                     else
                        Release (Old_Block);
                     end if;
                  else
                     Release (Old_Block);
                  end if;
                  if "+"(Block).Deleted then
                     Release (Block);
                     Block := Dereference (From.Tail_Block'Access);
                  end if;
                  Thread.Tail_Block := Block;
                  Tail              := "+"(Block).Tail;
               end;
            else
               declare
                  Result : constant Element_Type := "+"(Block).Element (Tail);
               begin
                  if Result = Null_1 then
                     Tail := Tail + 1;
                  elsif
                    Result = Null_0 and then
                    Compare_And_Swap ("+"(Block).Element (Tail)'Access,
                                      Old_Value => Null_0,
                                      New_Value => Null_0)
                  then
                     Thread.Tail := Tail;
                     raise Queue_Empty;
                  elsif
                    Compare_And_Swap ("+"(Block).Element (Tail)'Access,
                                      Old_Value => Result,
                                      New_Value => Null_1)
                  then
                     Thread.Tail := Tail + 1;
                     return Result;
                  end if;
               end;
            end if;
         end loop;
      end;
   end Dequeue;

   ----------------------------------------------------------------------------
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type) is
      use MR_Ops;
      Thread : constant Thread_Local_Access :=
        Thread_Local_Access (TLS.Get (On.Thread_Local));
   begin
      if ("+"(Thread.Head_Block) = null) then
         Init_Thread (On, Thread);
      end if;

      declare
         Block : Private_Reference := Thread.Head_Block;
         Head  : Element_Index     := Thread.Head;
      begin
         loop
            if Head = Block_Size then
               declare
                  Old_Block : constant Private_Reference := Block;
               begin
                  "+"(Old_Block).Head := Head;
                  Block := Dereference ("+"(Old_Block).Next'Access);
                  if "+"(Block) = null then
                     Release (Block);
                     Block := New_Node;

                     while
                       On.Head_Block /= Old_Block and then
                       "+"(Old_Block).Next = Null_Reference
                     loop
                        declare
                           Head : constant Private_Reference :=
                             Dereference (On.Head_Block'Access);
                        begin
                           if
                             "+"(Head).Next /= Old_Block or else
                             Compare_And_Swap (On.Head_Block'Access,
                                               Old_Value => Head,
                                               New_Value => Old_Block)
                           then
                              Release (Head);
                              exit;
                           else
                              Release (Head);
                           end if;
                        end;
                     end loop;
                     if
                       Compare_And_Swap ("+"(Old_Block).Next'Access,
                                         Old_Value => Null_Reference,
                                         New_Value => Block)
                     then
                        Compare_And_Swap (On.Head_Block'Access,
                                          Old_Value => Old_Block,
                                          New_Value => Block);
                     else
                        Delete (Block);
                        Block := Dereference ("+"(Old_Block).Next'Access);
                     end if;
                  elsif "+"(Block).Head = Block_Size and
                        "+"(Block).Next /= Null_Reference
                  then
                     Release (Block);
                     Block := Dereference (On.Head_Block'Access);
                  end if;
                  Release (Old_Block);
                  Head              := "+"(Block).Head;
                  Thread.Head_Block := Block;
               end;
            elsif "+"(Block).Element (Head) = Null_0 then
               if
                 Compare_And_Swap ("+"(Block).Element (Head)'Access,
                                   Old_Value => Null_0,
                                   New_Value => Element)
               then
                  Thread.Head := Head + 1;
                  return;
               end if;
            else
               Head := Head + 1;
            end if;
         end loop;
      end;
   end Enqueue;

   ----------------------------------------------------------------------------
   procedure Verify (Queue : in out Queue_Type;
                     Print : in     Boolean := False) is
      use MR_Ops;

      function Image (Link : Queue_Node_Reference) return String;
      function Image (Node : Private_Reference) return String;

      -----------------------------------------------------------------
      function Image (Link : Queue_Node_Reference) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (Queue_Node_Reference,
                                          NBAda.Primitives.Standard_Unsigned);
         use NBAda.Primitives;
      begin
         return Standard_Unsigned'Image (To_Unsigned (Link));
      end Image;

      -----------------------------------------------------------------
      function Image (Node : Private_Reference) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                          NBAda.Primitives.Standard_Unsigned);
         use NBAda.Primitives;
      begin
         declare
            Text : constant String :=
              Standard_Unsigned'Image (To_Unsigned ("+" (Node))) &
              "@(" &
              "Next = " &
              Image ("+" (Node).Next) &
              ")";
         begin
            return
              Text;
         end;
      end Image;


      Node : Private_Reference := Dereference (Queue.Head_Block'Access);
   begin
      if Print then
         Ada.Text_IO.Put_Line ("Head = " & Image (Queue.Head_Block));
         Ada.Text_IO.Put_Line ("Tail = " & Image (Queue.Tail_Block));
      end if;
      loop
         if Print then
            Ada.Text_IO.Put_Line ("  " & Image (Node));
         end if;
         declare
            Next : constant Private_Reference :=
              Dereference ("+" (Node).Next'Access);
         begin
            exit when "+" (Next) = null;

            Release (Node);
            Node := Next;
         end;
      end loop;
      Release (Node);
   end Verify;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init_Thread (Queue : in out Queue_Type;
                          Local : access Thread_Local) is
      use MR_Ops;
   begin
      Release (Local.Head_Block);
      Release (Local.Tail_Block);
      Local.Head_Block := Dereference (Queue.Head_Block'Access);
      Local.Head       := "+"(Local.Head_Block).Head;
      Local.Tail_Block := Dereference (Queue.Tail_Block'Access);
      Local.Tail       := "+"(Local.Tail_Block).Tail;
   end Init_Thread;

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean) is
      use MR_Ops;
   begin
      if not Concurrent then
         Store (Node.Next'Access, Null_Reference);
      else
         declare
            Tmp : MR_Ops.Unsafe_Reference_Value;
         begin
            loop
               Tmp := Unsafe_Read (Node.Next'Access);
               exit when Compare_And_Swap (Node.Next'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Clean_Up (Node : access Queue_Node) is
      use MR_Ops;
      Next, Tmp, Tmp_Next : Private_Reference;
   begin
      Next := Dereference (Node.Next'Access);
      Tmp  := Dereference (Node.Next'Access);
      loop
         if "+" (Tmp) /= null and then Is_Deleted (+Tmp) then
            Tmp_Next := Dereference ("+"(Tmp).Next'Access);
            Release (Tmp);
            Tmp := Tmp_Next;
         else
            exit;
         end if;
      end loop;
      Compare_And_Swap (Node.Next'Access,
                        Old_Value => Next,
                        New_Value => Tmp);
      Release (Tmp);
      Release (Next);
   end Clean_Up;

   ----------------------------------------------------------------------------
   function All_References (Node : access Queue_Node)
                           return MR.Reference_Set is
      type Link_Access is access all Queue_Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  MR.Shared_Reference_Base_Access);

      Result : MR.Reference_Set (1 .. 1);

   begin
      Result (Integer (1)) :=
        To_Shared_Reference_Access (Node.Next'Access);

      return Result;
   end All_References;

   ----------------------------------------------------------------------------
   procedure Free (Node : access Queue_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Queue_Node,
                                    New_Queue_Node_Access);
      function To_New_Queue_Node_Access is new
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_Queue_Node_Access);
      X : New_Queue_Node_Access :=
        To_New_Queue_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Queue_Node type from the New_Queue_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Lock_Free_Queues;
