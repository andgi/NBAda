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
--  Filename        : nbada-lock_free_queues.ads
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

with NBAda.Process_Identification;
with NBAda.Per_Task_Storage.Local;
with NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter;

generic

   type Element_Type is private;
   --  The Element_Type must be atomic and Element_Type'Object_Size must be
   --  equal to System.Word_Size.

   Null_0 : Element_Type;
   Null_1 : Element_Type;
   --  NOTE: These two values MUST be different and MUST NOT appear as
   --        data values in the queue.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Queues is

   ----------------------------------------------------------------------------
   --  Lock-free Queue.
   ----------------------------------------------------------------------------
   type Queue_Type is limited private;

   Queue_Empty : exception;

   procedure Init    (Queue : in out Queue_Type);
   function  Dequeue (From : access Queue_Type) return Element_Type;
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type);

--private

   procedure Verify (Queue : in out Queue_Type;
                     Print : in     Boolean := False);
   --  Should only be called when the deque is idle.

private

   package MR_Adapter is
      new Lock_Free_Queues_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   Block_Size : constant := 32;
   type Element_Index is new Natural range 0 .. Block_Size;

   type Atomic_Element_Array is
     array (Element_Index range 0 .. Block_Size - 1) of aliased Element_Type;
   --  pragma Atomic_Components(Atomic_Element_Array);

   type Atomic_Boolean is new Boolean;
   for Atomic_Boolean'Object_Size use 32;

   type Queue_Node_Reference is new MR.Shared_Reference_Base;

   type Queue_Node is
     new MR.Managed_Node_Base with
      record
         Element : Atomic_Element_Array := (others => Null_0);
         Head    : Element_Index := 0;
         pragma Atomic (Head);
         Tail    : Element_Index := 0;
         pragma Atomic (Tail);
         Deleted : aliased Atomic_Boolean := False;
         pragma Atomic (Deleted);
         Next    : aliased Queue_Node_Reference;
         pragma Atomic (Next);
      end record;

   procedure Free (Node : access Queue_Node);

   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Queue_Node);

   function All_References (Node : access Queue_Node)
                           return MR.Reference_Set;

   package MR_Ops is new MR.Operations
     (Managed_Node     => Queue_Node,
      Shared_Reference => Queue_Node_Reference);

   type Thread_Local is
      limited record
         Head_Block : MR_Ops.Private_Reference := MR_Ops.Null_Reference;
         Tail_Block : MR_Ops.Private_Reference := MR_Ops.Null_Reference;
         Head       : Element_Index := 0;
         Tail       : Element_Index := 0;
      end record;
   type Thread_Local_Access is access all Thread_Local;

   package TLS is
      new NBAda.Per_Task_Storage.Local (Thread_Local,
                                        Process_Ids);

   type Queue_Type is
     limited record
        Head_Block   : aliased Queue_Node_Reference;
        pragma Atomic (Head_Block);
        Tail_Block   : aliased Queue_Node_Reference;
        pragma Atomic (Tail_Block);
        Thread_Local : TLS.Storage;
     end record;

end NBAda.Lock_Free_Queues;
