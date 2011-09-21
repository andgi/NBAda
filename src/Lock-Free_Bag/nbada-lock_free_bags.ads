-------------------------------------------------------------------------------
--  Lock-free bag - An implementation of  the lock-free bag algorithm
--  by H. Sundell, A. Gidenstam, M. Papatriantafilou and P. Tsigas.
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
--  Filename        : nbada-lock_free_bags.ads
--  Description     : A lock-free bag algorithm based on
--                    H. Sundell, A. Gidenstam, M. Papatriantafilou and
--                    P. Tsigas,
--                    "A Lock-Free Algorithm for Concurrent Bags",
--                    Proceedings of the 23rd Annual Symposium on Parallelism
--                    in Algorithms and Architectures (SPAA 2011),
--                    pages 335 - 344, ACM, 2011. 
--  Author          : Anders Gidenstam
--  Created On      : Thu Aug 02 15:53:00 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Per_Task_Storage.Shared;
with NBAda.Per_Task_Storage.Local;
with NBAda.Primitives;
with NBAda.Lock_Free_Bags_Memory_Reclamation_Adapter;

generic

   type Element_Type is private;
   --  The Element_Type must be atomic and Element_Type'Object_Size must be
   --  equal to System.Word_Size.

   Null_0 : Element_Type;
   --  NOTE: This value MUST NOT appear as data values in the bag.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Bags is

   ----------------------------------------------------------------------------
   --  Lock-free Bag.
   ----------------------------------------------------------------------------
   type Bag_Type is limited private;

   function  Try_Remove_Any (From    : access Bag_Type;
                             Result  : access Element_Type) return Boolean;
   procedure Insert       (On      : in out Bag_Type;
                           Element : in     Element_Type);

private

   package MR_Adapter is
      new Lock_Free_Bags_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   Block_Size : constant := 62;
   type Element_Index is new Integer range -1 .. Block_Size;

   type Atomic_Element_Array is
     array (Element_Index range 0 .. Block_Size - 1) of aliased Element_Type;
   --  pragma Atomic_Components(Atomic_Element_Array);

   type Bag_Node_Reference is
     new NBAda.Memory_Reclamation.Shared_Reference_Base;
   type Bitfield is new Primitives.Standard_Unsigned;

   type Bag_Node is
     new MR.Managed_Node_Base with
      record
         Element : Atomic_Element_Array := (others => Null_0);
         Notify  : aliased Bitfield;
         pragma Atomic (Notify);
         Next    : aliased Bag_Node_Reference;
         pragma Atomic (Next);
      end record;

   procedure Free (Node : access Bag_Node);

   package MR_Ops is new MR.Reference_Operations
     (Managed_Node     => Bag_Node,
      Shared_Reference => Bag_Node_Reference);

   type Thread_Shared is
      limited record
         Head : aliased Bag_Node_Reference;
         pragma Atomic (Head);
      end record;
   type Thread_Shared_Access is access all Thread_Shared;

   package TSS is
      new NBAda.Per_Task_Storage.Shared (Thread_Shared,
                                         Process_Ids);

   type Thread_Local is
      limited record
         Own_Block        : MR_Ops.Private_Reference := MR_Ops.Null_Reference;
         Own_Index        : Element_Index            := 0;
         Steal_Thread     : Process_Ids.Process_ID_Type
           := Process_Ids.Process_ID_Type'First;
         Steal_Block      : MR_Ops.Private_Reference := MR_Ops.Null_Reference;
         Prev_Steal_Block : MR_Ops.Private_Reference := MR_Ops.Null_Reference;
         Steal_Index      : Element_Index            := Block_Size;
      end record;
   type Thread_Local_Access is access all Thread_Local;

   package TLS is
      new NBAda.Per_Task_Storage.Local (Thread_Local,
                                        Process_Ids);

   type Bag_Type is
      limited record
         MM            : MR_Ops.Memory_Manager;
         Thread_Shared : TSS.Storage;
         Thread_Local  : TLS.Storage;
      end record;

end NBAda.Lock_Free_Bags;
