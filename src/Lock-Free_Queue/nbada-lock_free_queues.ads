-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of Michael and Scott's lock-free queue.
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_queues.ads
--  Description     : An Ada implementation of Michael and Scott's
--                    lock-free queue algorithm.
--  Author          : Anders Gidenstam
--  Created On      : Tue Nov 28 10:55:38 2006
--  $Id: nbada-lock_free_queues.ads,v 1.6 2007/09/05 12:19:47 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter;

generic

   type Element_Type is private;
   --  Element type.

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

private

   package MR_Adapter is
      new Lock_Free_Queues_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   type Queue_Node_Reference is new MR.Shared_Reference_Base;

   type Queue_Node is
     new MR.Managed_Node_Base with
      record
         Element : Element_Type;
         Next    : aliased Queue_Node_Reference;
         pragma Atomic (Next);
      end record;

   procedure Free (Node : access Queue_Node);

   package MR_Ops is new MR.Reference_Operations
     (Managed_Node     => Queue_Node,
      Shared_Reference => Queue_Node_Reference);

   type Queue_Type is
     limited record
        Head : aliased Queue_Node_Reference;
        pragma Atomic (Head);
        Tail : aliased Queue_Node_Reference;
        pragma Atomic (Tail);
     end record;

end NBAda.Lock_Free_Queues;
