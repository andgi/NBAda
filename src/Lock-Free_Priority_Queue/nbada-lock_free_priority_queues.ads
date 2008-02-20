-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - Based on the the lock-free set algorithm by
--                              M. Michael.
--
--  Copyright (C) 2006 - 2008  Anders Gidenstam
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
--  Filename        : lock_free_priority_queues.ads
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 11:54:37 2006
--  $Id: nbada-lock_free_priority_queues.ads,v 1.1 2008/02/20 20:55:38 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;

with NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter;

generic

   type Element_Type is private;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   --  Note: Element_Type must be totally ordered.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Lock-free Priority Queue.
   ----------------------------------------------------------------------------
   type Priority_Queue_Type is limited private;

   Queue_Empty     : exception;
   Already_Present : exception;

   procedure Initialize (Queue : in out Priority_Queue_Type);

   procedure Insert  (Into    : in out Priority_Queue_Type;
                      Element : in     Element_Type);

   procedure Delete_Min (From    : in out Priority_Queue_Type;
                         Element :    out Element_Type);
   function  Delete_Min (From : in Priority_Queue_Type)
                        return Element_Type;
   function  Delete_Min (From : access Priority_Queue_Type)
                        return Element_Type;

--  private

   procedure Verify (Queue : in out Priority_Queue_Type;
                     Print : in     Boolean := False);
   --  Should only be called when the deque is idle.

   package MR_Adapter is new
     Lock_Free_Priority_Queues_Memory_Reclamation_Adapter
     (Process_Ids => Process_Ids);
   package MRS renames MR_Adapter.Memory_Reclamation;

private

   type List_Node_Reference is new MRS.Shared_Reference_Base;

   type List_Node is new MRS.Managed_Node_Base with
      record
         Next  : aliased List_Node_Reference;
         pragma Atomic (Next);
         Value : Element_Type;
      end record;

   procedure Free     (Node : access List_Node);

   package MRS_Ops is new MRS.Reference_Operations (List_Node,
                                                    List_Node_Reference);

   subtype List_Node_Access is MRS_Ops.Private_Reference;

   type Mutable_View (Self : access Priority_Queue_Type) is
     limited null record;

   type Priority_Queue_Type is limited
      record
         Head : aliased List_Node_Reference;
         pragma Atomic (Head);
         Mutable : Mutable_View (Priority_Queue_Type'Access);
      end record;

end NBAda.Lock_Free_Priority_Queues;
