-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - An implementation of the lock-free skip-list
--                              algorithm by H. Sundell.
--
--  Copyright (C) 2007  Anders Gidenstam
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_priority_queues.ads
--  Description     : Lock-free priority queue based on Håkan Sundell,
--                    "
--                    ",
--  Author          : Anders Gidenstam
--  Created On      : Wed Jun  6 15:26:34 2007
--  $Id: nbada-lock_free_priority_queues.ads,v 1.5 2007/10/31 10:47:56 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter;
with NBAda.Process_Identification;

generic

   type Element_Type is private;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   --  Note: Element_Type must be totally ordered.

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Priority_Queues is

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

   procedure Verify (Queue : in out Priority_Queue_Type;
                     Print : in     Boolean := False);
   --  Should only be called when the deque is idle.

private

   Max_Levels : constant := 3;

   package MR_Adapter is
      new Lock_Free_Priority_Queues_Memory_Reclamation_Adapter (Process_Ids,
                                                                Max_Levels);
   package LFMR renames MR_Adapter.Memory_Reclamation;

   type List_Node_Reference is new LFMR.Shared_Reference_Base;
   --  List_Node_Reference is an atomic type since Shared_Reference_Base is.

   type List_Level is range 0 .. Max_Levels;

   type List_Node_Reference_Array is
      array (List_Level range 1 .. Max_Levels) of aliased List_Node_Reference;

   type Atomic_Boolean is new Boolean;
   for Atomic_Boolean'Size use 32;

   type List_Node is --  (Max_Level : Level) is
     new LFMR.Managed_Node_Base with
      record
         Max_Level   : aliased List_Level;
         pragma Atomic (Max_Level);
         Valid_Level : aliased List_Level := 0;
         pragma Atomic (Valid_Level);
         Next        : List_Node_Reference_Array;
         Previous    : aliased List_Node_Reference;
         pragma Atomic (Previous);
         Deleted     : aliased Atomic_Boolean := False;
         pragma Atomic (Deleted);
         Value       : Element_Type;
      end record;

   procedure Dispose  (Node       : access List_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access List_Node);

   function All_References (Node : access List_Node)
                           return LFMR.Reference_Set;

   procedure Free     (Node : access List_Node);

   package LFMR_Ops is new LFMR.Operations (List_Node,
                                            List_Node_Reference);

   subtype List_Node_Access is LFMR_Ops.Private_Reference;

   type Priority_Queue_Access is access all Priority_Queue_Type;

   type Mutable_View (Self : access Priority_Queue_Type) is
     limited null record;

   type Priority_Queue_Type is limited
      record
         Head    : aliased List_Node_Reference;
         pragma Atomic (Head);
         Tail    : aliased List_Node_Reference;
         pragma Atomic (Tail);
         Mutable : Mutable_View (Priority_Queue_Type'Access);
      end record;


end NBAda.Lock_Free_Priority_Queues;
