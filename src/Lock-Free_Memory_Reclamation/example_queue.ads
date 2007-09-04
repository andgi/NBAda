-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
--  Copyright (C) 2004 - 2006  Anders Gidenstam
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
--  Filename        : example_queue.ads
--  Description     : Simple example ADT for lock-free garbage reclamation
--                    scheme.
--  Author          : Anders Gidenstam
--  Created On      : Sat May  7 20:54:49 2005
--  $Id: example_queue.ads,v 1.11 2007/09/04 12:03:59 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Memory_Reclamation;
with NBAda.Process_Identification;

pragma Elaborate_All (NBAda.Lock_Free_Memory_Reclamation);

generic
   type Value_Type is private;
   --  Value type.
   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.
package Example_Queue is

   ----------------------------------------------------------------------------
   --  Lock-free Queue.
   ----------------------------------------------------------------------------
   type Queue_Type is limited private;

   Queue_Empty : exception;

   procedure Init    (Queue : in out Queue_Type);
   function  Dequeue (Queue : access Queue_Type) return Value_Type;
   procedure Enqueue (Queue : in out Queue_Type;
                      Value : in     Value_Type);

   package LFMR is new NBAda.Lock_Free_Memory_Reclamation
     (Max_Number_Of_Dereferences   => 4,
      --  Remember to account for the dereferences in the
      --  callbacks Clean_Up and Dispose (which are invoked by Delete).
      --  Here: Engueue  <= 3
      --        Dequeue  <= 3
      --        Dispose  <= 1
      --        Clean_up <= 2
      --  Delete is called from Dequeue on a dereferenced node so the
      --  maximum number of simultaneous dereferences is 4.
      --  (2 in Dequeue and 2 in Clean_Up.)
      Max_Number_Of_Links_Per_Node => 1,
      Clean_Up_Threshold           => 256,
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids,
      Debug                        => True);

private

   type Queue_Node_Reference is new LFMR.Shared_Reference_Base;

   type Queue_Node is new LFMR.Managed_Node_Base with
      record
         Next  : aliased Queue_Node_Reference;
         pragma Atomic (Next);
         Value : Value_Type;
      end record;

   procedure Dispose  (Node       : access Queue_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Queue_Node);
   procedure Free     (Node : access Queue_Node);

   package LFMR_Ops is new LFMR.Operations (Queue_Node,
                                            Queue_Node_Reference);

   subtype Queue_Node_Access is LFMR_Ops.Private_Reference;

   type Queue_Type is limited
      record
         Head : aliased Queue_Node_Reference;
         pragma Atomic (Head);
         Tail : aliased Queue_Node_Reference;
         pragma Atomic (Tail);
      end record;

end Example_Queue;
