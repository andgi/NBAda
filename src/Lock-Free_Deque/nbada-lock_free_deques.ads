-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
--
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
--  Filename        : lock_free_deques.ads
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:46:02 2006
--  $Id: nbada-lock_free_deques.ads,v 1.7 2007/09/03 17:11:53 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Reference_Counting;
pragma Elaborate_All (NBAda.Lock_Free_Reference_Counting);
--  with Lock_Free_Memory_Reclamation;

with NBAda.Process_Identification;

generic

   type Value_Type is private;
   --  Value type.

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.
package NBAda.Lock_Free_Deques is

   ----------------------------------------------------------------------------
   --  Lock-free double-ended queue (a.k.a deque).
   ----------------------------------------------------------------------------
   type Deque_Type is limited private;

   Deque_Empty : exception;

   procedure Init    (Deque : in out Deque_Type);

   function  Pop_Right  (Deque : access Deque_Type) return Value_Type;
   procedure Push_Right (Deque : in out Deque_Type;
                         Value : in     Value_Type);

   function  Pop_Left  (Deque : access Deque_Type) return Value_Type;
   procedure Push_Left (Deque : in out Deque_Type;
                        Value : in     Value_Type);

   package LFRC is new Lock_Free_Reference_Counting
     (Max_Number_Of_Guards => 128);
--     package LFRC is new Lock_Free_Memory_Reclamation
--       (Max_Number_Of_Dereferences   => 8,
--        --  Remember to account for the dereferences in the
--        --  callbacks Clean_Up and Dispose (which are invoked by Delete).
--        --  Here: PushRight <= ?
--        --        PopRight  <= ?
--        --        PushLeft  <= ?
--        --        PopLeft   <= ?
--        --        Dispose   <= ?
--        --        Clean_up  <= ?
--        --  Delete is called from Pop* on a dereferenced node so the
--        --  maximum number of simultaneous dereferences is ?.
--        Max_Number_Of_Links_Per_Node => 2,
--        Clean_Up_Threshold           => 256,
--        --  Clean up and scan often.
--        Process_Ids                  => Process_Ids);l

   procedure Verify (Deque : in out Deque_Type;
                     Print : in     Boolean := False);
   --  Should only be called when the deque is idle.

private

   type Deque_Node_Reference is new LFRC.Shared_Reference_Base;

   type Deque_Node is new LFRC.Managed_Node_Base with
      record
         Next     : aliased Deque_Node_Reference;
         pragma Atomic (Next);
         --  Next can be marked with a deletion mark.
         Previous : aliased Deque_Node_Reference;
         pragma Atomic (Previous);
         --  Next can be marked with a deletion mark. This must never be
         --  done before Next has been marked.
         Value    : Value_Type;
      end record;
   --  Note:

   procedure Dispose  (Node       : access Deque_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (Node : access Deque_Node);
   procedure Free     (Node : access Deque_Node);
   function  All_References (Node : access Deque_Node)
                            return LFRC.Reference_Set;

   package LFRC_Ops is new LFRC.Operations (Deque_Node,
                                            Deque_Node_Reference);

   procedure Delete (X : LFRC_Ops.Private_Reference) renames LFRC_Ops.Release;

   subtype Deque_Node_Access is LFRC_Ops.Private_Reference;

   type Deque_Type is limited
      record
         Head : aliased Deque_Node_Reference;
         pragma Atomic (Head);
         Tail : aliased Deque_Node_Reference;
         pragma Atomic (Tail);
      end record;

end NBAda.Lock_Free_Deques;
