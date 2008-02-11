-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
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
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_deques.ads
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:46:02 2006
--  $Id: nbada-lock_free_deques.ads,v 1.9 2008/02/11 16:59:45 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter;
with NBAda.Process_Identification;

generic

   type Element_Type is private;
   --  Element type.

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

   function  Pop_Right  (Deque   : access Deque_Type) return Element_Type;
   procedure Push_Right (Deque   : in out Deque_Type;
                         Element : in     Element_Type);

   function  Pop_Left  (Deque   : access Deque_Type) return Element_Type;
   procedure Push_Left (Deque   : in out Deque_Type;
                        Element : in     Element_Type);

   package MR_Adapter is
      new Lock_Free_Deques_Memory_Reclamation_Adapter (Process_Ids);
   package LFRC renames MR_Adapter.Memory_Reclamation;

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
         Value    : Element_Type;
      end record;

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
