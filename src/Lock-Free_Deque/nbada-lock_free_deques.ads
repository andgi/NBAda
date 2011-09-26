-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
--
--  Copyright (C) 2006 - 2011  Anders Gidenstam
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
--  Filename        : nbada-lock_free_deques.ads
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:46:02 2006
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter;
with NBAda.Process_Identification;
with NBAda.Interfaces.Exceptions;

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

   Deque_Empty : exception
     renames NBAda.Interfaces.Exceptions.Empty;

   procedure Init    (Deque : in out Deque_Type);

   function  Pop_Right  (Deque   : access Deque_Type) return Element_Type;
   procedure Push_Right (Deque   : in out Deque_Type;
                         Element : in     Element_Type);

   function  Pop_Left  (Deque   : access Deque_Type) return Element_Type;
   procedure Push_Left (Deque   : in out Deque_Type;
                        Element : in     Element_Type);

   procedure Verify (Deque : in out Deque_Type;
                     Print : in     Boolean := False);
   --  Should only be called when the deque is idle.

private

   package MR_Adapter is
      new Lock_Free_Deques_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   type Deque_Node_Reference is
     new NBAda.Memory_Reclamation.Shared_Reference_Base;

   type Deque_Node is new MR.Managed_Node_Base with
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

   procedure Free     (Node : access Deque_Node);
   procedure Dispose  (Node       : access Deque_Node;
                       Concurrent : in     Boolean);
   procedure Clean_Up (MM   : in     MR.Memory_Manager_Base'Class;
                       Node : access Deque_Node);
   function  All_References (Node : access Deque_Node)
                            return MR.Reference_Set;

   package MR_Ops is new MR.Reference_Operations
     (Managed_Node     => Deque_Node,
      Shared_Reference => Deque_Node_Reference);

   subtype Private_Reference is MR_Ops.Private_Reference;

   type Deque_Type is
      limited record
         MM   : MR_Ops.Memory_Manager;
         Head : aliased Deque_Node_Reference;
         pragma Atomic (Head);
         Tail : aliased Deque_Node_Reference;
         pragma Atomic (Tail);
      end record;

end NBAda.Lock_Free_Deques;
