-------------------------------------------------------------------------------
--  Lock-free Stack - A lock-free stack using lock-free memory reclamation.
--  Copyright (C) 2005 - 2011  Anders Gidenstam
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
--  Filename        : lock_free_stack.ads
--  Description     : A lock-free stack using lock-free memory reclamation.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 17:55:38 2005
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Interfaces.Exceptions;
with NBAda.Lock_Free_Stacks_Memory_Reclamation_Adapter;

generic

   type Element_Type is private;
   --  Element type.

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Stack is

   ----------------------------------------------------------------------------
   --  Lock-free Stack.
   ----------------------------------------------------------------------------
   type Stack_Type is limited private;

   Stack_Empty : exception
     renames NBAda.Interfaces.Exceptions.Empty;

   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type);
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type);
   function  Pop  (From    : access Stack_Type)
                  return Element_Type;

   function  Top  (From    : access Stack_Type)
                  return Element_Type;

private

   package MR_Adapter is
      new Lock_Free_Stacks_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   type Stack_Node_Reference is new Memory_Reclamation.Shared_Reference_Base;

   type Stack_Node is
     new MR.Managed_Node_Base with
      record
         Element : Element_Type;
         Next    : aliased Stack_Node_Reference;
         pragma Atomic (Next);
         --  NOTE: Next MUST NEVER be DEREFERENCED!
      end record;

   procedure Free (Node : access Stack_Node);

   package MR_Ops is new MR.Reference_Operations
     (Managed_Node     => Stack_Node,
      Shared_Reference => Stack_Node_Reference);

   type Stack_Type is
      limited record
         MM   : MR_Ops.Memory_Manager;
         Head : aliased Stack_Node_Reference;
         pragma Atomic (Head);
      end record;

end NBAda.Lock_Free_Stack;
