-------------------------------------------------------------------------------
--  Lock-free Stack - A lock-free stack using lock-free memory reclamation.
--  Copyright (C) 2005 - 2007  Anders Gidenstam
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
--  Filename        : lock_free_stack.ads
--  Description     : A lock-free stack using lock-free memory reclamation.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 17:55:38 2005
--  $Id: nbada-lock_free_stack.ads,v 1.5 2007/09/07 11:45:42 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
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

   Stack_Empty : exception;

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

   type Stack_Node;
   type Stack_Node_No_Access is access all Stack_Node;

   type Stack_Node is
     new MR.Managed_Node_Base with
      record
         Element : Element_Type;
         Next    : aliased Stack_Node_No_Access;
         pragma Atomic (Next);
         --  NOTE: Next MUST NEVER be DEREFERENCED!
      end record;

   procedure Free (Node : access Stack_Node);

   package MR_Ops is new MR.Operations (Managed_Node => Stack_Node);

   subtype Stack_Node_Reference is MR_Ops.Shared_Reference;

   type Stack_Type is
     limited record
        Head : aliased Stack_Node_Reference;
        pragma Atomic (Head);
     end record;

end NBAda.Lock_Free_Stack;
