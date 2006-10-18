-------------------------------------------------------------------------------
--  Lock-free Stack - A lock-free stack using lock-free memory reclamation.
--  Copyright (C) 2005 - 2006  Anders Gidenstam
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_stack.ads
--  Description     : A lock-free stack using lock-free memory reclamation.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 17:55:38 2005
--  $Id: nbada-lock_free_stack.ads,v 1.3 2006/10/18 21:38:44 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Process_Identification;
with Lock_Free_Memory_Reclamation;

generic

   type Element_Type is private;
   --  Element type.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package Lock_Free_Stack is

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

   package MR is new Lock_Free_Memory_Reclamation
     (Max_Number_Of_Dereferences => 1,
      --  Each operation on the stack only accesses one element on the stack.
      Epoch_Update_Threshold     => 100,
      --  Suitable number for epoch-based reclamation.
      Process_Ids         => Process_Ids);

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

end Lock_Free_Stack;
