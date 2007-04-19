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
--  $Id: lock_free_stack.ads,v 1.2 2007/04/19 12:18:16 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Pass_The_Buck;
with Primitives;

generic

   type Element_Type is private;
   --  Element type.

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

   type Stack_Node;
   type Stack_Node_Access is access all Stack_Node;
   for Stack_Node_Access'Size use Primitives.Standard_Unsigned'Size;
   pragma Atomic (Stack_Node_Access);

   type Stack_Node is
      record
         Element : Element_Type;
         Next    : aliased Stack_Node_Access;
         pragma Atomic (Next);
         --  NOTE: Next MUST NEVER be DEREFERENCED!
      end record;

   procedure Free (Node : access Stack_Node);

   package MR is new Pass_The_Buck
     (Max_Number_Of_Guards => 128,
      Value_Type           => Stack_Node_Access,
      Null_Value           => null);

   subtype Stack_Node_Reference is Stack_Node_Access;
   subtype Node_Access is Stack_Node_Access;

   type Stack_Type is
     limited record
        Head : aliased Stack_Node_Reference;
        pragma Atomic (Head);
     end record;

end Lock_Free_Stack;
