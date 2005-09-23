-------------------------------------------------------------------------------
--  Example Stack - A lock-free stack using hazard pointers.
--  Copyright (C) 2005  Anders Gidenstam
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
--  Filename        : example_stack.adb
--  Description     : A lock-free stack using hazard pointers for
--                    memory management and ABA prevention.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 18:15:38 2005
--  $Id: example_stack.adb,v 1.1 2005/09/23 17:28:44 anders Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Lock_Free_Growing_Storage_Pools;

package body Example_Stack is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Stack_Node'Max_Size_In_Storage_Elements);

   type New_Stack_Node_Access is access Stack_Node;
   for New_Stack_Node_Access'Storage_Pool use Node_Pool;

   ----------------------------------------------------------------------------
   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type) is
      use HP_Ops;
      New_Node : New_Stack_Node_Access := new Stack_Node;
      Old_Head : Node_Access;
   begin
      New_Node.Element := Element;
      loop
         Old_Head := Dereference (On.Head'Access);

         New_Node.Next := Stack_Node_No_Access (Old_Head);

         if
           Boolean_Compare_And_Swap (Shared    => On.Head'Access,
                                     Old_Value => Old_Head,
                                     New_Value => Node_Access (New_Node))
         then
            Release (Old_Head);
            return;
         end if;

         Release (Old_Head);
      end loop;
   end Push;

   ----------------------------------------------------------------------------
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type) is
      use HP_Ops;
      Old_Head : Node_Access;
   begin
      loop
         Old_Head := Dereference (From.Head'Access);

         if Old_Head /= null then

            if
              Boolean_Compare_And_Swap (Shared    => From.Head'Access,
                                        Old_Value => Old_Head,
                                        New_Value =>
                                          Node_Access (Old_Head.Next))
            then
               --  NOTE: Old_Head.Next is guaranteed to be up-to-date when the
               --  CAS succeeds because the only way for concurrent operations
               --  to update Old_Head.Next is to pop the node Old_Head and then
               --  push it(/a new node at the same memory address) on the
               --  stack again. The hazard pointer scheme guarantees that this
               --  cannot happen before we Release our reference to Old_Head.

               Element := Old_Head.Element;
               Delete (Old_Head);
               return;
            end if;

            Release (Old_Head);

         else
            Release (Old_Head);

            raise Stack_Empty;
         end if;
      end loop;
   end Pop;

   ----------------------------------------------------------------------------
   function  Pop  (From    : access Stack_Type)
                  return Element_Type is
      Result : Element_Type;
   begin
      Pop (From    => From.all,
           Element => Result);
      return Result;
   end Pop;

   ----------------------------------------------------------------------------
   function  Top  (From    : access Stack_Type)
                  return Element_Type is
      use HP_Ops;
      Head : Node_Access;
      Result : Element_Type;
   begin
      Head := Dereference (From.Head'Access);

      if Head /= null then
         Result := Head.Element;
         Release (Head);
         return Result;
      else
         Release (Head);
         raise Stack_Empty;
      end if;
   end Top;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Free (Node : access Stack_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Stack_Node,
                                    New_Stack_Node_Access);
      function To_New_Stack_Node_Access is new
        Ada.Unchecked_Conversion (HP_Ops.Node_Access,
                                  New_Stack_Node_Access);
      X : New_Stack_Node_Access :=
        To_New_Stack_Node_Access (HP_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Stack_Node type from the New_Stack_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end Example_Stack;
