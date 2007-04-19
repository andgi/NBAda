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
--  Filename        : lock_free_stack.adb
--  Description     : A lock-free stack using hazard pointers for
--                    memory management and ABA prevention.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 18:15:38 2005
--  $Id: lock_free_stack.adb,v 1.2 2007/04/19 12:18:16 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with Lock_Free_Growing_Storage_Pools;

package body Lock_Free_Stack is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Stack_Node'Max_Size_In_Storage_Elements);

   type New_Stack_Node_Access is access Stack_Node;
   for New_Stack_Node_Access'Storage_Pool use Node_Pool;

   function Boolean_Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap (Node_Access);

   ----------------------------------------------------------------------------
   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type) is
      New_Node : constant New_Stack_Node_Access := new Stack_Node;
      use MR;
      Guard    : constant Guard_Type := Hire_Guard;
   begin
      New_Node.Element := Element;
      loop
         declare
            Old_Head : Node_Access;
         begin
            loop
               Old_Head := On.Head;
               Post_Guard (Guard, Old_Head);

               exit when Old_Head = On.Head;
            end loop;

            New_Node.Next := Stack_Node_Access (Old_Head);

            if
              Boolean_Compare_And_Swap (Target    => On.Head'Access,
                                        Old_Value => Old_Head,
                                        New_Value => Node_Access (New_Node))
            then
               Post_Guard (Guard, null);
               Fire_Guard (Guard);
               return;
            end if;

         end;
      end loop;
   end Push;

   ----------------------------------------------------------------------------
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type) is
      use MR;
      Guard    : constant Guard_Type := Hire_Guard;
   begin
      loop
         declare
            Old_Head : Node_Access;
         begin
            loop
               Old_Head := From.Head;
               Post_Guard (Guard, Old_Head);

               exit when Old_Head = From.Head;
            end loop;

            if Old_Head /= null then

               if
                 Boolean_Compare_And_Swap (Target    => From.Head'Access,
                                           Old_Value => Old_Head,
                                           New_Value =>
                                             Node_Access (Old_Head.Next))
               then
                  --  NOTE: Old_Head.Next is guaranteed to be
                  --  up-to-date when the CAS succeeds because the
                  --  only way for concurrent operations to update
                  --  Old_Head.Next is to pop the node Old_Head and
                  --  then push it(/a new node at the same memory
                  --  address) on the stack again. The memory
                  --  reclamation scheme guarantees that this cannot
                  --  happen before we Release our reference to
                  --  Old_Head.


                  Element := Old_Head.Element;
                  Post_Guard (Guard, null);
                  Fire_Guard (Guard);
                  declare
                     Old : constant Value_Set (1 .. 1)
                       := (Old_Head, others => null);
                     VS  : constant Value_Set
                       := Liberate (Old);
                  begin
                     for I in VS'Range loop
                        Free (VS (I));
                     end loop;
                  end;
                  return;
               end if;

            else
               Post_Guard (Guard, null);
               Fire_Guard (Guard);
               raise Stack_Empty;
            end if;
         end;
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
      Result : Element_Type;
   begin
      declare
         use MR;
         Head  : Node_Access;
         Guard : constant Guard_Type := Hire_Guard;
      begin
         loop
            Head := From.Head;
            Post_Guard (Guard, Head);

            exit when Head = From.Head;
         end loop;

         if Head /= null then
            Result := Head.Element;
            Post_Guard (Guard, null);
            Fire_Guard (Guard);
            return Result;
         else
            Post_Guard (Guard, null);
            Fire_Guard (Guard);
            raise Stack_Empty;
         end if;
      end;
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
        Ada.Unchecked_Conversion (Node_Access,
                                  New_Stack_Node_Access);
      X : New_Stack_Node_Access :=
        To_New_Stack_Node_Access (Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Stack_Node type from the New_Stack_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end Lock_Free_Stack;
