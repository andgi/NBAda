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
--  Filename        : lock_free_stack.adb
--  Description     : A lock-free stack using hazard pointers for
--                    memory management and ABA prevention.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 18:15:38 2005
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;

package body NBAda.Lock_Free_Stack is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Stack_Node'Max_Size_In_Storage_Elements);

   type New_Stack_Node_Access is access Stack_Node;
   for New_Stack_Node_Access'Storage_Pool use Node_Pool;

   function Create_Node is new MR_Ops.Create
     (User_Node_Access => New_Stack_Node_Access);

   ----------------------------------------------------------------------------
   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type) is
      use MR_Ops;
      New_Node : Private_Reference := Create_Node (On.MM);
   begin
      "+" (New_Node).Element := Element;
      loop
         declare
            Old_Head : constant Private_Reference :=
              Dereference (On.MM,
                           On.Head'Access);
         begin
            Store ("+" (New_Node).Next'Access, Old_Head);
            if
              Compare_And_Swap (Link      => On.Head'Access,
                                Old_Value => Old_Head,
                                New_Value => New_Node)
            then
               Release (Old_Head);
               Release (New_Node);
               return;
            end if;

            Release (Old_Head);
         end;
      end loop;
   end Push;

   ----------------------------------------------------------------------------
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type) is
      use MR_Ops;
      use MR_Ops.Basic_Reference_Operations;
   begin
      loop
         declare
            Old_Head : constant Private_Reference :=
              Dereference (From.MM, From.Head'Access);
         begin
            if Old_Head /= Null_Reference then
               if
                 Compare_And_Swap
                 (Link      => From.Head'Access,
                  Old_Value => Old_Head,
                  New_Value => Unsafe_Read ("+" (Old_Head).Next'Access))
               then
                  --  NOTE: Old_Head.Next is guaranteed to be up-to-date when
                  --  the CAS succeeds because the only way for concurrent
                  --  operations to update Old_Head.Next is to pop the node
                  --  Old_Head and then push it(/a new node at the same memory
                  --  address) on the stack again. The memroy reclamation
                  --  scheme guarantees that this cannot happen before we
                  --  Release our reference to Old_Head.

                  Element := "+" (Old_Head).Element;
                  Delete (Old_Head);
                  return;
               else
                  Release (Old_Head);
               end if;
            else
               Release (Old_Head);
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
      use MR_Ops;

      Result : Element_Type;
   begin
      declare
         Head : constant Private_Reference :=
           Dereference (From.MM,
                        From.Head'Access);
      begin
         if Head /= Null_Reference then
            Result := "+" (Head).Element;
            Release (Head);
            return Result;
         else
            Release (Head);
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
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_Stack_Node_Access);
      X : New_Stack_Node_Access :=
        To_New_Stack_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Stack_Node type from the New_Stack_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Lock_Free_Stack;
