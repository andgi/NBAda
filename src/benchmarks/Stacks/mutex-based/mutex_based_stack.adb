-------------------------------------------------------------------------------
--  Mutex-based Stack - A mutex-based concurrent stack.
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
--  Filename        : mutex_based_stack.adb
--  Description     : A mutex-based concurrent stack.
--  Author          : Anders Gidenstam
--  Created On      : Fri Sep 23 18:15:38 2005
--  $Id: mutex_based_stack.adb,v 1.2 2007/09/11 12:27:50 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

package body Mutex_Based_Stack is

   use NBAda;

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Stack_Node'Max_Size_In_Storage_Elements);

   type New_Stack_Node_Access is access Stack_Node;
   for New_Stack_Node_Access'Storage_Pool use Node_Pool;

   -------------------------------------------------------------------------
   --  Internal operations.
   -------------------------------------------------------------------------

   function  CAS is new Primitives.Boolean_Compare_And_Swap_32 (Integer);
   procedure Free (Node : Stack_Node_Access);

   ----------------------------------------------------------------------------
   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type) is
      New_Node : constant New_Stack_Node_Access := new Stack_Node;
   begin
      New_Node.Element := Element;

      On.Mutex.Acquire;

      New_Node.Next := On.Head;

      On.Head := Stack_Node_Access (New_Node);

      On.Mutex.Release;
   end Push;

   ----------------------------------------------------------------------------
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type) is
   begin

      From.Mutex.Acquire;

      declare
         Old_Head : constant Stack_Node_Access := From.Head;
      begin
         if Old_Head /= null then

            From.Head := Old_Head.Next;
            Element := Old_Head.Element;

            Free (Old_Head);

            From.Mutex.Release;
         else
            From.Mutex.Release;
            raise Stack_Empty;
         end if;
      end;
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
      From.Mutex.Acquire;

      if From.Head /= null then
         Result := From.Head.Element;

         From.Mutex.Release;

         return Result;
      else
         From.Mutex.Release;
         raise Stack_Empty;
      end if;
   end Top;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   protected body Mutex_Type is

      entry Acquire when not Locked is
      begin
         Locked := True;
      end Acquire;

      procedure Release is
      begin
         Locked := False;
      end Release;

   end Mutex_Type;

   ----------------------------------------------------------------------------
   procedure Free (Node : Stack_Node_Access) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Stack_Node,
                                    New_Stack_Node_Access);
      function To_New_Stack_Node_Access is new
        Ada.Unchecked_Conversion (Stack_Node_Access,
                                  New_Stack_Node_Access);
      X : New_Stack_Node_Access :=
        To_New_Stack_Node_Access (Node);
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Stack_Node type from the New_Stack_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end Mutex_Based_Stack;
