-------------------------------------------------------------------------------
--  Lock-free Queue Test - Test benchmark for lock-free queues.
--
--  Copyright (C) 2007  Anders Gidenstam
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
--  Filename        : test_queues.adb
--  Description     : Benchmark application for lock-free queues.
--  Author          : Anders Gidenstam
--  Created On      : Wed Sep 12 16:35:41 2007
--  $Id: test_queues.adb,v 1.1 2007/09/12 15:35:18 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with NBAda.Lock_Free_Growing_Storage_Pools;

package body Test_Queues is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : NBAda.Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Element_Type'Max_Size_In_Storage_Elements);

   type New_Element_Access is access Element_Type;
   for New_Element_Access'Storage_Pool use Node_Pool;

   function To_New_Element_Access is
      new Ada.Unchecked_Conversion (Element_Access,
                                    New_Element_Access);

   ----------------------------------------------------------------------------
   procedure Init    (Queue : in out Queue_Type) is
   begin
      if Queue.Impl = null then
         Queue.Impl := new Queues.Lock_Free_Queue
           (Queues.Queue_Size (10_000 *
                               Natural (Process_Ids.Process_ID_Type'Last)));
      end if;
   end Init;

   ----------------------------------------------------------------------------
   function  Dequeue (From : access Queue_Type) return Element_Type is
      procedure Free is new Ada.Unchecked_Deallocation (Element_Type,
                                                        New_Element_Access);
   begin
      declare
         Item    : New_Element_Access    :=
           To_New_Element_Access (Queues.Dequeue (From.Impl));
         Element : constant Element_Type := Item.all;
      begin
         Free (Item);
         return Element;
      end;
   exception
      when Queues.Queue_Empty =>
         raise Queue_Empty;
   end Dequeue;

   ----------------------------------------------------------------------------
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type) is
      Item : constant New_Element_Access := new Element_Type'(Element);
   begin
      Queues.Enqueue (On.Impl.all,
                      Element_Access (Item));
   exception
      when Queues.Queue_Full =>
         raise Queue_Full;
   end Enqueue;

end Test_Queues;
