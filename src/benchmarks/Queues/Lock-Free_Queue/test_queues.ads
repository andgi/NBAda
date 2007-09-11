-------------------------------------------------------------------------------
--  Lock-free Queue Test - Test benchmark for lock-free queues.
--
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--  Filename        : queue_test.adb
--  Description     : Benchmark application for lock-free queues.
--  Author          : Anders Gidenstam
--  Created On      : Thu Nov 30 19:49:40 2006
--  $Id: test_queues.ads,v 1.2 2007/09/11 14:47:37 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Queues;
with NBAda.Process_Identification;

generic
   type Element_Type is private;
   --  Element type.
   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.
package Test_Queues is

   package Queues is new
     NBAda.Lock_Free_Queues (Element_Type, Process_Ids);

   ----------------------------------------------------------------------------
   --  Queue.
   ----------------------------------------------------------------------------
   subtype Queue_Type is Queues.Queue_Type;

   Queue_Empty : exception renames Queues.Queue_Empty;

   procedure Init    (Queue : in out Queue_Type)
     renames Queues.Init;
   function  Dequeue (From : access Queue_Type) return Element_Type
     renames Queues.Dequeue;
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type)
     renames Queues.Enqueue;

   --  procedure Print_Statistics renames Queues.MR.Print_Statistics;

end Test_Queues;
