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
--  Filename        : test_queues.ads
--  Description     : Benchmark application for lock-free queues.
--  Author          : Anders Gidenstam
--  Created On      : Wed Sep 12 16:25:49 2007
--  $Id: test_queues.ads,v 1.1 2007/09/12 15:35:18 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Lock_Free_Bounded_Queues;
with NBAda.Primitives;
with Ada.Unchecked_Conversion;

generic
   type Element_Type is private;
   --  Element type.
   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.
package Test_Queues is

   ----------------------------------------------------------------------------
   --  Queue.
   ----------------------------------------------------------------------------
   type Queue_Type is limited private;

   Queue_Empty : exception;
   Queue_Full  : exception;

   procedure Init    (Queue : in out Queue_Type);
   function  Dequeue (From : access Queue_Type) return Element_Type;
   procedure Enqueue (On      : in out Queue_Type;
                      Element : in     Element_Type);

   --  procedure Print_Statistics renames Queues.MR.Print_Statistics;

private

   type Element_Access is access all Element_Type;

   function To_Element_Access is new
     Ada.Unchecked_Conversion (NBAda.Primitives.Standard_Unsigned,
                               Element_Access);
   use type NBAda.Primitives.Standard_Unsigned;
   Null_0 : constant Element_Access := null;
   Null_1 : constant Element_Access := To_Element_Access (0-1);

   package Queues is new
     NBAda.Lock_Free_Bounded_Queues (Element_Type => Element_Access,
                                     Null_0       => Null_0,
                                     Null_1       => Null_1);

   type Queue_Access is access Queues.Lock_Free_Queue;

   type Queue_Type is limited
      record
         Impl : Queue_Access;
      end record;

end Test_Queues;
