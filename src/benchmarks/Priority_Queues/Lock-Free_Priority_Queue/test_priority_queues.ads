-------------------------------------------------------------------------------
--  Priority Queue Test - Test benchmark for lock-free priority queues.
--
--  Copyright (C) 2008  Anders Gidenstam
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
--  Filename        : test_priority_queues.ads
--  Description     : Harness for the lock-free priority queue.
--  Author          : Anders Gidenstam
--  Created On      : Tue Mar 11 15:57:43 2008
--  $Id: test_priority_queues.ads,v 1.1 2008/03/11 16:00:06 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Priority_Queues;
with NBAda.Process_Identification;

generic

   type Element_Type is private;

   with function "<" (Left, Right : Element_Type) return Boolean is <>;
   --  Note: Element_Type must be totally ordered.

   with function Image (Element : Element_Type) return String is <>;

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package Test_Priority_Queues is

   package Priority_Queues is new
     NBAda.Lock_Free_Priority_Queues (Element_Type => Element_Type,
                                      "<"         => "<",
                                      Process_Ids => Process_Ids);

   ----------------------------------------------------------------------------
   --  Priority Queue.
   ----------------------------------------------------------------------------
   subtype Priority_Queue_Type is Priority_Queues.Priority_Queue_Type;

   Queue_Empty     : exception
     renames Priority_Queues.Queue_Empty;
   Already_Present : exception
     renames Priority_Queues.Already_Present;

   procedure Initialize (Queue : in out Priority_Queue_Type)
     renames Priority_Queues.Initialize;

   procedure Insert  (Into    : in out Priority_Queue_Type;
                      Element : in     Element_Type)
     renames Priority_Queues.Insert;

   procedure Delete_Min (From    : in out Priority_Queue_Type;
                         Element :    out Element_Type)
     renames Priority_Queues.Delete_Min;

   function  Delete_Min (From : in Priority_Queue_Type)
                        return Element_Type
     renames Priority_Queues.Delete_Min;

   function  Delete_Min (From : access Priority_Queue_Type)
                        return Element_Type
     renames Priority_Queues.Delete_Min;

end Test_Priority_Queues;
