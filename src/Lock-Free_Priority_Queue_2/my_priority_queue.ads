-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - An implementation of the lock-free skip-list
--                              algorithm by H. Sundell.
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
--  Filename        : my_priority_queue.ads
--  Description     : Test program for lock-free priority queues.
--  Author          : Anders Gidenstam
--  Created On      : Mon Jun 11 15:15:56 2007
--  $Id
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Priority_Queues;
with NBAda.Process_Identification;

package My_Priority_Queue is

   package PID is
      new NBAda.Process_Identification (Max_Number_Of_Processes => 64);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   function "<" (Left, Right : Value_Type) return Boolean;
   function Image (X : Value_Type) return String;

   package Priority_Queues is new
     NBAda.Lock_Free_Priority_Queues
     (Element_Type => Value_Type,
      "<"          => "<",
      Process_Ids  => PID);

end My_Priority_Queue;
