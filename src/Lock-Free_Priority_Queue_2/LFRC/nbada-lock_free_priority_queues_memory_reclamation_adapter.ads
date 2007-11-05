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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_priority_queues_memory_reclamation_adapter.ads
--  Description     : Lock-free priority queue based on Håkan Sundell,
--                    "
--                    ",
--  Author          : Anders Gidenstam
--  Created On      : Mon Nov  5 19:56:23 2007
--  $Id: nbada-lock_free_priority_queues_memory_reclamation_adapter.ads,v 1.1 2007/11/05 19:14:09 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Lock_Free_Reference_Counting;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

   Max_Levels : Positive;

package NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter is

   package Memory_Reclamation is new Lock_Free_Reference_Counting
     (Max_Number_Of_Guards => 128);

end NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter;
