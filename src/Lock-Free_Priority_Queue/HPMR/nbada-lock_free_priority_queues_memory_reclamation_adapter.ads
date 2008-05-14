-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - Based on the the lock-free set algorithm by
--                              M. Michael.
--
--  Copyright (C) 2007 - 2008  Anders Gidenstam
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_sets_memory_reclamation_adapter.ads
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Wed Sep  5 17:01:01 2007
--  $Id: nbada-lock_free_priority_queues_memory_reclamation_adapter.ads,v 1.2 2008/05/14 11:44:34 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Hazard_Pointers;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter is

   package Memory_Reclamation is
      new NBAda.Hazard_Pointers (Process_Ids                => Process_Ids,
                                 Max_Number_Of_Dereferences => 4
                                 --  Integrity_Checking         => True
                                 );

end NBAda.Lock_Free_Priority_Queues_Memory_Reclamation_Adapter;
