-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of  the cache-aware lock-free queue
--  algorithm by A. Gidenstam, H. Sundell and P. Tsigas.
--  Copyright (C) 2011  Anders Gidenstam
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
--  Description     : A lock-free queue algorithm based on
--                    A. Gidenstam, H. Sundell and P. Tsigas,
--                    "Cache-Aware Lock-Free Queues for Multiple
--                     Producers/Consumers and Weak Memory Consistency",
--                    The 14th International Conference
--                    On the Principles Of Distributed Systems (OPODIS'10),
--                    LNCS 6490, pp. 302-317, 2011.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jun 07 19:49:28 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Reference_Counting;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter is

   package Memory_Reclamation is new NBAda.Lock_Free_Reference_Counting
     (Max_Number_Of_Guards => 128,
      Integrity_Checking   => True);

end NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter;
