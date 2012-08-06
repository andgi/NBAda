-------------------------------------------------------------------------------
--  Lock-free bag - An implementation of  the lock-free bag algorithm
--  by H. Sundell, A. Gidenstam, M. Papatriantafilou and P. Tsigas.
--  Copyright (C) 2011 - 2012  Anders Gidenstam
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
--  Description     : A lock-free bag algorithm based on
--                    H. Sundell, A. Gidenstam, M. Papatriantafilou and
--                    P. Tsigas,
--                    "A Lock-Free Algorithm for Concurrent Bags",
--                    Proceedings of the 23rd Annual Symposium on Parallelism
--                    in Algorithms and Architectures (SPAA 2011),
--                    pages 335 - 344, ACM, 2011. 
--  Author          : Anders Gidenstam
--  Created On      : Thu Sep 21 11:30:00 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Bags_Memory_Reclamation_Adapter is

   package Memory_Reclamation is
      new NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation
     (Epoch_Update_Threshold     => 100,
      --  Suitable number for epoch-based reclamation.
      Process_Ids                => Process_Ids);

end NBAda.Lock_Free_Bags_Memory_Reclamation_Adapter;
