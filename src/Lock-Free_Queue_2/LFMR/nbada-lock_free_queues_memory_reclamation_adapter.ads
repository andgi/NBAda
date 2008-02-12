-------------------------------------------------------------------------------
--  Lock-free Queue - An implementation of  M. Hoffman, O. Shalev and
--                    N. Shavit's lock-free queue algorithm.
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_queues_memory_reclamation_adapter.ads
--  Description     : A lock-free queue algorithm based on
--                    M. Hoffman, O. Shalev and N. Shavit,
--                    "The Baskets Queue", The 11th International Conference
--                    On the Principles Of Distributed Systems (OPODIS'07),
--                    LNCS 4878, pp. 401-414, 2007.
--  Author          : Anders Gidenstam
--  Created On      : Thu Jan 10 19:57:44 2008
--  $Id: nbada-lock_free_queues_memory_reclamation_adapter.ads,v 1.2 2008/02/12 18:18:23 andersg Exp $
-------------------------------------------------------------------------------

pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Memory_Reclamation;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter is

   package Memory_Reclamation is new Lock_Free_Memory_Reclamation
     (Max_Number_Of_Dereferences   => 10,
      --  Remember to account for the dereferences in the
      --  callbacks Clean_Up and Dispose (which are invoked by Delete).
      --  Here: Enqueue <= ?
      --        Dequeue <= ?
      --        Dispose   <= ?
      --        Clean_up  <= ?
      --  Delete is called from Dequeue on a dereferenced node so the
      --  maximum number of simultaneous dereferences is ?.
      Max_Number_Of_Links_Per_Node => 1,
      Clean_Up_Threshold           =>
        2 * Natural (Process_Ids.Max_Number_Of_Processes),
--      Scan_Threshold               =>
--        2 * Natural (Process_Ids.Max_Number_Of_Processes * 10) + 1,
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids);

end NBAda.Lock_Free_Queues_Memory_Reclamation_Adapter;
