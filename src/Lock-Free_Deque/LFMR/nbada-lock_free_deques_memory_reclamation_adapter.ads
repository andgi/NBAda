-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
--
--  Copyright (C) 2007 - 2011  Anders Gidenstam
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
--  Filename        : nbada-lock_free_deques_memory_reclamation_adapter.ads
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Thu Sep  6 12:05:42 2007
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Memory_Reclamation.Beware_And_Cleanup;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter is

   package Memory_Reclamation is
      new NBAda.Memory_Reclamation.Beware_And_Cleanup
     (Max_Number_Of_Dereferences   => 10,
      --  Remember to account for the dereferences in the
      --  callbacks Clean_Up and Dispose (which are invoked by Delete).
      --  Here: PushRight <= ?
      --        PopRight  <= ?
      --        PushLeft  <= ?
      --        PopLeft   <= ?
      --        Dispose   <= ?
      --        Clean_up  <= ?
      --  Delete is called from Pop* on a dereferenced node so the
      --  maximum number of simultaneous dereferences is ?.
      Max_Number_Of_Links_Per_Node => 2,
      Clean_Up_Threshold           =>
        2 * Natural (Process_Ids.Max_Number_Of_Processes),
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids,
      Integrity_Checking           => False);

end NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter;

