-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_deques_memory_reclamation_adapter.ads<2>
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Thu Sep  6 12:05:42 2007
--  $Id: nbada-lock_free_deques_memory_reclamation_adapter.ads,v 1.1 2007/09/06 10:12:39 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Memory_Reclamation;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter is

   package Memory_Reclamation is new Lock_Free_Memory_Reclamation
     (Max_Number_Of_Dereferences   => 8,
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
      Clean_Up_Threshold           => 256,
      --  Clean up and scan often.
      Process_Ids                  => Process_Ids);

end NBAda.Lock_Free_Deques_Memory_Reclamation_Adapter;

