-------------------------------------------------------------------------------
--  Lock-free Stack - A lock-free stack using lock-free memory reclamation.
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
--  Filename        : nbada-lock_free_stacks_memory_reclamation_adapter.ads
--  Description     : A lock-free stack using lock-free memory reclamation.
--  Author          : Anders Gidenstam
--  Created On      : Thu Sep  6 17:50:28 2007
--  $Id: nbada-lock_free_stacks_memory_reclamation_adapter.ads,v 1.1 2007/09/07 11:45:42 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Epoch_Based_Memory_Reclamation;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Stacks_Memory_Reclamation_Adapter is

   package Memory_Reclamation is new NBAda.Epoch_Based_Memory_Reclamation
     (Epoch_Update_Threshold     => 100,
      --  Suitable number for epoch-based reclamation.
      Process_Ids                => Process_Ids);

end NBAda.Lock_Free_Stacks_Memory_Reclamation_Adapter;
