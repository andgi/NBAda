-------------------------------------------------------------------------------
--  Lock-Free Simple Trees - An implementation of a simple lock-free binary
--                           tree algorithm by A. Gidenstam.
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_simple_trees_memory_reclamation_adapter.ads
--  Description     : An implementation of a simple lock-free binary
--                    tree algorithm by A. Gidenstam.
--  Author          : Anders Gidenstam
--  Created On      : Thu Sep  6 11:48:14 2007
--  $Id: nbada-lock_free_simple_trees_memory_reclamation_adapter.ads,v 1.2 2008/05/08 08:48:44 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Lock_Free_Reference_Counting;
--  with NBAda.Epoch_Based_Memory_Reclamation;
with NBAda.Hazard_Pointers;
with NBAda.Process_Identification;

generic

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Simple_Trees_Memory_Reclamation_Adapter is

   package Node_Memory_Reclamation is new NBAda.Lock_Free_Reference_Counting
     (Max_Number_Of_Guards => 128);

   package State_Memory_Reclamation is
--      new NBAda.Epoch_Based_Memory_Reclamation (Process_Ids);
      new NBAda.Hazard_Pointers (Max_Number_Of_Dereferences => 5,
                                 Process_Ids                => Process_Ids);

end NBAda.Lock_Free_Simple_Trees_Memory_Reclamation_Adapter;
