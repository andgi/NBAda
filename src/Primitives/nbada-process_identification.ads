-------------------------------------------------------------------------------
--  Primitives - A binding to the synchronization primitives of the hardware.
--  Copyright (C) 2004 - 2007  Anders Gidenstam
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
--  Filename        : process_identification.ads
--  Description     : Process IDs.
--  Author          : Anders Gidenstam
--  Created On      : Fri Nov 19 15:56:51 2004
--  $Id: nbada-process_identification.ads,v 1.6 2007/08/30 14:19:56 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

generic
   Max_Number_Of_Processes : Natural;
package NBAda.Process_Identification is

   pragma Elaborate_Body;

   type Process_ID_Type is new Natural range 1 .. Max_Number_Of_Processes;

   --  Register a process ID for this task.
   procedure Register;

   --  Returns the process ID of the calling task.
   function Process_ID return Process_ID_Type;

end NBAda.Process_Identification;
