-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
--
--  Copyright (C) 2012  Anders Gidenstam
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
--                              -*- Mode: Ada -*-
--  Filename        : nbada-configuration.ads
--  Description     : NBAda - A library of non-blocking algorithms and
--                    data structures.
--  Author          : Anders Gidenstam
--  Created On      : Thu Aug 03 12:31:08 2012
-------------------------------------------------------------------------------

pragma License (GPL);


package NBAda.Configuration is

   pragma Pure (NBAda.Configuration);

   Integrity_Checking : constant Boolean := False;
   --  Enables integrity checking in modules that support it.

   Collect_Statistics : constant Boolean := True;
   --  Enables collection of runtime statistics in modules
   --  that support it.

end NBAda.Configuration;
