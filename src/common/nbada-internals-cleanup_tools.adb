-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
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

pragma License (GPL);

package body NBAda.Internals.Cleanup_Tools is

   procedure Finalize (Object : in out On_Exit) is
   begin
      if Object.Final_Action /= null then
         Object.Final_Action.all;
      end if;
   end Finalize;

end NBAda.Internals.Cleanup_Tools;
