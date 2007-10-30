-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
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
--                              -*- Mode: Ada -*-
--  Filename        : memory_reclamation_debug.ads
--  Description     : Track local references.
--  Author          : Anders Gidenstam
--  Created On      : Tue Oct 30 10:37:19 2007
--  $Id: memory_reclamation_debug.ads,v 1.1 2007/10/30 14:59:26 andersg Exp $
-------------------------------------------------------------------------------

with NBAda.Process_Identification;

generic

   type Private_Reference is private;
   --  The private reference type to be tracked.
   Max_Number_Of_Dereferences : Natural;
   --  Maximum number of simultaneously dereferenced links per thread.

with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

   --  Needed reference operations.
   with function Same_Node (Left, Right : Private_Reference)
                           return Boolean;
   with function Image (Reference : Private_Reference)
                       return String;

package Memory_Reclamation_Debug is

   Reference_Error : exception;

   procedure Add_Reference (Node  : Private_Reference;
                            Where : String := "");
   procedure Remove_Reference (Node  : Private_Reference;
                               Where : String := "");
   procedure Dump_Local_References (Where : String := "");

   procedure Verify_Quiescent (Where : String := "");
   --  Verifies that there are no unreleased private references.

   procedure Enter;
   type Reference_Array is array (Positive range <>) of Private_Reference;
   procedure Exit_Quiescent (Where  : String := "";
                             Except : Reference_Array);
   --  Verifies that there are no unreleased private references
   --  except the exceptions.

   ----------------------------------------------------------------------------
   package Debug_IO is

      procedure Put_Line (Text : in String);

   end Debug_IO;

end Memory_Reclamation_Debug;
