-------------------------------------------------------------------------------
--  Lock-free Flat-sets - An implementation of A. Gidenstam et al.'s
--                        atomic move algorithm.
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
--                              -*- Mode: Ada -*-
--  Filename        : nbada-atomic_move.ads
--  Description     : Based on A. Gidenstam,
--                    M. Papatriantafilou and P. Tsigas,
--                    "Allocating memory in a lock-free manner",
--                    The 13th Annual European Symposium on Algorithms
--                    (ESA 2005), LNCS 3669, pages 329 - 242, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Jan 16 11:12:21 2008
--  $Id: nbada-atomic_move.ads,v 1.2 2008/01/22 15:09:30 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Large_Primitives;
with NBAda.Primitives;

generic

   type Element_Type is private;
   --  Element type.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Atomic_Move is

   type Shared_Location is limited private;

   type Private_Reference is private;


   function Dereference (Location : access Shared_Location)
                        return Private_Reference;

   type Move_Status is (Moved_Ok, Not_Moved, Moved_Away, Helped);
   procedure Move (To      : access Shared_Location;
                   Element : in out Private_Reference;
                   Result  :    out Move_Status);
   --  Move atomically moves Element to the new shared location thereby
   --  removing it from it's previous shared location.

   function Create (Element : Element_Type) return Private_Reference;
   procedure Delete (Element : in Private_Reference);

   Null_Reference : constant Private_Reference;

   function "=" (Left, Right : Private_Reference) return Boolean;
   function Image (Ref : Private_Reference) return String;
   function Image (Location : Shared_Location) return String;

   type Element_Access is access all Element_Type;
   function "+" (Ref : Private_Reference) return Element_Access;
   --  NOTE: Do not store the returned Element_Access.

private

   No_Of_Version_Bits : constant := 6;

   type Version_ID is mod 2 ** No_Of_Version_Bits;
   type Node_Access_Impl is
     mod 2 ** (Primitives.Standard_Unsigned'Size - No_Of_Version_Bits);

   type Node;
   type Node_Access is access all Node;

   type Node_Ref is
      record
         Node    : Node_Access_Impl := 0;
         Version : Version_ID := 0;
      end record;
   for Node_Ref use record
      Node    at 0 range
        Version_ID'Size .. Primitives.Standard_Unsigned'Size - 1;
      Version at 0 range 0 .. Version_ID'Size - 1;
   end record;
   for Node_Ref'Size use Primitives.Standard_Unsigned'Size;

   type Shared_Location is new Node_Ref;
   for Shared_Location'Size use Primitives.Standard_Unsigned'Size;
   pragma Atomic (Shared_Location);

   type Shared_Location_Access is access all Shared_Location;

   type Move_Info is
      record
         Current : Version_ID := 0;
         New_Pos : Shared_Location_Access;
         Old_Pos : Shared_Location_Access;
      end record;

   package Software_LL_SC is
      new Large_Primitives (Max_Number_Of_Links => 2,
                            Process_Ids         => Process_Ids);
   package Move_Info_LL_SC is
      new Software_LL_SC.Load_Linked_Store_Conditional (Move_Info);

   type Node is limited
      record
         Status  : aliased Move_Info_LL_SC.Shared_Element;
         Element : aliased Element_Type;
      end record;
   for Node'Alignment use 2 ** No_Of_Version_Bits;

   type Private_Reference is
      record
         Ref      : Node_Ref;
         Location : Shared_Location_Access;
      end record;

   Null_Reference : constant Private_Reference := ((0, 0), null);

end NBAda.Atomic_Move;
