-------------------------------------------------------------------------------
--  Lock-Free Dicitionary Test - Test benchmark for lock-free dictionaries.
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
--                              -*- Mode: Ada -*-
--  Filename        : red_black_trees.ads
--  Description     : Left-leaning red-black trees based on Robert Sedgewick's
--                    presentation at Dagstuhl, 2008-02-18.
--  Author          : Anders Gidenstam
--  Created On      : Tue Feb 26 18:25:37 2008
--  $Id: red_black_trees.ads,v 1.1 2008/02/27 17:27:27 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

generic

   type Value_Type is private;
   type Key_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with function Image (Key : Key_Type) return String is <>;

package Red_Black_Trees is

   type Dictionary_Type is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Dictionary : in out Dictionary_Type);

   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete (From  : in out Dictionary_Type;
                     Key   : in     Key_Type);

   function  Delete (From  : in Dictionary_Type;
                     Key   : in Key_Type)
                    return Value_Type;

   function Delete_Min (From : in Dictionary_Type)
                       return Value_Type;

   function Delete_Max (From : in Dictionary_Type)
                       return Value_Type;

   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type;

   procedure Verify (Tree  : in out Dictionary_Type;
                     Print : in     Boolean := False);

private

   type Color_Type is (Red, Black);

   type Tree_Node;
   type Tree_Node_Access is access Tree_Node;

   type Tree_Node is
      record
         Key         : Key_Type;
         Value       : Value_Type;
         Color       : Color_Type;
         Left, Right : Tree_Node_Access;
      end record;

   type Mutable_View (Self : access Dictionary_Type) is
     limited null record;

   type Dictionary_Type is
      record
         Root    : Tree_Node_Access;
         Mutable : Mutable_View (Dictionary_Type'Access);
      end record;

end Red_Black_Trees;
