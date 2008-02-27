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
--  Filename        : test_dictionaries.ads
--  Description     : Test application for the lock-free dictionaries.
--  Author          : Anders Gidenstam
--  Created On      : Tue Feb 26 14:31:56 2008
--  $Id: test_dictionaries.ads,v 1.1 2008/02/27 17:27:27 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Red_Black_Trees;
with NBAda.Process_Identification;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function Hash (Key        : Key_Type;
                       Table_Size : Positive) return Natural;
   --  Note: Not used.
   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.
   with function Image (Key : Key_Type) return String is <>;

   with package Process_Ids is
     new NBAda.Process_Identification (<>);

package Test_Dictionaries is

   ----------------------------------------------------------------------------
   --  Dictionary.
   ----------------------------------------------------------------------------
   type Dictionary_Type is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Dictionary : in out Dictionary_Type);

   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete  (From : in out Dictionary_Type;
                      Key  : in     Key_Type);

   function  Lookup  (From : in Dictionary_Type;
                      Key  : in Key_Type)
                     return Value_Type;

   procedure Verify (Dictionary : in out Dictionary_Type;
                     Print      : in     Boolean := False);

private

   package Trees is new Red_Black_Trees (Key_Type   => Key_Type,
                                         Value_Type => Value_Type);

   protected type Mutex_Type is
      entry     Acquire;
      procedure Release;
   private
      Locked : Boolean := False;
   end Mutex_Type;

   type Mutable_View (Self : access Dictionary_Type) is
     limited null record;

   type Dictionary_Type is
      record
         Dictionary : Trees.Dictionary_Type;
         Mutex      : Mutex_Type;
         Mutable    : Mutable_View (Dictionary_Type'Access);
      end record;

end Test_Dictionaries;
