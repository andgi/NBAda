-------------------------------------------------------------------------------
--  Lock-Free Dictionaries - An implementation of the lock-free hash table
--                           algorithm by M. Michael.
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_dictionaries.ads
--  Description     : Lock-free dictionary based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 18 17:01:34 2007
--  $Id: nbada-lock_free_dictionaries.ads,v 1.2 2007/09/03 09:56:27 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Process_Identification;

with NBAda.Lock_Free_Sets;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function Hash (Key        : Key_Type;
                       Table_Size : Positive) return Natural;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Dictionaries is

   type Dictionary_Type (No_Buckets : Natural) is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Dictionary : in out Dictionary_Type);

   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type);

   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type;

private

   package Sets is new Lock_Free_Sets (Value_Type  => Value_Type,
                                       Key_Type    => Key_Type,
                                       "<"         => "<",
                                       Process_Ids => Process_Ids);

   subtype Bucket_Index is Natural;

   type Set_Array is array (Bucket_Index range <>) of Sets.Set_Type;

   type Dictionary_Type (No_Buckets : Natural) is
      record
         Hash_Bucket : Set_Array (0 .. No_Buckets);
      end record;

end NBAda.Lock_Free_Dictionaries;
