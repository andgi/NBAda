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
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_dictionaries.adb
--  Description     : Lock-free dictionary based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 18 17:44:46 2007
--  $Id: nbada-lock_free_dictionaries.adb,v 1.1 2007/05/18 16:33:01 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);


package body Lock_Free_Dictionaries is

   ----------------------------------------------------------------------------
   procedure Init    (Dictionary : in out Dictionary_Type) is
   begin
      for I in Dictionary.Hash_Bucket'Range loop
         Sets.Init (Dictionary.Hash_Bucket (I));
      end loop;
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is
      Bucket : constant Bucket_Index :=
        Hash (Key, Into.Hash_Bucket'Last);
   begin
      Sets.Insert (Into  => Into.Hash_Bucket (Bucket),
                   Key   => Key,
                   Value => Value);

   exception
      when Sets.Already_Present =>
         raise Already_Present;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type) is
      Bucket : constant Bucket_Index :=
        Hash (Key, From.Hash_Bucket'Last);
   begin
      Sets.Delete (From  => From.Hash_Bucket (Bucket),
                   Key   => Key);

   exception
      when Sets.Not_Found =>
         raise Not_Found;
   end Delete;

   ----------------------------------------------------------------------------
   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type is
      Bucket : constant Bucket_Index :=
        Hash (Key, From.Hash_Bucket'Last);
   begin
      return Sets.Find (Set  => From.Hash_Bucket (Bucket),
                        Key   => Key);

   exception
      when Sets.Not_Found =>
         raise Not_Found;
   end Lookup;

end Lock_Free_Dictionaries;
