-------------------------------------------------------------------------------
--  Lock-Free Dicitionaries - An implementation of the lock-free hash table
--                            algorithm by M. Michael.
--
--  Copyright (C) 2006 - 2007  Anders Gidenstam
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
--  Filename        : my_set.ads
--  Description     : Test of the lock-free set.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 17:48:33 2006
--  $Id: my_dictionary.ads,v 1.1 2007/05/18 16:33:02 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Process_Identification;
with Lock_Free_Dictionaries;

package My_Dictionary is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   subtype Key_Type is Natural;
   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   function Hash_Nat (Ref  : in Natural;
                      Size : in Natural) return Natural;

   package Dictionaries is new
     Lock_Free_Dictionaries (Key_Type    => Key_Type,
                             Value_Type  => Value_Type,
                             Process_Ids => PID,
                             Hash        => Hash_Nat);

end My_Dictionary;
