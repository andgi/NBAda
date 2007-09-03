-------------------------------------------------------------------------------
--  Lock-Free Dicitionaries - An implementation of the lock-free hash table
--                            algorithm by M. Michael.
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
--  Filename        : my_dictionary.ads
--  Description     : Test of the lock-free set.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 18 14:48:33 2006
--  $Id: my_dictionary.adb,v 1.2 2007/09/03 09:56:27 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

package body My_Dictionary is

   ----------------------------------------------------------------------------
   function Hash_Nat (Ref  : in Natural;
                      Size : in Natural) return Natural is
   begin
      return Ref mod Size;
   end Hash_Nat;

end My_Dictionary;
