-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
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
--  Filename        : my_deque.ads
--  Description     : Instantiation of the lock-free deque.
--  Author          : Anders Gidenstam
--  Created On      : Thu Mar  2 16:41:48 2006
--  $Id: my_deque.ads,v 1.3 2007/09/03 17:11:53 andersg Exp $
-------------------------------------------------------------------------------

with NBAda.Process_Identification;
with NBAda.Lock_Free_Deques;

package My_Deque is

   package PID is
      new NBAda.Process_Identification (Max_Number_Of_Processes => 64);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   package Deques is new NBAda.Lock_Free_Deques (Value_Type  => Value_Type,
                                                 Process_Ids => PID);

end My_Deque;
