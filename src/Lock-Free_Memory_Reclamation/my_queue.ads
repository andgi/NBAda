-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
--  Copyright (C) 2004 - 2006  Anders Gidenstam
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
-- Filename        : my_queue.ads
-- Description     : Example application for lock-free reference counting.
-- Author          : Anders Gidenstam
-- Created On      : Thu Mar  2 18:49:40 2006
-- $Id: my_queue.ads,v 1.1 2006/03/02 17:52:00 anders Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Process_Identification;
with Example_Queue;

package My_Queue is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;
   package Queues is new Example_Queue (Value_Type  => Value_Type,
                                        Process_Ids => PID);

end My_Queue;
