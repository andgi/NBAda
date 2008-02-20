-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - Based on the the lock-free set algorithm by
--                              M. Michael.
--
--  Copyright (C) 2007 - 2008  Anders Gidenstam
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
--  Filename        : my_priority_queue.adb
--  Description     : Test program for lock-free priority queues.
--  Author          : Anders Gidenstam
--  Created On      : Mon Jun 11 19:23:38 2007
--  $Id: my_priority_queue.adb,v 1.1 2008/02/20 20:55:38 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

package body My_Priority_Queue is

   ----------------------------------------------------------------------------
   function "<" (Left, Right : Value_Type) return Boolean is
      use type PID.Process_ID_Type;
   begin
      return Left.Index < Right.Index or
        else (Left.Index = Right.Index and Left.Creator < Right.Creator);
   end "<";

   ----------------------------------------------------------------------------
   function Image (X : Value_Type) return String is
   begin
      return
        "(" &
        Integer'Image (X.Index) & ", " &
        PID.Process_ID_Type'Image (X.Creator) &
        ")";
   end Image;

end My_Priority_Queue;
