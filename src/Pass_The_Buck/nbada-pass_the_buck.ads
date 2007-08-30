-------------------------------------------------------------------------------
--  Pass-the-buck - An implementation of Herlihy et. al.'s algorithm.
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
--  Filename        : pass_the_buck.ads
--  Description     : Lock-Free Ada implementation of Herlihy et. al.'s
--                    Pass-The-Buck Algorithm.
--                    Based on M. Herlihy, V. Luchangco, P. Martin and M. Moir,
--                    "Nonblocking Memory Management Support for
--                     Dynamic-Sized Data Structures", ACM Transcations on
--                    Computer Systems, 23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
--  Created On      : Thu Nov 23 17:13:55 2006
--  $Id: nbada-pass_the_buck.ads,v 1.5 2007/08/30 16:59:30 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

generic

   Max_Number_Of_Guards : Natural;
   --  The maximum number of simultaneously active guards.
   --  (Rouhly: The maximum number of simultaneously dereferenced nodes.)

   type Value_Type is private;
   --  Value_Type must be atomic.
   --  Value_Type'Object_Size MUST equal System.Word_Size.

   Null_Value : Value_Type;

package NBAda.Pass_The_Buck is

   pragma Elaborate_Body;

   type Guard_Type is private;
   type Value_Set is array (Natural range <>) of Value_Type;

   function  Hire_Guard return Guard_Type;

   procedure Fire_Guard (Guard : in Guard_Type);

   procedure Post_Guard (Guard : in Guard_Type;
                         Value : in Value_Type);

   function Liberate (Values : in Value_Set) return Value_Set;

private

   type Guard_Type is new Natural range 0 .. Max_Number_Of_Guards;
   for Guard_Type'Size use 32;


   type Atomic_Guard_Type is new Guard_Type;
   pragma Atomic (Atomic_Guard_Type);

end NBAda.Pass_The_Buck;
