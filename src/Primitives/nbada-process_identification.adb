-------------------------------------------------------------------------------
-- Primitives - A binding to the synchronization primitives of the hardware.
-- Copyright (C) 2004  Anders Gidenstam
--
-- This program is free software; you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation; either version 2 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
--
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : process_identification.adb
-- Description     : Process IDs.
-- Author          : Anders Gidenstam
-- Created On      : Fri Nov 19 16:06:16 2004
-- $Id: nbada-process_identification.adb,v 1.3 2004/11/25 15:50:14 anders Exp $
-------------------------------------------------------------------------------

with Ada.Task_Attributes;
with Primitives;

package body Process_Identification is

   subtype Process_ID_Base is Primitives.Unsigned_32;

   package Process_IDs is
      new Ada.Task_Attributes (Attribute     => Process_ID_Base,
                               Initial_Value => 0);
   --  In GNAT some of the operations in Ada.Task_Attributes are
   --  blocking,  in particular, Set_Value. Value is non-blocking for
   --  small values, though.

   Process_Count : aliased Process_ID_Base := 0;
   pragma Atomic (Process_Count);
   --  Shared process id counter.

   ----------------------------------------------------------------------------
   procedure Register is
      use type Process_ID_Base;
   begin
      if Process_IDs.Value = 0 then
         Process_IDs.Set_Value
           (Primitives.Fetch_And_Add (Target    => Process_Count'Access,
                                      Increment => 1) + 1);
      end if;
   end Register;

   ----------------------------------------------------------------------------
   function Process_ID return Process_ID_Type is
   begin
      return Process_ID_Type (Process_IDs.Value);
   end Process_ID;

end Process_Identification;