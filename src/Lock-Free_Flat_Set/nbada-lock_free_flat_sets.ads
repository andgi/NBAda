-------------------------------------------------------------------------------
--  Lock-free Flat-sets - An implementation of A. Gidenstam et al.'s
--                        lock-free flat-set algorithm.
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
--  Filename        : nbada-lock_free_flat_sets.ads
--  Description     : A lock-free flat-set based on A. Gidenstam,
--                    M. Papatriantafilou and P. Tsigas,
--                    "Allocating memory in a lock-free manner",
--                    The 13th Annual European Symposium on Algorithms
--                    (ESA 2005), LNCS 3669, pages 329 - 242, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Tue Jan 15 17:41:28 2008
--  $Id: nbada-lock_free_flat_sets.ads,v 1.5 2008/07/29 14:35:07 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Atomic_Move;
with NBAda.Primitives;

generic

   type Element_Type is private;
   --  Element type.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Flat_Sets is

   Max_Set_Size : constant := 2**15;

   type Flat_Set_Size is new Natural range 0 .. Max_Set_Size - 1;
   type Flat_Set_Type (Size : Flat_Set_Size) is limited private;

   type Element_Reference is private;

   function New_Element (Element : Element_Type) return Element_Reference;

   type Element_Access is access all Element_Type;
   function Dereference (Ref : Element_Reference) return Element_Access;
   function "+" (Ref : Element_Reference) return Element_Access
     renames Dereference;
   --  NOTE: Do not store the returned Element_Access.

   procedure Get_Any (From    : in out Flat_Set_Type;
                      Element :    out Element_Reference);
   procedure Insert  (Into    : in out Flat_Set_Type;
                      Element : in out Element_Reference);

   Flat_Set_Empty : exception;
   Flat_Set_Full  : exception;

   --  Only for debugging. Not concurrency safe.
   procedure Dump (Set : in Flat_Set_Type);
   function Number_Of_Elements (Set : access Flat_Set_Type) return Natural;

private

   package AM is new Atomic_Move (Element_Type => Element_Type,
                                  Process_Ids  => Process_Ids);

   type Shared_Location_Array is array (Flat_Set_Size range <>) of
     aliased AM.Shared_Location;

   type Version_ID is mod 2**16;
   type Set_Index is mod Max_Set_Size;

   type Set_State is record
      Empty   : Boolean     := True;
      Last    : Set_Index   := 0;
      Version : Version_ID  := 0;
   end record;
   for Set_State use record
      Empty   at 0 range 0 .. 0;
      Last    at 0 range 1 .. 15;
      Version at 0 range 16 .. 31;
   end record;
   for Set_State'Size use 32;
   pragma Atomic (Set_State);

   type Flat_Set_Type (Size : Flat_Set_Size) is limited record
      State : aliased Set_State;
      pragma Atomic (State);
      Set   : Shared_Location_Array (0 .. Size);
   end record;

   type Element_Reference is new AM.Private_Reference;

end NBAda.Lock_Free_Flat_Sets;
