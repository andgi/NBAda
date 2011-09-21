-------------------------------------------------------------------------------
--  NBAda - A library of non-blocking algorithms and data structures.
--
--  Copyright (C) 2011  Anders Gidenstam
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

pragma License (GPL);
pragma Style_Checks (ALL_CHECKS);

with NBAda.Primitives;

package NBAda.Memory_Reclamation is

   ----------------------------------------------------------------------------
   type Shared_Reference_Base is limited private;
   --  For type separation between shared references to different
   --  managed types derive your own shared reference types from
   --  Shared_Reference_Base and instantiate the memory management
   --  operation package below for each of them.

   type Reference_Mark is (A, B, AB);
   --  Private and shared references can be tagged with marks.

private

   type Reference_Impl is new Primitives.Standard_Unsigned;

   type Shared_Reference_Base is
      record
         Ref : aliased Reference_Impl := 0;
         pragma Atomic (Ref);
      end record;
   for Shared_Reference_Base'Size use Primitives.Standard_Unsigned'Size;
   pragma Atomic (Shared_Reference_Base);

   function Boolean_Compare_And_Swap_Impl is
      new Primitives.Standard_Boolean_Compare_And_Swap (Reference_Impl);
   procedure Void_Compare_And_Swap_Impl is
      new Primitives.Standard_Void_Compare_And_Swap (Reference_Impl);

   Mark_Bits  : constant := 2;
   --  Note: Reference_Counted_Node_Base'Alignment >= 2 ** Mark_Bits MUST hold.
   Mark_Mask  : constant array (Reference_Mark) of Reference_Impl
     := (A =>  1,
         B =>  2,
         AB => 3);
   Ref_Mask   : constant Reference_Impl := -(2 ** Mark_Bits);

end NBAda.Memory_Reclamation;
