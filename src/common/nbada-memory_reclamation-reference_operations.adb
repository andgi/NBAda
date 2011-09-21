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

with Ada.Unchecked_Conversion;
with NBAda.Interfaces.Exceptions;

package body NBAda.Memory_Reclamation.Reference_Operations is

   ----------------------------------------------------------------------------
   type Shared_Reference_Access is access all Shared_Reference;
   type Reference_Impl_Access is access all
     Reference_Impl;
   function To_Reference_Impl_Access is
      new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                    Reference_Impl_Access);

   ----------------------------------------------------------------------
   function "=" (Left, Right : in     Private_Reference_Base'Class)
                return Boolean is
   begin
      return Left.Ref = Right.Ref;
   end "=";

   ----------------------------------------------------------------------
   function "=" (Link : in     Shared_Reference;
                 Ref  : in     Private_Reference_Base'Class) return Boolean is
   begin
      return Shared_Reference_Base (Link).Ref = Ref.Ref;
   end "=";

   ----------------------------------------------------------------------
   function "=" (Ref  : in     Private_Reference_Base'Class;
                 Link : in     Shared_Reference) return Boolean is
   begin
      return Shared_Reference_Base (Link).Ref = Ref.Ref;
   end "=";

   ----------------------------------------------------------------------
   function Image (R : Private_Reference_Base) return String is
   begin
      return Reference_Impl'Image (R.Ref);
   end Image;


   ----------------------------------------------------------------------
   function Null_Reference_Value return Unsafe_Reference_Value is
   begin
      return 0;
   end Null_Reference_Value;

   ----------------------------------------------------------------------
   function  Unsafe_Read (Link : access Shared_Reference)
                         return Unsafe_Reference_Value is
   begin
      return Unsafe_Reference_Value (Shared_Reference_Base (Link.all).Ref);
   end Unsafe_Read;

   ----------------------------------------------------------------------
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Unsafe_Reference_Value;
                               New_Value : in Private_Reference_Base)
                              return Boolean is
   begin
      return Boolean_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Reference_Impl (Old_Value),
         New_Value => New_Value.Ref);
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Private_Reference_Base;
                               New_Value : in Unsafe_Reference_Value)
                              return Boolean is
   begin
      return Boolean_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Old_Value.Ref,
         New_Value => Reference_Impl (New_Value));
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Unsafe_Reference_Value;
                               New_Value : in Unsafe_Reference_Value)
                              return Boolean is
   begin
      return Boolean_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Reference_Impl (Old_Value),
         New_Value => Reference_Impl (New_Value));
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Unsafe_Reference_Value;
                               New_Value : in     Private_Reference_Base) is
   begin
      Void_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Reference_Impl (Old_Value),
         New_Value => New_Value.Ref);
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Private_Reference_Base;
                               New_Value : in     Unsafe_Reference_Value) is
   begin
      Void_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Old_Value.Ref,
         New_Value => Reference_Impl (New_Value));
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Unsafe_Reference_Value;
                               New_Value : in     Unsafe_Reference_Value) is
   begin
      Void_Compare_And_Swap_Impl
        (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
         Old_Value => Reference_Impl (Old_Value),
         New_Value => Reference_Impl (New_Value));
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   function "=" (Value : in     Unsafe_Reference_Value;
                 Ref   : in     Private_Reference_Base'Class) return Boolean is
   begin
      return Reference_Impl (Value) = Ref.Ref;
   end "=";

   ----------------------------------------------------------------------
   function "=" (Ref   : in     Private_Reference_Base'Class;
                 Value : in     Unsafe_Reference_Value) return Boolean is
   begin
      return Reference_Impl (Value) = Ref.Ref;
   end "=";

   ----------------------------------------------------------------------
   function "=" (Link  : in     Shared_Reference;
                 Value : in     Unsafe_Reference_Value) return Boolean is
   begin
      return Shared_Reference_Base (Link).Ref = Reference_Impl (Value);
   end "=";

   ----------------------------------------------------------------------
   function "=" (Value : in     Unsafe_Reference_Value;
                 Link  : in     Shared_Reference) return Boolean is
   begin
      return Shared_Reference_Base (Link).Ref = Reference_Impl (Value);
   end "=";

   -------------------------------------------------------------------------
   package body Reference_Mark_Operations is

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference) is
      begin
         Node.Ref := Node.Ref or Mark_Mask (A);
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
         Result : Private_Reference := Node;
      begin
         Result.Ref := Node.Ref or Mark_Mask (A);
         return Result;
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node.Ref := Node.Ref and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
         Result : Private_Reference := Node;
      begin
         Result.Ref := Node.Ref and Ref_Mask;
         return Result;
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node.Ref and Mark_Mask (A)) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Link : access Shared_Reference)
                          return Boolean is
      begin
         return (Shared_Reference_Base (Link.all).Ref and Mark_Mask (A)) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference;
                           Mark : in     Reference_Mark) is
      begin
         Node.Ref := Node.Ref or Mark_Mask (Mark);
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Private_Reference is
         Result : Private_Reference := Node;
      begin
         Result.Ref := Node.Ref or Mark_Mask (Mark);
         return Result;
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference;
                           Mark : in     Reference_Mark) is
      begin
         Node.Ref := Node.Ref - (Node.Ref and Mark_Mask (Mark));
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Private_Reference is
         Result : Private_Reference := Node;
      begin
         Result.Ref := Node.Ref - (Node.Ref and Mark_Mask (Mark));
         return Result;
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Boolean is
      begin
         return (Node.Ref and Mark_Mask (Mark)) = Mark_Mask (Mark);
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Link : access Shared_Reference;
                           Mark : in     Reference_Mark)
                          return Boolean is
      begin
         return
           (Shared_Reference_Base (Link.all).Ref and
              Mark_Mask (Mark)) = Mark_Mask (Mark);
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Value : in     Unsafe_Reference_Value)
                          return Boolean is
      begin
         return (Reference_Impl (Value) and Mark_Mask (A)) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Value : in     Unsafe_Reference_Value;
                           Mark  : in     Reference_Mark)
                          return Boolean is
      begin
         return (Reference_Impl (Value) and Mark_Mask (Mark)) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Mark      (Value : in     Unsafe_Reference_Value)
                          return Unsafe_Reference_Value is
      begin
         return Value or Unsafe_Reference_Value (Mark_Mask (A));
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Value : in     Unsafe_Reference_Value;
                           Mark  : in     Reference_Mark)
                          return Unsafe_Reference_Value is
      begin
         return Value or Unsafe_Reference_Value (Mark_Mask (Mark));
      end Mark;

   end Reference_Mark_Operations;


   ----------------------------------------------------------------------
   function To_Shared_Reference (R : in     Private_Reference_Base)
                                return Shared_Reference is
   begin
      return Shared_Reference (Shared_Reference_Base'(Ref => R.Ref));
   end To_Shared_Reference;

   ----------------------------------------------------------------------
   function From_Shared_Reference (R : in     Shared_Reference)
                                  return Private_Reference_Base'Class is
   begin
      return Secret_Concrete'(Ref => Shared_Reference_Base (R).Ref);
   end From_Shared_Reference;

   ----------------------------------------------------------------------
   function Null_Reference return Secret_Concrete is
   begin
      return (Ref => 0);
   end Null_Reference;

   ----------------------------------------------------------------------
--   function  Dereference (MM   : in     Memory_Manager;
--                          Link : access Shared_Reference)
--                         return Secret_Concrete is
--   begin
--      raise NBAda.Interfaces.Exceptions.Not_Implemented;
--      return (Ref => 0);
--   end Dereference;

   ----------------------------------------------------------------------
   procedure Release (Node : in Secret_Concrete) is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
   end Release;

   ----------------------------------------------------------------------
   function  "+"     (Node : in Secret_Concrete)
                     return Private_Node_Access is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
      return null;
   end "+";

   ----------------------------------------------------------------------
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Secret_Concrete;
                               New_Value : in Secret_Concrete)
                              return Boolean is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
      return False;
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Secret_Concrete;
                               New_Value : in Secret_Concrete) is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
   end Compare_And_Swap;

   ----------------------------------------------------------------------
   procedure Delete  (Node : in Secret_Concrete) is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
   end Delete;

   ----------------------------------------------------------------------
   procedure Rescan  (Node : in Secret_Concrete) is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
   end Rescan;

   ----------------------------------------------------------------------
   procedure Store   (Link : access Shared_Reference;
                      Node : in     Secret_Concrete) is
   begin
      raise NBAda.Interfaces.Exceptions.Not_Implemented;
   end Store;

end NBAda.Memory_Reclamation.Reference_Operations;
