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

generic

   type Shared_Reference is new Shared_Reference_Base;
   --  All shared variables of type Shared_Reference MUST be declared
   --  atomic by 'pragma Atomic (Variable_Name);'.

   type Managed_Node is limited private;

   type Private_Node_Access is access all Managed_Node;
   --  Note: There SHOULD NOT be any shared variables of type
   --        Node_Access.

package NBAda.Memory_Reclamation.Reference_Operations is

   ----------------------------------------------------------------------------
   --  Reference enabled operations on private and shared references.
   ----------------------------------------------------------------------------

   type Private_Reference_Base is abstract tagged private;
   --  Note: There SHOULD NOT be any shared variables of type
   --        Private_Reference.

   function Null_Reference return Private_Reference_Base is abstract;

--   function  Dereference (MM   : in     Memory_Manager;
--                          Link : access Shared_Reference)
--                         return Private_Reference_Base is abstract;
   --  Note: Dereference preservs any mark on Link.all.
   --        In particular Mark (Null_Reference) /= Null_Reference.

   procedure Release (Node : in Private_Reference_Base) is abstract;

   function  "+"     (Node : in Private_Reference_Base)
                     return Private_Node_Access is abstract;

   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Private_Reference_Base;
                               New_Value : in Private_Reference_Base)
                              return Boolean is abstract;

   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Private_Reference_Base;
                               New_Value : in Private_Reference_Base)
      is abstract;

   procedure Delete  (Node : in Private_Reference_Base) is abstract;

   procedure Rescan  (Node : in Private_Reference_Base) is abstract;

   procedure Store   (Link : access Shared_Reference;
                      Node : in     Private_Reference_Base) is abstract;
   --  Note: Store is only safe to use when there cannot be any
   --        concurrent updates to Link.

   function "=" (Left, Right : in     Private_Reference_Base'Class)
                return Boolean;
   --  Private references are equal when they reference the same node and
   --  have the same marks.
   function "=" (Link : in     Shared_Reference;
                 Ref  : in     Private_Reference_Base'Class) return Boolean;
   function "=" (Ref  : in     Private_Reference_Base'Class;
                 Link : in     Shared_Reference) return Boolean;
   --  It is possible to compare a reference to the current value of a link.

   function Image (R : Private_Reference_Base) return String;


   ------------------------------------------------------------------------
   --  Unsafe operations.
   --  These SHOULD only be used when the user algorithm guarantees
   --  that the absence of premature reclamation or ABA-problems.
   --  In such algorithms the use of these operations in some particular
   --  situations could allow some performance improving optimizations.
   ------------------------------------------------------------------------

   type Unsafe_Reference_Value is private;
   --  Note: An Unsafe_Reference_Value does not keep a claim to any
   --        node and can therefore only be used where ABA safety is
   --        ensured by other means. It cannot be dereferenced.

   function Null_Reference_Value return Unsafe_Reference_Value;

   function  Unsafe_Read (Link : access Shared_Reference)
                         return Unsafe_Reference_Value;
   pragma Inline_Always (Unsafe_Read);

   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Unsafe_Reference_Value;
                               New_Value : in Private_Reference_Base)
                              return Boolean;
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Private_Reference_Base;
                               New_Value : in Unsafe_Reference_Value)
                              return Boolean;
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Unsafe_Reference_Value;
                               New_Value : in Unsafe_Reference_Value)
                              return Boolean;
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Unsafe_Reference_Value;
                               New_Value : in     Private_Reference_Base);
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Private_Reference_Base;
                               New_Value : in     Unsafe_Reference_Value);
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in     Unsafe_Reference_Value;
                               New_Value : in     Unsafe_Reference_Value);

   function "=" (Value : in     Unsafe_Reference_Value;
                 Ref   : in     Private_Reference_Base'Class) return Boolean;

   function "=" (Ref   : in     Private_Reference_Base'Class;
                 Value : in     Unsafe_Reference_Value) return Boolean;

   function "=" (Link  : in     Shared_Reference;
                 Value : in     Unsafe_Reference_Value) return Boolean;

   function "=" (Value : in     Unsafe_Reference_Value;
                 Link  : in     Shared_Reference) return Boolean;

   ------------------------------------------------------------------------
   generic

      type Private_Reference is new Private_Reference_Base with private;

   package Reference_Mark_Operations is

      --  Private (and shared) references can be tagged with a mark.
      procedure Mark      (Node : in out Private_Reference);
      function  Mark      (Node : in Private_Reference)
                          return Private_Reference;
      procedure Unmark    (Node : in out Private_Reference);
      function  Unmark    (Node : in Private_Reference)
                          return Private_Reference;
      function  Is_Marked (Node : in Private_Reference)
                          return Boolean;

      function  Is_Marked (Link : access Shared_Reference)
                          return Boolean;

      subtype Reference_Mark is NBAda.Memory_Reclamation.Reference_Mark;
      --  Operations for two marks. Mark A is the mark above.
      --  The Unmark operation above removes all marks.
      function A  return Reference_Mark
        renames NBAda.Memory_Reclamation.A;
      function B  return Reference_Mark
        renames NBAda.Memory_Reclamation.B;
      function AB return Reference_Mark
        renames NBAda.Memory_Reclamation.AB;

      procedure Mark      (Node : in out Private_Reference;
                           Mark : in     Reference_Mark);
      function  Mark      (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Private_Reference;
      procedure Unmark    (Node : in out Private_Reference;
                           Mark : in     Reference_Mark);
      function  Unmark    (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Private_Reference;
      function  Is_Marked (Node : in     Private_Reference;
                           Mark : in     Reference_Mark)
                          return Boolean;

      function  Is_Marked (Link : access Shared_Reference;
                           Mark : in     Reference_Mark)
                          return Boolean;

      ----------------------------------------------------------------------
      function  Is_Marked (Value : in     Unsafe_Reference_Value)
                          return Boolean;
      pragma Inline_Always (Is_Marked);
      function  Is_Marked (Value : in     Unsafe_Reference_Value;
                           Mark  : in     Reference_Mark)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function  Mark      (Value : in     Unsafe_Reference_Value)
                          return Unsafe_Reference_Value;
      pragma Inline_Always (Mark);
      function  Mark      (Value : in     Unsafe_Reference_Value;
                           Mark  : in     Reference_Mark)
                          return Unsafe_Reference_Value;

   end Reference_Mark_Operations;
   ------------------------------------------------------------------------

   ------------------------------------------------------------------------
   function To_Shared_Reference (R : in     Private_Reference_Base)
                                return Shared_Reference;

   function From_Shared_Reference (R : Shared_Reference)
                                  return Private_Reference_Base'Class;

private

   type Private_Reference_Base is abstract tagged
      record
         Ref : Reference_Impl := 0;
      end record;

   type Unsafe_Reference_Value is new Reference_Impl;

   type Secret_Concrete is new Private_Reference_Base with null record;
   function Null_Reference return Secret_Concrete;
--   function  Dereference (MM   : in     Memory_Manager;
--                          Link : access Shared_Reference)
--                         return Secret_Concrete;
   procedure Release (Node : in Secret_Concrete);
   function  "+"     (Node : in Secret_Concrete)
                     return Private_Node_Access;
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Secret_Concrete;
                               New_Value : in Secret_Concrete)
                              return Boolean;
   procedure Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Secret_Concrete;
                               New_Value : in Secret_Concrete);
   procedure Delete  (Node : in Secret_Concrete);
   procedure Rescan  (Node : in Secret_Concrete);
   procedure Store   (Link : access Shared_Reference;
                      Node : in     Secret_Concrete);

end NBAda.Memory_Reclamation.Reference_Operations;
