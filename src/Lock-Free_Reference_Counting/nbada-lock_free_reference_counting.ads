-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - Lock-Free Reference Counting based on the
--  algorithm by Herlihy et al.
--  Copyright (C) 2006  Anders Gidenstam
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_reference_counting.ads
--  Description     : Ada implementation of lock-free reference counting.
--                    Based on M. Herlihy, V. Luchango, P. Martin, M. Moir,
--                    "Nonblocking Memory Management Support for Dynamic-Sized
--                     Data Structures",  ACM Transactions on Computer Systems,
--                    23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Nov 29 16:42:38 2006
--  $Id: nbada-lock_free_reference_counting.ads,v 1.4 2006/12/01 10:41:58 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Primitives;

generic

   Max_Number_Of_Guards : Natural;
   --  The maximum number of simultaneously active guards.
   --  (Here: One guard per thread is good.)

   Debug : Boolean := False;
   --  Enables some profiling.

package Lock_Free_Reference_Counting is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   type Managed_Node_Base is abstract tagged limited private;
   --  Inherit from this base type to create your own managed types.

--   procedure Dispose  (Node       : access Managed_Node_Base) is abstract;
   --  Dispose should set all shared references inside the node to null.

   procedure Free (Object : access Managed_Node_Base) is abstract;
   --  Note: Due to some peculiarities of the Ada storage pool
   --        management managed nodes need to have a dispatching primitive
   --        operation that calls the instance of Unchecked_Deallocation
   --        appropriate for the specific node type at hand. Without
   --        this the wrong instance of Unchecked_Deallocation might get
   --        called - often with disastrous consequences as it tries return
   --        the memory to the wrong storage pool.
   --        This workaround is not very nice but I have not found any
   --        better way.

   ----------------------------------------------------------------------------
   type Shared_Reference_Base is limited private;
   --  For type separation between shared references to different
   --  managed types derive your own shared reference types from
   --  Shared_Reference_Base and instantiate the memory management
   --  operation package below for each of them.

   type Shared_Reference_Base_Access is access all Shared_Reference_Base;
   type Reference_Set is array (Integer range <>) of
     Shared_Reference_Base_Access;

   function All_References (Node : access Managed_Node_Base)
                           return Reference_Set is abstract;

   ----------------------------------------------------------------------------
   generic

      type Managed_Node is
        new Managed_Node_Base with private;

      type Shared_Reference is new Shared_Reference_Base;
      --  All shared variables of type Shared_Reference MUST be declared
      --  atomic by 'pragma Atomic (Variable_Name);' .

   package Operations is

      type Node_Access is access all Managed_Node;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Node_Access.

      type Private_Reference is private;
      --  Note: There SHOULD NOT be any shared variables of type
      --        Private_Reference.

      Null_Reference : constant Private_Reference;

      ----------------------------------------------------------------------
      --  Operations.
      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference;

      procedure Release (Node : in Private_Reference);

      function  "+"     (Node : in Private_Reference)
                        return Node_Access;
      pragma Inline_Always ("+");
      function  Deref   (Node : in Private_Reference)
                        return Node_Access;

      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean;

      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Private_Reference;
                                  New_Value : in     Private_Reference);

      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference);


      generic
         type User_Node_Access is access Managed_Node;
         --  Select an appropriate (preferably non-blocking) storage
         --  pool by the "for User_Node_Access'Storage_Pool use ..."
         --  construct.
         --  Note: The nodes allocated in this way must have an
         --        implementation of Free that use the same storage pool.
      function Create return Private_Reference;
      --  Creates a new User_Node and returns a safe reference to it.

      --  Private (and shared) references can be tagged with a mark.
      --  NOTE: A private reference with the value Null_Reference always loses
      --        its mark.
      procedure Mark      (Node : in out Private_Reference);
      pragma Inline_Always (Mark);
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference;
      pragma Inline_Always (Mark);
      procedure Unmark    (Node : in out Private_Reference);
      pragma Inline_Always (Unmark);
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference;
      pragma Inline_Always (Unmark);
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean;
      pragma Inline_Always (Is_Marked);

      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean;
      pragma Inline_Always ("=");
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean;
      pragma Inline_Always ("=");
      --  It is possible to compare a reference to the current value of a link.

   private

      type Private_Reference is mod 2 ** 32;

      Null_Reference : constant Private_Reference := 0;

   end Operations;

   procedure Print_Statistics;

private

   subtype Reference_Count is Primitives.Unsigned_32;

   type Managed_Node_Base is abstract tagged limited
      record
         MM_RC    : aliased Reference_Count := 1;
         pragma Atomic (MM_RC);
      end record;

   type Managed_Node_Access is
     access all Managed_Node_Base'Class;

   type Shared_Reference_Base_Impl is mod 2 ** 32;
   type Shared_Reference_Base is
      record
         Ref : Shared_Reference_Base_Impl := 0;
      end record;
   for Shared_Reference_Base'Size use 32;
   pragma Atomic (Shared_Reference_Base);

   Null_Reference : constant Shared_Reference_Base := (Ref => 0);

   Mark_Bits  : constant := 1;
   --  Note: Reference_Counted_Node_Base'Alignment >= 2 ** Mark_Bits MUST hold.
   Mark_Mask  : constant Shared_Reference_Base_Impl := 2 ** Mark_Bits - 1;
   Ref_Mask   : constant Shared_Reference_Base_Impl := -(2 ** Mark_Bits);

end Lock_Free_Reference_Counting;
