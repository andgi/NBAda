-------------------------------------------------------------------------------
--  Lock-Free Sets - An implementation of the lock-free set algorithm by
--                   M. Michael.
--
--  Copyright (C) 2006 - 2011  Anders Gidenstam
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--  Filename        : nbada-lock_free_sets.ads
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 11:54:37 2006
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Process_Identification;

with NBAda.Lock_Free_Sets_Memory_Reclamation_Adapter;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Sets is

   ----------------------------------------------------------------------------
   --  Lock-free Set.
   ----------------------------------------------------------------------------
   type Set_Type is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Set : in out Set_Type);

   procedure Insert  (Into  : in out Set_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete  (From : in out Set_Type;
                      Key  : in     Key_Type);

   function  Find    (In_Set : in Set_Type;
                      Key    : in Key_Type) return Value_Type;

--  private

   package MR_Adapter is new
     Lock_Free_Sets_Memory_Reclamation_Adapter (Process_Ids => Process_Ids);
   package MRS renames MR_Adapter.Memory_Reclamation;

private

   type List_Node_Reference is
     new NBAda.Memory_Reclamation.Shared_Reference_Base;

   type List_Node is new MRS.Managed_Node_Base with
      record
         Next  : aliased List_Node_Reference;
         pragma Atomic (Next);
         Key   : Key_Type;
         Value : Value_Type;
      end record;

   procedure Free     (Node : access List_Node);

   package MR_Ops is new MRS.Reference_Operations (List_Node,
                                                   List_Node_Reference);

   type Set_Type is limited
      record
         MM   : MR_Ops.Memory_Manager;
         Head : aliased List_Node_Reference;
         pragma Atomic (Head);
      end record;

end NBAda.Lock_Free_Sets;
