-------------------------------------------------------------------------------
--  Lock-Free Sets - An implementation of the lock-free set algorithm by
--                   M. Michael.
--
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_sets.ads
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 11:54:37 2006
--  $Id: nbada-lock_free_sets.ads,v 1.4 2007/08/31 15:53:15 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with Process_Identification;

with
--  Epoch_Based_Memory_Reclamation;
  Hazard_Pointers;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package Lock_Free_Sets is

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

   package Memory_Reclamation_Scheme is
--      new Epoch_Based_Memory_Reclamation (Process_Ids => Process_Ids);
        new Hazard_Pointers (Process_Ids                => Process_Ids,
                             Max_Number_Of_Dereferences => 4);
   package MRS renames Memory_Reclamation_Scheme;

private

   type List_Node_Reference is new MRS.Shared_Reference_Base;

   type List_Node is new MRS.Managed_Node_Base with
      record
         Next  : aliased List_Node_Reference;
         pragma Atomic (Next);
         Key   : Key_Type;
         Value : Value_Type;
      end record;

   procedure Free     (Node : access List_Node);

   package MRS_Ops is new MRS.Reference_Operations (List_Node,
                                                    List_Node_Reference);

   subtype List_Node_Access is MRS_Ops.Private_Reference;

   type Set_Type is limited
      record
         Head : aliased List_Node_Reference;
         pragma Atomic (Head);
      end record;

end Lock_Free_Sets;
