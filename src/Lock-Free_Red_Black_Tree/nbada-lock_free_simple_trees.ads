-------------------------------------------------------------------------------
--  Lock-Free Simple Trees - An implementation of a lock-free simple
--                           tree algorithm by A. Gidenstam.
--
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
pragma Style_Checks (OFF);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_simple_trees.ads
--  Description     : Lock-free algorithm for simple binary trees by
--                    Anders Gidenstam.
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 21 22:50:08 2008
-- $Id: nbada-lock_free_simple_trees.ads,v 1.2 2008/03/10 13:00:34 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Lock_Free_Simple_Trees_Memory_Reclamation_Adapter;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with function Image (Key : Key_Type) return String is <>;

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Simple_Trees is

   type Dictionary_Type is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Dictionary : in out Dictionary_Type);

   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type);

   function  Delete (From  : in Dictionary_Type;
                     Key   : in Key_Type)
                    return Value_Type;

   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type;

   procedure Verify (Tree  : in out Dictionary_Type;
                     Print : in     Boolean := False);

private

   package MR_Adapter is
      new Lock_Free_Simple_Trees_Memory_Reclamation_Adapter (Process_Ids);
   package Node_MR renames MR_Adapter.Node_Memory_Reclamation;
   package State_MR renames MR_Adapter.State_Memory_Reclamation;

   type Node_Reference  is new Node_MR.Shared_Reference_Base;
   type State_Reference is new State_MR.Shared_Reference_Base;
   --  These are atomic types since Shared_Reference_Base is.

   type Atomic_Boolean is new Boolean;
   for Atomic_Boolean'Size use 32;
   pragma Atomic (Atomic_Boolean);

   ----------------------------------------------------------------------
   type Node_State is
     new State_MR.Managed_Node_Base with
      record
         Value   : Value_Type;
         Deleted : Boolean := False;
      end record;

   procedure Free (State : access Node_State);

   package State_Ops is new State_MR.Reference_Operations (Node_State,
                                                           State_Reference);

   ----------------------------------------------------------------------
   type Tree_Node is
     new Node_MR.Managed_Node_Base with
      record
         Key     : Key_Type;
         State   : aliased State_Reference;
         pragma Atomic (State);
         Left    : aliased Node_Reference;
         pragma Atomic (Left);
         Right   : aliased Node_Reference;
         pragma Atomic (Right);
      end record;

   function All_References (Node : access Tree_Node)
                           return Node_MR.Reference_Set;
   procedure Free (Node : access Tree_Node);

   package Node_Ops is new Node_MR.Operations (Tree_Node,
                                               Node_Reference);

   ----------------------------------------------------------------------
   type Mutable_View (Self : access Dictionary_Type) is
     limited null record;

   type Dictionary_Type is
      record
         Root : aliased Node_Reference;
         pragma Atomic (Root);
         Mutable : Mutable_View (Dictionary_Type'Access);
      end record;

end NBAda.Lock_Free_Simple_Trees;
