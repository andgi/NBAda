-------------------------------------------------------------------------------
--  Lock-Free Red Black Trees - An implementation of the lock-free red black
--                              tree algorithm by A. Gidenstam.
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
--  Filename        : nbada-lock_free_red_black_trees.ads
--  Description     : Lock-free algorithm for Red-Black trees by
--                    Anders Gidenstam. Based on the left leaning
--                    red-black trees by ...
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 21 22:50:08 2008
-- $Id: nbada-lock_free_red_black_trees.ads,v 1.1 2008/02/26 16:55:14 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (ALL_CHECKS);

pragma License (GPL);

with NBAda.Process_Identification;
with NBAda.Lock_Free_Red_Black_Trees_Memory_Reclamation_Adapter;

generic

   type Value_Type is private;
   type Key_Type is private;

   with function "<" (Left, Right : Key_Type) return Boolean is <>;
   --  Note: Key_Type must be totally ordered.

   with function Image (Key : Key_Type) return String is <>;

   with package Process_Ids is
     new NBAda.Process_Identification (<>);
   --  Process identification.

package NBAda.Lock_Free_Red_Black_Trees is

   type Dictionary_Type is limited private;

   Not_Found       : exception;
   Already_Present : exception;

   procedure Init    (Dictionary : in out Dictionary_Type);

   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type);

   procedure Delete  (From  : in out Dictionary_Type;
                      Key   : in     Key_Type);

   function  Lookup  (From  : in Dictionary_Type;
                      Key   : in Key_Type)
                     return Value_Type;

   procedure Verify (Tree  : in out Dictionary_Type;
                     Print : in     Boolean := False);

private

   package MR_Adapter is
      new Lock_Free_Red_Black_Trees_Memory_Reclamation_Adapter (Process_Ids);
   package MR renames MR_Adapter.Memory_Reclamation;

   type Node_Reference is new MR.Shared_Reference_Base;
   type Node_State_Reference is new MR.Shared_Reference_Base;
   --  These are atomic types since Shared_Reference_Base is.

   ----------------------------------------------------------------------
   type Tree_Node is
     new MR.Managed_Node_Base with
      record
         Key   : Key_Type;
         Value : Value_Type;
         State : aliased Node_State_Reference;
         pragma Atomic (State);
      end record;

   function All_References (Node : access Tree_Node)
                           return MR.Reference_Set;
   procedure Free (Node : access Tree_Node);

   package Node_Ops is new MR.Operations (Tree_Node,
                                          Node_Reference);

   ----------------------------------------------------------------------
   type Operation is (None, Rotate_Left, Rotate_Right);
   type Operation_Info is
      record
         Op          : Operation;
         Child       : Node_Reference;
         Grand_Child : Node_Reference;
         B_Subtree   : Node_Reference;
      end record;

   type Tree_Node_State is
     new MR.Managed_Node_Base with
      record
         Deleted     : Boolean := False;
         Left, Right : aliased Node_Reference;
         pragma Atomic (Left);
         pragma Atomic (Right);
      end record;

   function All_References (Node : access Tree_Node_State)
                           return MR.Reference_Set;
   procedure Free (Node : access Tree_Node_State);

   package Node_State_Ops is
      new MR.Operations (Tree_Node_State,
                         Node_State_Reference);

   ----------------------------------------------------------------------
   type Mutable_View (Self : access Dictionary_Type) is
     limited null record;

   type Dictionary_Type is
      record
         Root : aliased Node_Reference;
         pragma Atomic (Root);
         Mutable : Mutable_View (Dictionary_Type'Access);
      end record;

end NBAda.Lock_Free_Red_Black_Trees;
