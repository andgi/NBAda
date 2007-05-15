-------------------------------------------------------------------------------
--  Atomic M-component N-process snapshot implementation based on P. Jayanti,
--  "An Optimal Multi-Writer Snapshot Algorithm", Proceedings of STOC'05,
--  ACM, 2005.
--
--  Copyright (C) 2007  Anders Gidenstam
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
--  Filename        : atomic_multiwriter_snapshots.ads
--  Description     : M-component N-process snapshot implementation based on
--                    P. Jayanti, "An Optimal Multi-Writer Snapshot Algorithm",
--                    Proceedings of STOC'05, ACM, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Fri May 11 15:58:30 2007
--  $Id: nbada-atomic_multiwriter_snapshots.ads,v 1.1 2007/05/15 14:02:15 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Process_Identification;
with Primitives;

generic

   Max_Number_Of_Components : Natural;
   --  Maximum number of components in the snapshot.

   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.

package Atomic_Multiwriter_Snapshots is

   pragma Elaborate_Body;

   type Snapshot (<>) is private;

   function Scan return Snapshot;

   Maximum_Number_Of_Components_Exceeded : exception;

   generic
      --  Use pragma Atomic and pragma Volatile for Target.
      --  Element'Object_Size MUST be System.Word_Size.
      type Element is private;
   package Element_Components is

      type Element_Component is private;

      function Create (Default_Value : in Element) return Element_Component;

      procedure Write (To    : in Element_Component;
                       Value : in Element);

      function Read (Component : in Element_Component;
                     From      : in Snapshot) return Element;

   private

      type Element_Component is
         record
            Index : Natural := 0;
         end record;

   end Element_Components;

private

   type Component_Index is new Natural range 0 .. Max_Number_Of_Components;

   type Component_Impl is new Primitives.Standard_Unsigned;
   type Component_Array is array (Component_Index) of Component_Impl;

   type Snapshot is new Component_Array;

end Atomic_Multiwriter_Snapshots;
