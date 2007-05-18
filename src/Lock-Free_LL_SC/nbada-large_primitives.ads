-------------------------------------------------------------------------------
--  Large Primitives - An implementation of Maged Michael's LL/SC primitives.
--  Copyright (C) 2005 - 2007   Anders Gidenstam
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
--  Filename        : large_primitives.ads
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    Based on the algorithm in Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 23 17:59:44 2005
--  $Id: nbada-large_primitives.ads,v 1.10 2007/05/18 12:10:52 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Process_Identification;

with
  Hazard_Pointers;
--  Epoch_Based_Memory_Reclamation;

generic
   Max_Number_Of_Links : Natural;
   --  Maximum number of simultaneous LL/SC per thread.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Large_Primitives is

   package MR is
      new Hazard_Pointers (Max_Number_Of_Links + 1,
                           Process_Ids);
--      new Epoch_Based_Memory_Reclamation (Epoch_Update_Threshold => 100,
--                                          --  Suitable number for epoch-based
--                                          --  reclamation.
--                                          Process_Ids => Process_Ids);

   generic
      type Element is private;
   package Load_Linked_Store_Conditional is

      type Shared_Element is limited private;

      function  Load_Linked (Target : in Shared_Element) return Element;
      function  Load_Linked (Target : access Shared_Element) return Element;

      function  Store_Conditional (Target : in     Shared_Element;
                                   Value  : in     Element) return Boolean;
      function  Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element) return Boolean;

      procedure Store_Conditional (Target : in out Shared_Element;
                                   Value  : in     Element);
      procedure Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element);


      function  Verify_Link (Target : in Shared_Element) return Boolean;
      function  Verify_Link (Target : access Shared_Element) return Boolean;


      procedure Initialize (Target : in out Shared_Element;
                            Value  : in     Element);
      procedure Initialize (Target : access Shared_Element;
                            Value  : in     Element);
      --  Note: Initialize is only safe to use when there are no
      --        concurrent updates.

   private

      type Object_Value is new MR.Managed_Node_Base with
         record
            Value : Element;
         end record;
      procedure Free (Node : access Object_Value);

      package Object_Value_Operations is new MR.Operations (Object_Value);

      type Shared_Reference is new Object_Value_Operations.Shared_Reference;

      type Shared_Element is tagged limited
         record
            Reference : aliased Shared_Reference;
            pragma Atomic (Reference);
         end record;

   end Load_Linked_Store_Conditional;

   procedure Print_Statistics;

end Large_Primitives;
