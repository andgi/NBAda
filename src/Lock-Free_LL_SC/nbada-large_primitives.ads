-------------------------------------------------------------------------------
--  Large Primitives - An implementation of Maged Michael's LL/SC primitives.
--  Copyright (C) 2005  Anders Gidenstam
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
--  Filename        : large_primitives.ads
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    Based on the algorithm in Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 23 17:59:44 2005
--  $Id: nbada-large_primitives.ads,v 1.6 2005/09/23 17:27:35 anders Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Process_Identification;
with Hazard_Pointers;

generic
   Max_Number_Of_Links : Natural;
   --  Maximum number of simultaneous LL/SC per thread.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Large_Primitives is

   package HP is new Hazard_Pointers (Max_Number_Of_Links + 1,
                                      Process_Ids);

   generic
      type Element is private;
   package Load_Linked_Store_Conditional is

      type Shared_Element is limited private;

      function  Load_Linked (Target : access Shared_Element) return Element;

      function  Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element) return Boolean;

      procedure Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element);


      function  Verify_Link (Target : access Shared_Element) return Boolean;


      procedure Initialize (Target : access Shared_Element;
                            Value  : in     Element);
      --  Note: Initialize is only safe to use when there are no
      --        concurrent updates.

   private

      type Object_Value is new HP.Managed_Node_Base with
         record
            Value : Element;
         end record;
      procedure Free (Node : access Object_Value);

      package Object_Value_Operations is new HP.Operations (Object_Value);

      type Shared_Element is new Object_Value_Operations.Shared_Reference;

   end Load_Linked_Store_Conditional;

   procedure Print_Statistics;

end Large_Primitives;
