-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : large_primitives.ads
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    From Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 23 17:59:44 2005
--  $Id: nbada-large_primitives.ads,v 1.1 2005/02/24 16:05:28 anders Exp $
-------------------------------------------------------------------------------

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

      function Load_Linked (Target : access Shared_Element) return Element;

      function Store_Conditional (Target : access Shared_Element;
                                  Value  : in     Element) return Boolean;

      function Verify_Link (Target : access Shared_Element) return Boolean;


      procedure Initialize (Target : access Shared_Element;
                            Value  : in     Element);
      --  Note: Initialize is only safe to use when there are no
      --        concurrent updates.

   private

      type Object_Value is new HP.Managed_Node with
         record
            Value : Element;
         end record;
      type Object_Value_Access is access all Object_Value;

      type Shared_Element is new HP.Shared_Reference;

   end Load_Linked_Store_Conditional;

   procedure Print_Statistics;

end Large_Primitives;
