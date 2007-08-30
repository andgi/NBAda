-------------------------------------------------------------------------------
--  Large Primitives - An implementation of Maged Michael's LL/SC primitives.
--  Copyright (C) 2005 - 2007  Anders Gidenstam
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
--  Filename        : large_primitives.adb
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    Based on the algorithm in Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 24 10:25:44 2005
--  $Id: nbada-large_primitives.adb,v 1.17 2007/08/30 16:42:12 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Conversion;
with System;

with NBAda.Lock_Free_Fixed_Size_Storage_Pools;
with Ada.Unchecked_Deallocation;

with NBAda.Primitives;
with Ada.Text_IO;

package body NBAda.Large_Primitives is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;
   type    Exp_Index is new Integer range 1 .. Max_Number_Of_Links;

   type Link is
      record
         Target : MR.Managed_Node_Access;
         Source : System.Address;
      end record;

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Process local static data.
   Exp  : array (Processes, Exp_Index) of Link;
   Next : array (Processes) of Exp_Index := (others => 1);

   --  Shared statistics.
   Allocated : aliased Primitives.Standard_Unsigned := 0;
   pragma Atomic (Allocated);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   package body Load_Linked_Store_Conditional is

      -------------------------------------------------------------------------
      --  Types and static variables.
      -------------------------------------------------------------------------
      type Shared_Element_Access is access all Shared_Reference;
      subtype Object_Value_Access is Object_Value_Operations.Node_Access;
      use type Object_Value_Access;

      function  Get_Block  (ID : in Processes) return Object_Value_Access;
      procedure Keep_Block (ID : in Processes;
                            B  : in Object_Value_Access);



      --  Process local static data.
      Safe_Block : array (Processes) of Object_Value_Access;

      -------------------------------------------------------------------------
      --  Storage pool for the nodes.
      -------------------------------------------------------------------------

      Pool_Size : constant Natural :=
        2 * (Max_Number_Of_Links + 1) * Natural (Processes'Last) ** 2;
      --  This should be a conservative and safe number.

      Node_Pool :
        Lock_Free_Fixed_Size_Storage_Pools.Lock_Free_Storage_Pool
        (Pool_Size  =>
           Lock_Free_Fixed_Size_Storage_Pools.Block_Count (Pool_Size),
         Block_Size => Object_Value'Max_Size_In_Storage_Elements);

      type New_Object_Value_Access is access Object_Value;
      for New_Object_Value_Access'Storage_Pool use Node_Pool;

      -------------------------------------------------------------------------
      function  Load_Linked (Target : Shared_Element) return Element is
         --  Unrestricted access is safe here since Shared_Element is
         --  a by-reference type.
      begin
         return Load_Linked (Target'Unrestricted_Access);
      end Load_Linked;

      -------------------------------------------------------------------------
      function Load_Linked (Target : access Shared_Element) return Element is
         ID : constant Processes := Process_Ids.Process_ID;
         use MR;
         use Object_Value_Operations;
      begin
         Exp (ID, Next (ID)).Source := Target.all'Address;
         MR.Release (Exp (ID, Next (ID)).Target);
         Exp (ID, Next (ID)).Target :=
           Managed_Node_Access (Dereference (Target.Reference'Access));

         if
           Exp (ID, Next (ID)).Target /= null and then
           Exp (ID, Next (ID)).Target.all in Object_Value
         then

            declare
               Val : constant Object_Value_Access :=
                 Object_Value_Access (Exp (ID, Next (ID)).Target);
            begin
               if Next (ID) = Exp_Index'Last then
                  Next (ID) := Exp_Index'First;
               else
                  Next (ID) := Next (ID) + 1;
               end if;

               return Val.Value;
            end;
         else
            Release (Exp (ID, Next (ID)).Target);
            raise Constraint_Error;
         end if;
      end Load_Linked;

      -------------------------------------------------------------------------
      function Store_Conditional (Target : Shared_Element;
                                  Value  : Element) return Boolean is
      begin
         return Store_Conditional (Target'Unrestricted_Access, Value);
      end Store_Conditional;

      -------------------------------------------------------------------------
      function Store_Conditional (Target : access Shared_Element;
                                  Value  : in     Element) return Boolean is
         ID : constant Processes := Process_Ids.Process_ID;
         use type System.Address;
         use MR;
         use Object_Value_Operations;
         Val : constant Object_Value_Access := Get_Block (ID);
      begin
         Val.Value := Value;

         for I in Exp'Range (2) loop
            if Exp (ID, I).Source = Target.all'Address then
               declare
                  Old : constant Object_Value_Access :=
                    Object_Value_Access (Exp (ID, I).Target);
               begin
                  --  Clear this link.
                  Exp (ID, I).Source := System.Null_Address;
                  Exp (ID, I).Target := null;

                  if Boolean_Compare_And_Swap
                    (Shared    => Target.Reference'Access,
                     Old_Value => Old,
                     New_Value => Node_Access (Val))
                  then
                     Delete (Old);
                     return True;
                  else
                     Keep_Block (ID, Val);
                     Release (Old);
                     return False;
                  end if;
               end;
            end if;
         end loop;
         return False;
      end Store_Conditional;

      -------------------------------------------------------------------------
      procedure Store_Conditional (Target : in out Shared_Element;
                                   Value  : in     Element) is
      begin
         if Store_Conditional (Target'Unrestricted_Access, Value) then
            null;
         end if;
      end Store_Conditional;

      -------------------------------------------------------------------------
      procedure Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element) is
      begin
         if Store_Conditional (Target, Value) then
            null;
         end if;
      end Store_Conditional;

      -------------------------------------------------------------------------
      function Verify_Link (Target : in Shared_Element) return Boolean is
      begin
         return Verify_Link (Target'Unrestricted_Access);
      end Verify_Link;

      -------------------------------------------------------------------------
      function Verify_Link (Target : access Shared_Element) return Boolean is
         ID : constant Processes := Process_Ids.Process_ID;
         use MR;
         use Object_Value_Operations;
         use type System.Address;
      begin
         for I in Exp'Range (2) loop
            if Exp (ID, I).Source =  Target.all'Address then
               declare
                  Tmp : constant Managed_Node_Access :=
                    Managed_Node_Access
                    (Dereference (Target.Reference'Access));
               begin
                  Release (Tmp);
                  return Tmp = Exp (ID, I).Target;
               end;
            end if;
         end loop;
         return False;
      end Verify_Link;

      -------------------------------------------------------------------------
      procedure Initialize (Target : in out Shared_Element;
                            Value  : in     Element) is
      begin
         Initialize (Target'Access, Value);
      end Initialize;

      -------------------------------------------------------------------------
      procedure Initialize (Target : access Shared_Element;
                            Value  : in     Element) is
         Val : Object_Value_Access;

         type Shared_Reference_Access is
           access all Object_Value_Operations.Shared_Reference;

         function To_Shared_Reference is
              new Ada.Unchecked_Conversion (Shared_Element_Access,
                                            Shared_Reference_Access);
      begin
         Primitives.Fetch_And_Add (Allocated'Access, 1);
         Val :=
           Object_Value_Access (New_Object_Value_Access'(new Object_Value));

         Val.Value := Value;
         Object_Value_Operations.Initialize
           (To_Shared_Reference (Target.Reference'Access),
            Val);
      end Initialize;

      -------------------------------------------------------------------------
      --  Private operations.
      -------------------------------------------------------------------------

      -------------------------------------------------------------------------
      procedure Free (Node : access Object_Value) is
         procedure Reclaim is new
           Ada.Unchecked_Deallocation (Object_Value,
                                       New_Object_Value_Access);
         function To_New_Object_Value_Access is new
           Ada.Unchecked_Conversion (Object_Value_Access,
                                     New_Object_Value_Access);

         X : New_Object_Value_Access :=
           To_New_Object_Value_Access (Object_Value_Access (Node));
         --  This is dangerous in the general case but here we know
         --  for sure that we have allocated all the nodes of the
         --  Object_Value type from the Object_Value_Access2 pool.
      begin
         Reclaim (X);
      end Free;

      -------------------------------------------------------------------------
      function  Get_Block  (ID : in Processes) return Object_Value_Access is
         Tmp : constant Object_Value_Access := Safe_Block (ID);
      begin
         if Tmp /= null then
            Safe_Block (ID) := null;
            return Tmp;
         else
            Primitives.Fetch_And_Add (Allocated'Access, 1);
            return
              Object_Value_Access (New_Object_Value_Access'(new Object_Value));
         end if;
      end Get_Block;

      -------------------------------------------------------------------------
      procedure Keep_Block (ID : in Processes;
                            B  : in Object_Value_Access) is
      begin
         Safe_Block (ID) := B;
      end Keep_Block;

   end Load_Linked_Store_Conditional;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line ("Large_Primitives.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Allocated = " &
                            Primitives.Standard_Unsigned'Image (Allocated));
   end Print_Statistics;


end NBAda.Large_Primitives;
