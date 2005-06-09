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
--  Filename        : large_primitives.adb
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    Based on the algorithm in Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 24 10:25:44 2005
--  $Id: large_primitives.adb,v 1.5 2005/06/09 14:53:17 anders Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;
--  with Lock_Free_Fixed_Size_Storage_Pools;

with Primitives;
with Ada.Text_IO;

package body Large_Primitives is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   subtype Processes is Process_Ids.Process_ID_Type;
   type    Exp_Index is new Integer range 1 .. Max_Number_Of_Links;

   type Shared_Reference_Access is access all HP.Shared_Reference;

   type Link is
      record
         Target : HP.Node_Access;
         Source : Shared_Reference_Access;
      end record;

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Process local static data.
   Exp  : array (Processes, Exp_Index) of Link;
   Next : array (Processes) of Exp_Index := (others => 1);

   --  Shared statistics.
   Allocated : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Allocated);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   package body Load_Linked_Store_Conditional is

      -------------------------------------------------------------------------
      --  Types and static variables.
      -------------------------------------------------------------------------
      type Shared_Element_Access is access all Shared_Element;

      function To_Shared_Reference_Access is
         new Ada.Unchecked_Conversion (Shared_Element_Access,
                                       Shared_Reference_Access);
      function  Get_Block  (ID : in Processes) return Object_Value_Access;
      procedure Keep_Block (ID : in Processes;
                            B  : in Object_Value_Access);

      --  Process local static data.
      Safe_Block : array (Processes) of Object_Value_Access;

      -------------------------------------------------------------------------
      --  Storage pool for the nodes.
      -------------------------------------------------------------------------
--        Node_Pool :
--          Lock_Free_Fixed_Size_Storage_Pools.Lock_Free_Storage_Pool
--          (Pool_Size  => 4_096,
--           Block_Size => Object_Value'Max_Size_In_Storage_Elements);

      type Object_Value_Access2 is access Object_Value;
--      for Object_Value_Access2'Storage_Pool use Node_Pool;


      -------------------------------------------------------------------------
      function Load_Linked (Target : access Shared_Element) return Element is
         ID : constant Processes := Process_Ids.Process_ID;
         use HP;
      begin
         Exp (ID, Next (ID)).Source :=
           To_Shared_Reference_Access (Shared_Element_Access (Target));
         Release (Exp (ID, Next (ID)).Target);
         Exp (ID, Next (ID)).Target := Dereference (Target);

         if Exp (ID, Next (ID)).Target.all in Object_Value then
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
      function Store_Conditional (Target : access Shared_Element;
                                  Value  : in     Element) return Boolean is
         ID : constant Processes := Process_Ids.Process_ID;
         use HP;
         Val : Object_Value_Access := Get_Block (ID);
      begin
         Val.Value := Value;

         for I in Exp'Range (2) loop
            if Exp (ID, I).Source =
              To_Shared_Reference_Access (Shared_Element_Access (Target))
            then
               declare
                  Old : constant Node_Access := Exp (ID, I).Target;
               begin
                  --  Clear this link.
                  Exp (ID, I).Source := null;
                  Exp (ID, I).Target := null;

                  if Compare_And_Swap (Shared    => Target,
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
      procedure Store_Conditional (Target : access Shared_Element;
                                   Value  : in     Element) is
         Tmp : Boolean;
      begin
         Tmp := Store_Conditional (Target, Value);
      end Store_Conditional;

      -------------------------------------------------------------------------
      function Verify_Link (Target : access Shared_Element) return Boolean is
         ID : constant Processes := Process_Ids.Process_ID;
         use HP;
      begin
         for I in Exp'Range (2) loop
            if Exp (ID, I).Source =
              To_Shared_Reference_Access (Shared_Element_Access (Target))
            then
               declare
                  Tmp : constant Node_Access := Dereference (Target);
               begin
                  Release (Tmp);
                  return Tmp = Exp (ID, I).Target;
               end;
            end if;
         end loop;
         return False;
      end Verify_Link;

      -------------------------------------------------------------------------
      procedure Initialize (Target : access Shared_Element;
                            Value  : in     Element) is
         ID  : constant Processes := Process_Ids.Process_ID;
         Val : constant Object_Value_Access := Get_Block (ID);
      begin
         Val.Value := Value;
         HP.Initialize
           (To_Shared_Reference_Access (Shared_Element_Access (Target)),
            HP.Node_Access (Val));
      end Initialize;

      -------------------------------------------------------------------------
      --  Private operations.
      -------------------------------------------------------------------------

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
              Object_Value_Access (Object_Value_Access2'(new Object_Value));
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
                            Primitives.Unsigned_32'Image (Allocated));
   end Print_Statistics;


end Large_Primitives;
