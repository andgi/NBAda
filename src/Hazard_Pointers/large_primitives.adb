-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : large_primitives.adb
--  Description     : Software Load-Linked and Store-Conditional for large
--                    words.
--                    From Maged Michael,
--                    "Practical Lock-Free and Wait-Free LL/SC/VL
--                     Implementations Using 64-Bit CAS".
--  Author          : Anders Gidenstam
--  Created On      : Thu Feb 24 10:25:44 2005
--  $Id: large_primitives.adb,v 1.1 2005/02/24 16:05:28 anders Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

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
         Target : HP.Node_access;
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

      --  Process local static data.
      Safe_Block : array (Processes) of Object_Value_Access;

      type Shared_Element_Access is access all Shared_Element;

      function To_Shared_Reference_Access is
         new Ada.Unchecked_Conversion (Shared_Element_Access,
                                       Shared_Reference_Access);

      function  Get_Block  (ID : in Processes) return Object_Value_Access;
      procedure Keep_Block (ID : in Processes;
                            B  : in Object_Value_Access);

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
            return new Object_Value;
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
