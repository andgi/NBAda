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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : atomic_multiwriter_snapshots.adb
--  Description     : M-component N-process snapshot implementation based on
--                    P. Jayanti, "An Optimal Multi-Writer Snapshot Algorithm",
--                    Proceedings of STOC'05, ACM, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Mon May 14 13:54:44 2007
--  $Id: nbada-atomic_multiwriter_snapshots.adb,v 1.2 2007/08/31 15:03:14 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Large_Primitives;
with Ada.Unchecked_Conversion;

package body NBAda.Atomic_Multiwriter_Snapshots is

   ----------------------------------------------------------------------------
   type Component_State is (Free, Reserved, Active);
   for Component_State'Size use Primitives.Standard_Unsigned'Size;

   type Component_Record is
      record
         State : aliased Component_State;
         pragma Atomic (State);
         Value : aliased Component_Impl;
         pragma Atomic (Value);
      end record;

   type B_Component_Record is
      record
         Valid : Boolean;
         Value : Component_Impl;
      end record;

   type Phase_Id is range 1 .. 3;

   type Phase_Record is
      record
         Phase  : Phase_Id;
         Proc_A : Process_Ids.Process_ID_Type;
         Proc_B : Process_Ids.Process_ID_Type;
         Toggle : Boolean;
      end record;

   package LL_SC_VL_Base is new
     Large_Primitives (Max_Number_Of_Links => 2,
                       Process_Ids         => Process_Ids);
   package LL_SC_VL_Base2 is new
     Large_Primitives (Max_Number_Of_Links => 2,
                       Process_Ids         => Process_Ids);

   package Phase_LL_SC_VL is new
     LL_SC_VL_Base2.Load_Linked_Store_Conditional (Phase_Record);
   --  NOTE: Load links for Phase records need to be long lived.

   subtype Shared_Phase_Record is Phase_LL_SC_VL.Shared_Element;

   package B_Component_LL_SC_VL is new
     LL_SC_VL_Base.Load_Linked_Store_Conditional (B_Component_Record);

   subtype Shared_B_Component is B_Component_LL_SC_VL.Shared_Element;

   type B_Component_Array is
     array (Component_Index) of aliased Shared_B_Component;
   --  pragma Atomic_Components (B_Component_Array);

   type Process_Record is
      record
         A : Component_Array;
         B : B_Component_Array;
      end record;

   type Snapshot_Record is
      record
         Toggle   : Boolean := False;
         Snapshot : Component_Array;
      end record;
   package Snapshot_LL_SC_VL is new
     LL_SC_VL_Base.Load_Linked_Store_Conditional (Snapshot_Record);

   subtype Shared_Snapshot_Record is Snapshot_LL_SC_VL.Shared_Element;


   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap (Component_State);

   ----------------------------------------------------------------------------
   procedure Transfer (I : in Component_Index;
                       P : in Process_Ids.Process_ID_Type);
   procedure Push_LS  (P : in Process_Ids.Process_ID_Type);

   ----------------------------------------------------------------------------
   --  Shared data.

   X       : aliased Shared_Phase_Record;
   --  pragma Atomic (X);
   A       : array (Component_Index) of Component_Record :=
     (others => (Free, 0));
   --  pragma Volatile_Components (A);
   Process : array (Process_Ids.Process_ID_Type) of Process_Record;
   --  pragma Volatile_Components (Process);
   SS      : aliased Shared_Snapshot_Record;
   --  pragma Atomic (SS);

   ----------------------------------------------------------------------------
   function Scan return Snapshot is
      ID : constant Process_Ids.Process_ID_Type := Process_Ids.Process_ID;
   begin
      Push_LS (ID);
      Push_LS (ID);
      declare
         use Snapshot_LL_SC_VL;
         Current : constant Snapshot_Record := Load_Linked (SS'Access);
      begin
         return Snapshot (Current.Snapshot);
      end;
   end Scan;

   ----------------------------------------------------------------------------
   package body Element_Components is

      ----------------------------------------------------------------------
      function To_Component is
         new Ada.Unchecked_Conversion (Element, Component_Impl);
      function To_Element is
         new Ada.Unchecked_Conversion (Component_Impl, Element);

      ----------------------------------------------------------------------
      function Create (Default_Value : in Element) return Element_Component is
      begin
         for C in A'Range loop
            if A (C).State = Free then
               if Compare_And_Swap (A (C).State'Access,
                                    Old_Value => Free,
                                    New_Value => Reserved)
               then
                  A (C).Value := To_Component (Default_Value);
                  Primitives.Membar;
                  A (C).State := Active;
                  return Element_Component'(Index => Natural (C));
               end if;
            end if;
         end loop;

         raise Maximum_Number_Of_Components_Exceeded;
      end Create;

      ----------------------------------------------------------------------
      procedure Write (To    : in Element_Component;
                       Value : in Element) is
         Component : constant Component_Index := Component_Index (To.Index);
      begin
         if A (Component).State = Active then
            A (Component).Value := To_Component (Value);

            declare
               use Phase_LL_SC_VL;
               Status : constant Phase_Record := Load_Linked (X'Access);
            begin
               if Status.Phase = 2 then
                  Transfer (Component, Status.Proc_B);
                  Transfer (Component, Status.Proc_B);
               end if;
            end;
         else
            raise Constraint_Error;
         end if;
      end Write;

      ----------------------------------------------------------------------
      function Read (Component : in Element_Component;
                     From      : in Snapshot) return Element is
      begin
         return To_Element (From (Component_Index (Component.Index)));
      end Read;

   end Element_Components;
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Transfer (I : in Component_Index;
                       P : in Process_Ids.Process_ID_Type) is
      use B_Component_LL_SC_VL;
      use Phase_LL_SC_VL;
   begin
      declare
         B_I : B_Component_Record :=
           Load_Linked (Process (P).B (I)'Access);
      begin
         B_I.Valid := True;
         B_I.Value := A (I).Value;
         if Verify_Link (X'Access) then
            Store_Conditional (Process (P).B (I)'Access, B_I);
         end if;
      end;
   end Transfer;

   ----------------------------------------------------------------------------
   procedure Push_LS  (P : in Process_Ids.Process_ID_Type) is
      use Phase_LL_SC_VL;
      use Snapshot_LL_SC_VL;
      use B_Component_LL_SC_VL;
      Status : Phase_Record := Load_Linked (X'Access);
   begin
      if Status.Phase = 1 then
         for I in Component_Index loop
            if A (I).State = Active then
               loop
                  declare
                     B_I : B_Component_Record :=
                       Load_Linked (Process (P).B (I)'Access);
                  begin
                     B_I.Valid := False;
                     exit when
                       Store_Conditional (Process (P).B (I)'Access, B_I);
                  end;
               end loop;
            end if;
         end loop;
         Status.Phase  := 3;
         Status.Proc_B := P;
         Store_Conditional (X'Access, Status);
         Status        := Load_Linked (X'Access);
      end if;

      if Status.Phase = 2 then
         for I in Component_Index loop
            if A (I).State = Active then
               Process (P).A (I) := A (I).Value;
            end if;
         end loop;
         Status.Phase  := 3;
         Status.Proc_A := P;
         Store_Conditional (X'Access, Status);
         Status        := Load_Linked (X'Access);
      end if;

      if Status.Phase = 3 then
         declare
            V_P : Snapshot_Record;
         begin
            V_P.Toggle := Status.Toggle;

            for I in Component_Index loop
               if A (I).State = Active then
                  V_P.Snapshot (I) := Process (Status.Proc_A).A (I);
               end if;
            end loop;

            for I in Component_Index loop
               if A (I).State = Active then
                  declare
                     B : constant B_Component_Record :=
                       Load_Linked (Process (Status.Proc_B).B (I)'Access);
                  begin
                     if B.Valid then
                        V_P.Snapshot (I) := B.Value;
                     end if;
                  end;
               end if;
            end loop;

            declare
               Current : constant Snapshot_Record := Load_Linked (SS'Access);
            begin
               if Current.Toggle /= Status.Toggle and then
                 Verify_Link (X'Access)
               then
                  Store_Conditional (SS'Access, V_P);
               end if;
            end;
         end;

         Status.Phase  := 1;
         Status.Toggle := not Status.Toggle;
         Store_Conditional (X'Access, Status);
      end if;
   end Push_LS;

begin
   declare
      V : Phase_Record;
   begin
      V.Phase  := 1;
      V.Toggle := True;
      V.Proc_A := 1;
      V.Proc_B := 1;
      Phase_LL_SC_VL.Initialize (X'Access, V);
   end;
   declare
      V : Snapshot_Record;
   begin
      V.Toggle := True;
      Snapshot_LL_SC_VL.Initialize (SS'Access, V);
   end;
   for P in Process'Range loop
      for I in Process (P).B'Range loop
         B_Component_LL_SC_VL.Initialize (Process (P).B (I)'Access,
                                          (False, 0));
      end loop;
   end loop;
end NBAda.Atomic_Multiwriter_Snapshots;
