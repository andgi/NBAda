-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
--  Copyright (C) 2004 - 2006  Anders Gidenstam
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
--  Filename        : lock_free_memory_reclamation.adb
--  Description     : Ada implementation of the lock-free garbage reclamation
--                    Scheme from "Efficient and Reliable Lock-Free Memory
--                    Reclamation Based on Reference Counting",
--                    Anders Gidenstam, Marina Papatriantafilou,
--                    Håkan Sundell and Philippas Tsigas,
--                    Proceedings of the 8th International Symposium on
--                    Parallel Architectures, Algorithms and Networks (I-SPAN),
--                    pages 202 - 207, IEEE Computer Society, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Fri Nov 19 14:07:58 2004
--  $Id: nbada-lock_free_memory_reclamation.adb,v 1.24 2006/11/30 21:15:23 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Hash_Tables;

--  with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;

with Ada.Text_IO;

package body Lock_Free_Memory_Reclamation is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------
   subtype Processes  is Process_Ids.Process_ID_Type;
   type    HP_Index   is new Natural range 1 .. Max_Number_Of_Dereferences;
   type    Node_Index is new Natural range 0 .. Max_Delete_List_Size;
   subtype Valid_Node_Index is
     Node_Index range 1 .. Node_Index (Max_Delete_List_Size);

   subtype Atomic_Node_Access is Managed_Node_Access;

   subtype Node_Count  is Natural;
   subtype Claim_Count is Primitives.Unsigned_32;


   procedure Scan           (ID : in Processes);
   procedure Clean_Up_Local (ID : in Processes);
   procedure Clean_Up_All   (ID : in Processes);

   function Hash_Ref (Ref  : in Managed_Node_Access;
                      Size : in Natural) return Natural;

   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add;

   package HP_Sets is
      new Hash_Tables (Managed_Node_Access, "=", Hash_Ref);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Persistent shared variables.
   Hazard_Pointer : array (Processes, HP_Index) of aliased Atomic_Node_Access;
   pragma Volatile (Hazard_Pointer);
   pragma Atomic_Components (Hazard_Pointer);

   DL_Nodes  : array (Processes, Valid_Node_Index) of
     aliased Atomic_Node_Access;
   pragma Volatile (DL_Nodes);
   pragma Atomic_Components (DL_Nodes);

   DL_Claims : array (Processes, Valid_Node_Index) of aliased Claim_Count
     := (others => (others => 0));
   pragma Volatile (DL_Claims);
   pragma Atomic_Components (DL_Claims);

   DL_Done : array (Processes, Valid_Node_Index) of aliased Boolean :=
     (others => (others => False));
   pragma Volatile (DL_Done);
   pragma Atomic_Components (DL_Done);

   --  Persistent process local variables.
   type DL_Nexts_Array is array (Valid_Node_Index) of Node_Index;
   type Persistent_Local is
     record
        D_List   : Node_Index := 0;
        D_Count  : Node_Count := 0;
        DL_Nexts : DL_Nexts_Array := (others => 0);
     end record;

   Persistent_Local_Variables : array (Processes) of Persistent_Local;

--     D_List   : array (Processes) of Node_Index :=
--       (others => 0);
--     pragma Atomic_Components (D_List);
--     D_Count  : array (Processes) of Node_Count :=
--       (others => 0);
--     pragma Atomic_Components (D_Count);
--     DL_Nexts : array (Processes, Valid_Node_Index) of Node_Index :=
--       (others => (others => 0));
--     pragma Atomic_Components (DL_Nexts);

   No_Nodes_Created   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Created);
   No_Nodes_Reclaimed : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Reclaimed);


   --  The P_Sets are preallocated from the heap as it can easily become
   --  too large to fit on the task stack.
   type HP_Set_Access is access HP_Sets.Hash_Table;
   P_Set : constant array (Processes) of HP_Set_Access :=
     (others => new HP_Sets.Hash_Table
      (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
                            Max_Number_Of_Dereferences) + 1));

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Is_Deleted (Node : access Managed_Node_Base)
                       return Boolean is
   begin
      return Node.MM_Del;
   end Is_Deleted;

   ----------------------------------------------------------------------------
   package body Operations is

      ----------------------------------------------------------------------
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Shared_Reference,
                                       Private_Reference_Impl);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Private_Reference_Impl);

      function To_Node_Access (X : Private_Reference)
                              return Node_Access;
      pragma Inline (To_Node_Access);

      type Shared_Reference_Base_Access is access all Shared_Reference_Base;
      type Shared_Reference_Access is access all Shared_Reference;
      function To_Shared_Reference_Base_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Shared_Reference_Base_Access);

      function Compare_And_Swap_32 is
         new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference_Base);

      Mark_Mask  : constant Private_Reference_Impl := 2 ** Mark_Bits - 1;
      Ref_Mask   : constant Private_Reference_Impl := -(2 ** Mark_Bits);

      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference is
         ID       : constant Processes := Process_Ids.Process_ID;
         Index    : HP_Index;
         Found    : Boolean := False;
         Node_Ref : Private_Reference;
      begin
         --  Find a free hazard pointer.
         for I in Hazard_Pointer'Range (2) loop
            if Hazard_Pointer (ID, I) = null then
               Index       := I;
               Node_Ref.HP := Natural (I);
               Found       := True;
               exit;
            end if;
         end loop;
         --  Dereference node iff there is a free hazard pointer.
         if not Found then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "lock_free_memory_reclamation.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            loop
               Node_Ref.Ref := To_Private_Reference (Link.all);
               Hazard_Pointer (ID, Index) :=
                 Atomic_Node_Access (To_Node_Access (Node_Ref));

               Primitives.Membar;
               --  The write to the hazard pointer must be visible before
               --  Link is read again.
               exit when To_Private_Reference (Link.all) = Node_Ref.Ref;
            end loop;
         end if;

         return Node_Ref;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
         ID : constant Processes := Process_Ids.Process_ID;
      begin
         --  Find and clear hazard pointer.
         Primitives.Membar;
         Hazard_Pointer (ID, HP_Index (Node.HP)) := null;
         --  If we have dereferenced the same node several time, there are
         --  several hazard pointers to it. Release removes one.
--           for I in Hazard_Pointer'Range (2) loop
--              if
--                Hazard_Pointer (ID, I) =
--                Atomic_Node_Access (To_Node_Access (Node))
--              then
--                 Hazard_Pointer (ID, I) := null;
--                 exit;
--              end if;
--           end loop;
      end Release;

      ----------------------------------------------------------------------
      function  "+"     (Node : in Private_Reference)
                        return Node_Access renames To_Node_Access;

      ----------------------------------------------------------------------
      function Deref (Node : in Private_Reference)
                     return Node_Access renames To_Node_Access;

      ----------------------------------------------------------------------
      procedure Delete  (Node : in Private_Reference) is
         use type Node_Count;
         ID        : constant Processes := Process_Ids.Process_ID;
         PL        : Persistent_Local renames Persistent_Local_Variables (ID);
         Index     : Node_Index;
      begin
         Release (Node);
         declare
            Node_Base : constant Managed_Node_Access :=
              Managed_Node_Access (To_Node_Access (Node));
            --  Base type view of the node.
         begin
            Node_Base.MM_Del   := True;
            Node_Base.MM_Trace := False;
         end;

         --  Find a free index in DL_Nodes.
         --  This is probably not the best search strategy.
         for I in DL_Nodes'Range (2) loop
            if DL_Nodes (ID, I) = null then
               Index := I;
            end if;
         end loop;

         DL_Done  (ID, Index) := False;
         DL_Nodes (ID, Index) := Atomic_Node_Access (To_Node_Access (Node));
         PL.DL_Nexts (Index)  := PL.D_List;
         PL.D_List  := Index;
         PL.D_Count := PL.D_Count + 1;

         loop
            if PL.D_Count >= Clean_Up_Threshold then
               Clean_Up_Local (ID);
            end if;
            if PL.D_Count >= Scan_Threshold then
               Scan (ID);
            end if;
            if PL.D_Count >= Clean_Up_Threshold then
               Clean_Up_All (ID);
            end if;

            exit when PL.D_Count < Max_Delete_List_Size;
         end loop;
      end Delete;

      ----------------------------------------------------------------------
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean is
         use type Reference_Count;
      begin
         if
           Compare_And_Swap_32
           (Target    =>
              To_Shared_Reference_Base_Access (Link.all'Unchecked_Access),
            Old_Value => (Ref => Shared_Reference_Base_Impl (Old_Value.Ref)),
            New_Value => (Ref => Shared_Reference_Base_Impl (New_Value.Ref)))
         then
            if To_Node_Access (New_Value) /= null then
               declare
                  New_Value_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (New_Value));
                  --  Base type view of the node.
               begin
                  Fetch_And_Add (New_Value_Base.MM_RC'Access, 1);
                  New_Value_Base.MM_Trace := False;
               end;
            end if;

            if To_Node_Access (Old_Value) /= null then
               declare
                  Old_Value_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (Old_Value));
                  --  Base type view of the node.
               begin
                  Fetch_And_Add (Old_Value_Base.MM_RC'Access, -1);
               end;
            end if;

            return True;
         end if;

         return False;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Private_Reference;
                                  New_Value : in     Private_Reference) is
         use type Reference_Count;
      begin
         if
           Compare_And_Swap (Link,
                             Old_Value,
                             New_Value)
         then
            null;
         end if;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
         use type Reference_Count;

         Old : constant Node_Access :=
           To_Node_Access ((To_Private_Reference (Link.all), 0));
      begin
         To_Shared_Reference_Base_Access (Link.all'Unchecked_Access).all :=
           (Ref => Shared_Reference_Base_Impl (Node.Ref));

         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
               --  Base type view of the node.
            begin
               Fetch_And_Add (Node_Base.MM_RC'Access, 1);
               Node_Base.MM_Trace := False;
            end;
         end if;

         if Old /= null then
            declare
               Old_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Old);
               --  Base type view of the node.
            begin
               Fetch_And_Add (Old_Base.MM_RC'Access, -1);
            end;
         end if;
      end Store;

      ----------------------------------------------------------------------
      function Create return Private_Reference is
         ID    : constant Processes        := Process_Ids.Process_ID;
         UNode : constant User_Node_Access := new Managed_Node;
         Node  : constant Node_Access      := UNode.all'Unchecked_Access;
         Index : HP_Index;
         Found : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in Hazard_Pointer'Range (2) loop
            if Hazard_Pointer (ID, I) = null then
               Index := I;
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "lock_free_memory_reclamation.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            Hazard_Pointer (ID, Index) := Atomic_Node_Access (Node);
         end if;

         if Debug then
            Fetch_And_Add (No_Nodes_Created'Access, 1);
         end if;

         return (To_Private_Reference (Node), Natural (Index));
      end Create;

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference) is
      begin
         Node.Ref := Node.Ref or 1;
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return (Node.Ref or 1, Node.HP);
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node.Ref := Node.Ref and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return (Node.Ref and Ref_Mask, Node.HP);
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node.Ref and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean is
      begin
         return (To_Private_Reference (Node) and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function "=" (Left  : in     Private_Reference;
                    Right : in     Private_Reference) return Boolean is
      begin
         return Left.Ref = Right.Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref.Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref.Ref;
      end "=";

      ----------------------------------------------------------------------
      function To_Node_Access (X : Private_Reference)
                              return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Private_Reference_Impl,
                                          Node_Access);

      begin
         return To_Node_Access (X.Ref and Ref_Mask);
      end To_Node_Access;

      ----------------------------------------------------------------------
   end Operations;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line ("Lock_Free_Memory_Reclamation.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Created = " &
                            Primitives.Unsigned_32'Image (No_Nodes_Created));
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Unsigned_32'Image (No_Nodes_Reclaimed));

   end Print_Statistics;

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan (ID : in Processes) is
      use type Reference_Count;
      use HP_Sets;

--        type HP_Set_Access is access HP_Sets.Hash_Table;
--        procedure Free is new Ada.Unchecked_Deallocation (HP_Sets.Hash_Table,
--                                                          HP_Set_Access);
--        P_Set : HP_Set_Access :=
--          new HP_Sets.Hash_Table
--          (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
--                                Max_Number_Of_Dereferences) + 1);
--        --  The P_Set is allocated from the heap as it can easily become
--        --  too large to fit on the task stack.
      PL          : Persistent_Local renames Persistent_Local_Variables (ID);
      Index       : Node_Index;
      Node        : Atomic_Node_Access;
      New_D_List  : Node_Index := 0;
      New_D_Count : Node_Count := 0;
   begin
      --  Set the trace bit on each deleted node with MM_RC = 0.
      Index := PL.D_List;
      while Index /= 0 loop
         Node := DL_Nodes (ID, Index);
         if Node.MM_RC = 0 then
            Node.MM_Trace := True;
            if Node.MM_RC /= 0 then
               Node.MM_Trace := False;
            end if;
         end if;
         Index := PL.DL_Nexts (Index);
      end loop;

      Clear (P_Set (ID).all);

      --  Read all hazard pointers.
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Node := Hazard_Pointer (P, I);
            if Node /= null then
               declare
                  N : constant Managed_Node_Access :=
                    Managed_Node_Access (Node);
               begin
                  Insert (N, P_Set (ID).all);
               end;
            end if;
         end loop;
      end loop;

      --  Attempt to reclaim nodes.
      while PL.D_List /= 0 loop
         Index       := PL.D_List;
         Node        := DL_Nodes (ID, Index);
         PL.D_List   := PL.DL_Nexts (Index);

         if Node.MM_RC = 0 and Node.MM_Trace and
           not Member (Managed_Node_Access (Node), P_Set (ID).all)
         then
            DL_Nodes (ID, Index) := null;
            if DL_Claims (ID, Index) = 0 then
               Dispose (Managed_Node_Access (Node),
                        Concurrent => False);
               Free (Managed_Node_Access (Node));

               if Debug then
                  Fetch_And_Add (No_Nodes_Reclaimed'Access, 1);
               end if;
            else
               Dispose (Managed_Node_Access (Node),
                        Concurrent => True);
               DL_Done  (ID, Index) := True;
               DL_Nodes (ID, Index) := Node;

               --  Keep Node in D_List.
               PL.DL_Nexts (Index) := New_D_List;
               New_D_List   := Index;
               New_D_Count  := New_D_Count + 1;
            end if;
         else
            --  Keep Node in D_List.
            PL.DL_Nexts (Index) := New_D_List;
            New_D_List   := Index;
            New_D_Count  := New_D_Count + 1;
         end if;
      end loop;

      PL.D_List  := New_D_List;
      PL.D_Count := New_D_Count;

--        Free (P_Set);
   end Scan;

   ----------------------------------------------------------------------------
   procedure Clean_Up_Local (ID : in Processes) is
      PL    : Persistent_Local renames Persistent_Local_Variables (ID);
      Index : Node_Index := PL.D_List;
      Node  : Atomic_Node_Access;
   begin
      while Index /= 0 loop
         Node  := DL_Nodes (ID, Index);
         Clean_Up (Managed_Node_Access (Node));
         Index := PL.DL_Nexts (Index);
      end loop;
   end Clean_Up_Local;

   ----------------------------------------------------------------------------
   procedure Clean_Up_All (ID : in Processes) is
      use type Primitives.Unsigned_32;
      Node  : Atomic_Node_Access;
   begin
      for P in Processes loop
         for Index in Valid_Node_Index loop
            Node := DL_Nodes (P, Index);
            if Node /= null and then not DL_Done (P, Index) then
               Fetch_And_Add (Target    => DL_Claims (P, Index)'Access,
                              Increment => 1);
               if Node = DL_Nodes (P, Index) then
                  Clean_Up (Managed_Node_Access (Node));
               end if;
               Fetch_And_Add (Target    => DL_Claims (P, Index)'Access,
                              Increment => -1);
            end if;
         end loop;
      end loop;
   end Clean_Up_All;

   ----------------------------------------------------------------------------
   function Hash_Ref (Ref  : in Managed_Node_Access;
                      Size : in Natural) return Natural is
      type Unsigned is mod 2**32;
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Managed_Node_Access,
                                       Unsigned);
   begin
      return Natural ((To_Unsigned (Ref) / 4) mod Unsigned (Size));
   end Hash_Ref;

end Lock_Free_Memory_Reclamation;
