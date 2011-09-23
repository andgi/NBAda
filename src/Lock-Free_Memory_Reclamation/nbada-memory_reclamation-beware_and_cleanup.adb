-------------------------------------------------------------------------------
--  Lock-Free Memory Reclamation - An implementation of the lock-free
--  garbage reclamation scheme by A. Gidenstam, M. Papatriantafilou, H. Sundell
--  and P. Tsigas.
--
--  Copyright (C) 2004 - 2011  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-beware_and_cleanup.adb
--  Description     : Ada implementation of the lock-free garbage reclamation
--                    Scheme from "Efficient and Reliable Lock-Free Memory
--                    Reclamation Based on Reference Counting",
--                    Anders Gidenstam, Marina Papatriantafilou,
--                    Håkan Sundell and Philippas Tsigas,
--                    Proceedings of the 8th International Symposium on
--                    Parallel Architectures, Algorithms and Networks (I-SPAN),
--                    pages 202 - 207, IEEE Computer Society, 2005.
--  Author          : Anders Gidenstam
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Internals.Cleanup_Tools;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;

with Ada.Text_IO;

package body NBAda.Memory_Reclamation.Beware_And_Cleanup is

   ----------------------------------------------------------------------------
   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add_32;

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
   package body Reference_Operations is

      ----------------------------------------------------------------------
      function To_Reference_Impl is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Reference_Impl);
      function To_Reference_Impl is
         new Ada.Unchecked_Conversion
        (Basic_Reference_Operations.Unsafe_Reference_Value,
         Reference_Impl);

      type Shared_Reference_Access is access all Shared_Reference;
      type Reference_Impl_Access is access all
        Reference_Impl;
      function To_Reference_Impl_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Reference_Impl_Access);

      procedure Scan           (MM : in Memory_Manager);
      procedure Clean_Up_Local (MM : in Memory_Manager);
      procedure Clean_Up_All   (MM : in Memory_Manager);

      procedure Validate (Node  : in Private_Reference;
                          Where : in String);

      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl;
      pragma Inline (Get_Ref);
      pragma Inline_Always (Get_Ref);
      function Create_Private_Reference (Ref  : in Reference_Impl;
                                         HP   : in Index;
                                         MM   : in Memory_Manager_Access)
                                        return Private_Reference;

      function  Deref   (Ref : in Reference_Impl)
                        return Node_Access;
      pragma Inline (Deref);
      pragma Inline_Always (Deref);
      function  Deref   (Node : in Private_Reference)
                        return Node_Access;
      pragma Inline (Deref);
      pragma Inline_Always (Deref);
      function To_Unsafe_Reference (Node : in Private_Reference)
        return Basic_Reference_Operations.Unsafe_Reference_Value;
      pragma Inline (To_Unsafe_Reference);
      pragma Inline_Always (To_Unsafe_Reference);

      ----------------------------------------------------------------------
      function Null_Reference return Private_Reference is
      begin
         return (Basic_Reference_Operations.Private_Reference_Base
                 with HP => 0, MM => null);
      end Null_Reference;

      ----------------------------------------------------------------------
      function Image (R : Private_Reference) return String is
         package BRO renames Basic_Reference_Operations;
         type Node_Access is access all Managed_Node_Base'Class;
      begin
         if Deref (R) /= null then
            return
              "(" &
              Ada.Tags.External_Tag (Node_Access (Deref (R)).all'Tag) & "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) &
              ", HP[" &
              Integer'Image (R.HP) & "])";

         else
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) &
              ", HP[" &
              Integer'Image (R.HP) & "])";
         end if;
      exception
         when Storage_Error =>
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) &
              ", HP[" &
              Integer'Image (R.HP) & "])";
      end Image;

      ----------------------------------------------------------------------
      function  Dereference (MM   : in     Memory_Manager'Class;
                             Link : access Shared_Reference)
                            return Private_Reference is
         Shared   : Task_Shared renames TSS.Get (MM.Shared).all;
         Local    : Task_Local  renames TLS.Get (MM.Local).all;
         Index    : HP_Index;
         Node_Ref : Reference_Impl;
         Found    : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in Shared.Hazard_Pointer'Range loop
            if Shared.Hazard_Pointer (I) = null then
               Index := I;
               Found := True;
               exit;
            end if;
         end loop;
         --  Dereference node iff there is a free hazard pointer.
         if not Found then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-beware_and_cleanup.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            if Integrity_Checking then
               declare
                  use type Primitives.Unsigned_32;
               begin
                  loop
                     Node_Ref := Shared_Reference_Base (Link.all).Ref;
                     Shared.Hazard_Pointer (Index) :=
                       Atomic_Node_Access (Deref (Node_Ref));

                     Primitives.Membar;
                     --  The write to the hazard pointer must be visible
                     --  before Link is read again.
                     exit when Node_Ref = Shared_Reference_Base (Link.all).Ref;
                  end loop;
                  if Deref (Node_Ref) /= null then
                     declare
                        State : constant MM_Magic_Type :=
                          Deref (Node_Ref).MM_Magic;
                     begin
                        if not (State = MM_Live or State = MM_Deleted) then
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "nbada-memory_reclamation-" &
                              "beware_and_cleanup.adb: " &
                              "Dereferenced a node with the bad MM_Magic " &
                              MM_Magic_Type'Image (State) &
                              "! " & Reference_Impl'Image (Node_Ref));
                        end if;
                     end;
                  end if;
               end;
            else
               loop
                  Node_Ref := Shared_Reference_Base (Link.all).Ref;
                  Shared.Hazard_Pointer (Index) :=
                    Atomic_Node_Access (Deref (Node_Ref));

                  Primitives.Membar;
                  --  The write to the hazard pointer must be visible
                  --  before Link is read again.
                  exit when Shared_Reference_Base (Link.all).Ref = Node_Ref;
               end loop;
            end if;
         end if;

         return Create_Private_Reference
           (Ref => Node_Ref,
            HP  => Index,
            MM  => MM.Mutable.Self.all'Unchecked_Access);
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
      begin
         Primitives.Membar;
         --  Complete all preceding memory operations before releasing
         --  the hazard pointer.
         if Deref (Node) /= null then
            declare
               Shared : Task_Shared renames TSS.Get (Node.MM.Shared).all;
            begin
               if Integrity_Checking then
                  Validate (Node, "Attempting to release");
               end if;
               Shared.Hazard_Pointer (Node.HP) := null;
            end;
         end if;
      end Release;

      ----------------------------------------------------------------------
      function  "+"     (Node : in Private_Reference)
                        return Node_Access is
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to use");
         end if;
         return Deref (Node);
      end "+";

      ----------------------------------------------------------------------
      function  Copy (Node : in Private_Reference)
                     return Private_Reference is
      begin
         if Deref (Node) /= null then
            declare
               Shared : Task_Shared renames TSS.Get (Node.MM.Shared).all;
               Local  : Task_Local  renames TLS.Get (Node.MM.Local).all;
               Index  : HP_Index;
               Found  : Boolean := False;
            begin
               if Integrity_Checking then
                  Validate (Node, "Attempting to copy");
               end if;
               --  Find a free hazard pointer.
               for I in Shared.Hazard_Pointer'Range loop
                  if Shared.Hazard_Pointer (I) = null then
                     Index   := I;
                     Found   := True;
                     exit;
                  end if;
               end loop;
               --  Copy the reference iff there is a free hazard pointer.
               if not Found then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-" &
                       "beware_and_cleanup.adb: " &
                       "Maximum number of local dereferences exceeded!");
               else
                  Shared.Hazard_Pointer (Index) :=
                    Atomic_Node_Access (Deref (Node));

                  Primitives.Membar;
                  --  Make sure the hazard pointer write is committed before
                  --  subsequent memory operations.
               end if;

               return Create_Private_Reference
                 (Ref => Get_Ref (Node),
                  HP  => Index,
                  MM  => Node.MM.Mutable.Self.all'Unchecked_Access);
            end;
         else
            return Null_Reference;
         end if;
      end Copy;

      ----------------------------------------------------------------------
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean is
         package BRO renames Basic_Reference_Operations;
         use type Reference_Count;
      begin
         if Integrity_Checking then
            Validate (Old_Value, "Attempting a CAS where Old_Value is");
            Validate (New_Value, "Attempting a CAS where New_Value is");
         end if;
         if
           Boolean_Compare_And_Swap_Impl
           (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
            Old_Value => Get_Ref (Old_Value),
            New_Value => Get_Ref (New_Value))
         then
            if Deref (New_Value) /= Deref (Old_Value) then
               if Deref (New_Value) /= null then
                  declare
                     New_Value_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (Deref (New_Value));
                     --  Base type view of the node.
                  begin
                     Fetch_And_Add (New_Value_Base.MM_RC'Access, 1);
                     New_Value_Base.MM_Trace := False;
                  end;
               end if;
               if Deref (Old_Value) /= null then
                  declare
                     Old_Value_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (Deref (Old_Value));
                     --  Base type view of the node.
                  begin
                     Fetch_And_Add (Old_Value_Base.MM_RC'Access, -1);
                  end;
               end if;
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
      procedure Delete  (Node : in Private_Reference) is
      begin
         if Deref (Node) = null then
            return;
         end if;
         declare
            use type Node_Count;
            Shared : Task_Shared renames TSS.Get (Node.MM.Shared).all;
            Local  : Task_Local  renames TLS.Get (Node.MM.Local).all;
            Index  : Node_Index;
         begin
            if Integrity_Checking then
               Validate (Node, "Attempting to delete");
            end if;

            Release (Node);
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Deref (Node));
               --  Base type view of the node.
            begin
               Node_Base.MM_Del   := True;
               Node_Base.MM_Trace := False;
            end;

            --  Find a free index in DL_Nodes.
            --  This is probably not the best search strategy.
            for I in Shared.DL_Nodes'Range loop
               if Shared.DL_Nodes (I) = null then
                  Index := I;
               end if;
            end loop;

            Shared.DL_Done  (Index) := False;
            Shared.DL_Nodes (Index) := Atomic_Node_Access (Deref (Node));
            Local.DL_Nexts (Index)  := Local.D_List;
            Local.D_List            := Index;
            Local.D_Count           := Local.D_Count + 1;

            loop
               if Local.D_Count >= Natural'Min (Clean_Up_Threshold,
                                                Max_Delete_List_Size) then
                  Clean_Up_Local (Node.MM.all);
               end if;
               if Local.D_Count >= Natural'Min (Scan_Threshold,
                                                Max_Delete_List_Size) then
                  Scan (Node.MM.all);
               end if;
               if Local.D_Count >= Natural'Min (Clean_Up_Threshold,
                                                Max_Delete_List_Size) then
                  Clean_Up_All (Node.MM.all);
               end if;

               exit when Local.D_Count < Max_Delete_List_Size;
            end loop;
         end;
      end Delete;

      ----------------------------------------------------------------------
      procedure Rescan  (Node : in Private_Reference) is
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to rescan");
         end if;
      end Rescan;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
         use type Reference_Count;
         Old : constant Node_Access :=
           Deref (Shared_Reference_Base (Link.all).Ref);
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to store");
         end if;
         Link.all := To_Shared_Reference (Node);

         if Deref (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Deref (Node));
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
      function Create (MM : in Memory_Manager) return Private_Reference is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
         UNode  : constant User_Node_Access := new Managed_Node;
         Node   : constant Node_Access      := UNode.all'Unchecked_Access;
         Index  : HP_Index;
         Found  : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in Shared.Hazard_Pointer'Range loop
            if Shared.Hazard_Pointer (I) = null then
               Index := I;
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-beware_and_cleanup.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            Shared.Hazard_Pointer (Index) := Atomic_Node_Access (Node);
         end if;

         if Collect_Statistics then
            Fetch_And_Add (MM.Mutable.Self.Nodes_Created'Access, 1);
         end if;

         return Create_Private_Reference
           (Ref => To_Reference_Impl (Node),
            HP  => Index,
            MM  => MM.Mutable.Self.all'Unchecked_Access);
      end Create;

      ----------------------------------------------------------------------
      function "=" (Left, Right : in Private_Reference) return Boolean is
      begin
         return Basic_Reference_Operations."=" (Left, Right);
      end "=";

      ----------------------------------------------------------------------
      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in Private_Reference)
        return Boolean is
         package BRO renames Basic_Reference_Operations;
      begin
         if Integrity_Checking then
            Validate (New_Value, "Attempting a CAS where New_Value is");
         end if;
         --  Since we have not dereferenced Old_Value it is not
         --  guaranteed to have a positive reference count.
         --  However, since we just successfully removed a link to that
         --  node it's reference count certainly should not be zero.
         return Compare_And_Swap (Link      => Link,
                                  Old_Value => Old_Value,
                                  New_Value =>
                                    To_Unsafe_Reference (New_Value));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Private_Reference;
         New_Value : in Basic_Reference_Operations.Unsafe_Reference_Value)
        return Boolean is
      begin
         if Integrity_Checking then
            Validate (Old_Value, "Attempting a CAS where Old_Value is");
         end if;
         --  Since we have not dereferenced New_Value it is not
         --  guaranteed to have a positive reference count.
         return Compare_And_Swap (Link      => Link,
                                  Old_Value =>
                                    To_Unsafe_Reference (Old_Value),
                                  New_Value => New_Value);
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      function  Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in Basic_Reference_Operations.Unsafe_Reference_Value)
        return Boolean is
      begin
         if
           Boolean_Compare_And_Swap_Impl
           (Target    => To_Reference_Impl_Access (Link.all'Unchecked_Access),
            Old_Value => To_Reference_Impl (Old_Value),
            New_Value => To_Reference_Impl (New_Value))
         then
            declare
               New_Node : constant Node_Access :=
                 Deref (To_Reference_Impl (New_Value));
               Old_Node : constant Node_Access :=
                 Deref (To_Reference_Impl (Old_Value));
            begin
               if New_Node /= Old_Node then
                  if New_Node /= null then
                     Fetch_And_Add
                       (Managed_Node_Access (New_Node).MM_RC'Access, 1);
                  end if;
                  if Old_Node /= null then
                     declare
                        use type Reference_Count;
                        Old_Value_Base : constant Managed_Node_Access :=
                          Managed_Node_Access (Old_Node);
                        --  Base type view of the node.
                     begin
                        Fetch_And_Add (Old_Value_Base.MM_RC'Access, -1);
                     end;
                  end if;
               end if;
               return True;
            end;
         end if;
         return False;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in     Private_Reference)  is
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
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Private_Reference;
         New_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value)
      is
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
      procedure Compare_And_Swap
        (Link      : access Shared_Reference;
         Old_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value;
         New_Value : in     Basic_Reference_Operations.Unsafe_Reference_Value)
      is
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
      --  Internal operations.
      ----------------------------------------------------------------------

      ----------------------------------------------------------------------
      function Hash_Ref (Ref  : in Managed_Node_Access;
                         Size : in Natural) return Natural is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (Managed_Node_Access,
                                          Primitives.Standard_Unsigned);
         use type Primitives.Standard_Unsigned;
      begin
         return Natural ((To_Unsigned (Ref) / 4) mod
                           Primitives.Standard_Unsigned (Size));
      end Hash_Ref;

      ---------------------------------------------------------------------
      procedure Scan (MM : in Memory_Manager) is
         use P_Sets;
         use type Reference_Count;
         use type TSS.Element_Access;

         Shared      : Task_Shared renames TSS.Get (MM.Shared).all;
         Local       : Task_Local  renames TLS.Get (MM.Local).all;
         Index       : Node_Index;
         Node        : Atomic_Node_Access;
         New_D_List  : Node_Index := 0;
         New_D_Count : Node_Count := 0;
      begin
         --  Set the trace bit on each deleted node with MM_RC = 0.
         Index := Local.D_List;
         while Index /= 0 loop
            Node := Shared.DL_Nodes (Index);
            if Node.MM_RC = 0 then
               Primitives.Membar;
               --  The read of MM_RC must precede the write of MM_trace.
               Node.MM_Trace := True;
               Primitives.Membar;
               --  The write of MM_Trace must precede the reread of MM_RC.
               if Node.MM_RC /= 0 then
                  Node.MM_Trace := False;
               end if;
            end if;
            Index := Local.DL_Nexts (Index);
         end loop;
         Primitives.Membar;
         --  Make sure the memory operations of the algorithm's phases are
         --  separated.

         Clear (Local.P_Set);

         --  Read all hazard pointers.
         for P in Process_Ids.Process_ID_Type'Range loop
            declare
               Shared_P : TSS.Element_Access := TSS.Get (MM.Shared, P);
            begin
               if Shared_P /= null then
                  for I in HP_Index loop
                     Node := Shared_P.Hazard_Pointer (I);
                     if Node /= null then
                        declare
                           N : constant Managed_Node_Access :=
                             Managed_Node_Access (Node);
                        begin
                           Insert (N, Local.P_Set);
                        end;
                     end if;
                  end loop;
               end if;
            end;
         end loop;
         Primitives.Membar;
         --  Make sure the memory operations of the algorithm's phases are
         --  separated.

         --  Attempt to reclaim nodes.
         while Local.D_List /= 0 loop
            Index        := Local.D_List;
            Node         := Shared.DL_Nodes (Index);
            Local.D_List := Local.DL_Nexts (Index);

            if
              Node.MM_RC = 0 and Node.MM_Trace and
              not Member (Managed_Node_Access (Node), Local.P_Set)
            then
               Shared.DL_Nodes (Index) := null;
               Primitives.Membar;
               --  The write to DL_Nodes (ID, Index) must precede the
               --  read of DL_Claims (ID, Index).
               if Shared.DL_Claims (Index) = 0 then
                  Dispose (Managed_Node_Access (Node),
                           Concurrent => False);
                  Free (Managed_Node_Access (Node));

                  if Collect_Statistics then
                     Fetch_And_Add (MM.Mutable.Self.Nodes_Reclaimed'Access, 1);
                  end if;
               else
                  Dispose (Managed_Node_Access (Node),
                           Concurrent => True);
                  Shared.DL_Done  (Index) := True;
                  Shared.DL_Nodes (Index) := Node;

                  --  Keep Node in D_List.
                  Local.DL_Nexts (Index) := New_D_List;
                  New_D_List   := Index;
                  New_D_Count  := New_D_Count + 1;
               end if;
            else
               --  Keep Node in D_List.
               Local.DL_Nexts (Index) := New_D_List;
               New_D_List   := Index;
               New_D_Count  := New_D_Count + 1;
            end if;
         end loop;

         Local.D_List  := New_D_List;
         Local.D_Count := New_D_Count;
      end Scan;

      ----------------------------------------------------------------------
      procedure Clean_Up_Local (MM : in Memory_Manager) is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
         Index  : Node_Index := Local.D_List;
         Node   : Atomic_Node_Access;
      begin
         while Index /= 0 loop
            Node  := Shared.DL_Nodes (Index);
            Clean_Up (MM, Managed_Node_Access (Node));
            Index := Local.DL_Nexts (Index);
         end loop;
      end Clean_Up_Local;

      ----------------------------------------------------------------------
      procedure Clean_Up_All (MM : in Memory_Manager) is
         use type Process_Ids.Process_ID_Type;
         use type TSS.Element_Access;
         Node : Atomic_Node_Access;
      begin
         for P in Process_Ids.Process_ID_Type'Range loop
            if P /= Process_Ids.Process_ID then
               declare
                  Shared_P : TSS.Element_Access := TSS.Get (MM.Shared, P);
               begin
                  if Shared_P /= null then
                     for Index in Valid_Node_Index loop
                        Node := Shared_P.DL_Nodes (Index);
                        if
                          Node /= null and then
                          not Shared_P.DL_Done (Index)
                        then
                           Fetch_And_Add
                             (Target    => Shared_P.DL_Claims (Index)'Access,
                              Increment => 1);
                           if
                             Node = Shared_P.DL_Nodes (Index)
                           then
                              Clean_Up (MM, Managed_Node_Access (Node));
                           end if;
                           Fetch_And_Add
                             (Target    => Shared_P.DL_Claims (Index)'Access,
                              Increment => -1);
                        end if;
                     end loop;
                  end if;
               end;
            end if;
         end loop;
      end Clean_Up_All;

      ----------------------------------------------------------------------
      procedure Validate (Node  : in Private_Reference;
                          Where : in String) is
         use type Reference_Count;
         Testee : Managed_Node_Access := Managed_Node_Access (Deref (Node));
      begin
         if Testee /= null then
            declare
               Shared : Task_Shared renames TSS.Get (Node.MM.Shared).all;
            begin
               if Node.HP < 1 or Node.HP > Max_Number_Of_Dereferences then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-beware_and_cleanup.adb: " &
                     Where &
                     " an invalid private reference, " & Image (Node));
               elsif Shared.Hazard_Pointer (HP_Index (Node.HP)) /= Testee then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-beware_and_cleanup.adb: " &
                     Where &
                     " a released private reference, " & Image (Node));
               elsif Testee.MM_Magic /= MM_Live then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-beware_and_cleanup.adb: " &
                     Where &
                     " a private reference to a non-live node, " &
                     Image (Node) & "!");
               end if;
            end;
         end if;
      end Validate;

      ----------------------------------------------------------------------
      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl is
      begin
         return Shared_Reference_Base (To_Shared_Reference (Node)).Ref;
      end Get_Ref;

      ----------------------------------------------------------------------
      function Create_Private_Reference (Ref  : in Reference_Impl;
                                         HP   : in Index;
                                         MM   : in Memory_Manager_Access)
                                        return Private_Reference is
         package BRO renames Basic_Reference_Operations;
      begin
         return
           Private_Reference'(BRO.Private_Reference_Base
                                (BRO.From_Shared_Reference
                                   (Shared_Reference
                                      (Shared_Reference_Base'(Ref => Ref))))
                              with
                              HP => HP,
                              MM => MM);
      end Create_Private_Reference;

      ----------------------------------------------------------------------
      function  Deref   (Ref : in Reference_Impl)
                        return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Reference_Impl,
                                          Node_Access);

      begin
         return To_Node_Access (Ref and Ref_Mask);
      end Deref;

      ----------------------------------------------------------------------
      function  Deref   (Node : in Private_Reference)
                        return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Reference_Impl,
                                          Node_Access);

      begin
         return To_Node_Access (Get_Ref (Node) and Ref_Mask);
      end Deref;

      ----------------------------------------------------------------------
      function To_Unsafe_Reference (Node : in Private_Reference)
        return Basic_Reference_Operations.Unsafe_Reference_Value is
         package BRO renames Basic_Reference_Operations;
         function To_Unsafe_Reference_Value is
            new Ada.Unchecked_Conversion (Reference_Impl,
                                          BRO.Unsafe_Reference_Value);
      begin
         return To_Unsafe_Reference_Value (Get_Ref (Node));
      end To_Unsafe_Reference;

      ----------------------------------------------------------------------
   end Reference_Operations;

end NBAda.Memory_Reclamation.Beware_And_Cleanup;
