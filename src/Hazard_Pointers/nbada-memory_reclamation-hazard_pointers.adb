-------------------------------------------------------------------------------
--  Hazard Pointers - An implementation of Maged Michael's hazard pointers.
--  Copyright (C) 2011  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-hazard_pointers.adb
--  Description     : Lock-free Ada implementation of Maged Michael's
--                    Hazard Pointers for safe memory management.
--                    Based on Maged Michael, "Hazard Pointers: Safe Memory
--                    Reclamation for Lock-Free Objects", IEEE Transactions on
--                    Parallell and Distributed Systems, 15(6), 491--504,
--                    June 2004.
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Internals.Hash_Tables;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;

package body NBAda.Memory_Reclamation.Hazard_Pointers is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   function  Image (Node : in Managed_Node_Access) return String;

   ----------------------------------------------------------------------------
   package body Reference_Operations is

      ----------------------------------------------------------------------
      function To_Reference_Impl is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Reference_Impl);

      type Shared_Reference_Access is access all Shared_Reference;
      type Reference_Impl_Access is access all
        Reference_Impl;
      function To_Reference_Impl_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Reference_Impl_Access);

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
      function  Dereference (MM   : in     Memory_Manager;
                             Link : access Shared_Reference)
                            return Private_Reference is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
         Index  : HP_Index;
         Ref    : Reference_Impl;
         Found  : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in Shared.Hazard_Pointer'Range loop
            if Shared.Hazard_Pointer (I) = null then
               --  Found a free hazard pointer.
               Index   := I;
               Found   := True;
               exit;
            end if;
         end loop;
         --  Dereference node iff there is a free hazard pointer.
         if not Found then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-hazard_pointers.adb: " &
               "Maximum number of local dereferences exceeded!");
         else
            if Integrity_Checking then
               declare
                  use type Primitives.Unsigned_32;
               begin
                  loop
                     Ref := Shared_Reference_Base (Link.all).Ref;
                     Shared.Hazard_Pointer (Index) := Deref (Ref);

                     if
                       Deref (Ref) /= null and then
                       Ref = 16#ffffffff#
                     then
                        Ada.Exceptions.Raise_Exception
                          (Constraint_Error'Identity,
                           "nbada-memory_reclamation-hazard_pointers.adb: " &
                             "Dereference found a nonsense reference value." &
                             Reference_Impl'Image (Ref));
                     end if;

                     Primitives.Membar;
                     --  The write to the hazard pointer must be visible
                     --  before Link is read again.
                     exit when Ref = Shared_Reference_Base (Link.all).Ref;
                  end loop;
                  if Deref (Ref) /= null then
                     declare
                        State : constant Primitives.Unsigned_32 :=
                          Deref (Ref).MM_Magic;
                     begin
                        if not (State = MM_Live or State = MM_Deleted) then
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "nbada-memory_reclamation-hazard_pointers.adb: "&
                              "Dereferenced a node with the bad MM_Magic " &
                              Primitives.Unsigned_32'Image (State) &
                              "! " & Reference_Impl'Image (Ref));
                        end if;
                     end;
                  end if;
               end;
            else
               loop
                  Ref := Shared_Reference_Base (Link.all).Ref;
                  Shared.Hazard_Pointer (Index) := Deref (Ref);

                  Primitives.Membar;
                  --  The write to the hazard pointer must be visible before
                  --  Link is read again.
                  exit when Shared_Reference_Base (Link.all).Ref = Ref;
               end loop;
            end if;
         end if;

         return Create_Private_Reference
           (Ref => Ref,
            HP  => Natural (Index),
            MM  => MM.Mutable.Self.all'Unchecked_Access);
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release     (Node : in Private_Reference) is
      begin
         Primitives.Membar;
         --  Complete all preceding memory operations before releasing
         --  the hazard pointer.
         if Deref (Node) /= null then
            declare
               Shared : TSS.Element_Access :=
                 TSS.Get (Node.MM.all.Shared);
            begin
               if Integrity_Checking then
                  Validate (Node, "Attempting to release");
               end if;
               Shared.Hazard_Pointer (HP_Index (Node.HP)) := null;
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
      function Compare_And_Swap (Link      : access Shared_Reference;
                                 Old_Value : in Private_Reference;
                                 New_Value : in Private_Reference)
                                return Boolean is
      begin
         if Integrity_Checking then
            Validate (Old_Value, "Attempting a CAS where Old_Value is");
            Validate (New_Value, "Attempting a CAS where New_Value is");
            if Get_Ref (New_Value) = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-hazard_pointers.adb: " &
                  "Error: Attempting to CAS a nonsense reference value!");
            end if;
         end if;
         return Boolean_Compare_And_Swap_Impl
           (Target =>
              To_Reference_Impl_Access (Link.all'Unchecked_Access),
            Old_Value => Get_Ref (Old_Value),
            New_Value => Get_Ref (New_Value));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference) is
      begin
         if Integrity_Checking then
            Validate (Old_Value, "Attempting a CAS where Old_Value is");
            Validate (New_Value, "Attempting a CAS where New_Value is");
            if Get_Ref (New_Value) = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-hazard_pointers.adb: " &
                  "Error: Attempting to CAS a nonsense reference value!");
            end if;
         end if;
         Void_Compare_And_Swap_Impl
           (Target =>
              To_Reference_Impl_Access (Link.all'Unchecked_Access),
            Old_Value => Get_Ref (Old_Value),
            New_Value => Get_Ref (New_Value));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Delete      (Node : in Private_Reference) is
      begin
         if Deref (Node) = null then
            return;
         end if;
         declare
            Local   : TLS.Element_Access   := TLS.Get (Node.MM.all.Local);
            Deleted : constant Node_Access := Deref (Node);
            use type Primitives.Unsigned_32;
         begin
            if Integrity_Checking then
               Validate (Node, "Attempting to delete");
               if Deleted.MM_Magic /= MM_Live then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-hazard_pointers.adb: " &
                     "Error: Deleting an already deleted or non-existing " &
                     "node! " &
                     Image (Node));
               end if;
               Managed_Node_Base (Deleted.all).MM_Magic := MM_Deleted;
            end if;

            Release (Node);
            Managed_Node_Base (Deleted.all).MM_Next :=
              Managed_Node_Access (Local.D_List);
            Local.D_List  := Deleted;
            Local.D_Count := Local.D_Count + 1;
            if Local.D_Count >=
              Node_Count (1.5 *
                          Float (Node_Count
                                   (Process_Ids.Process_ID_Type'Last) *
                                    Node_Count (Max_Number_Of_Dereferences)))
            then
               Scan (Node.MM.all);
            end if;
         end;
      end Delete;

      ----------------------------------------------------------------------
      procedure Rescan  (Node : in Private_Reference) is
         Deleted : constant Node_Access := Deref (Node);
      begin
         if Deref (Node) = null then
            return;
         end if;
         if Integrity_Checking then
            Validate (Node, "Attempting to rescan");
         end if;
         Managed_Node_Base (Deleted.all).MM_Rescan := True;
      end Rescan;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in     Private_Reference) is
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to store");
            if Get_Ref (Node) = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-hazard_pointers.adb: " &
               "Error: Storing a nonsense reference value! " &
               Image (Node));
            end if;
         end if;

         Link.all := To_Shared_Reference (Node);
      end Store;

      ----------------------------------------------------------------------
      function Create (MM : in Memory_Manager) return Private_Reference is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Index  : HP_Index;
         Found  : Boolean := False;
      begin
         --  Find a free hazard pointer.
         for I in Shared.Hazard_Pointer'Range loop
            if Shared.Hazard_Pointer (I) = null then
               --  Found a free hazard pointer.
               Index := I;
               Found := True;
               exit;
            end if;
         end loop;
         --  Dereference node iff there is a free hazard pointer.
         if Found then
            declare
               UNode : constant User_Node_Access := new Managed_Node;
               Node  : constant Node_Access      := UNode.all'Unchecked_Access;
            begin
               Shared.Hazard_Pointer (Index) := Node;

               Primitives.Fetch_And_Add (MM.Mutable.Self.Created'Access, 1);

               if Verbose_Debug then
                  Ada.Text_IO.Put_Line ("Created node " &
                                        Image (Managed_Node_Access (Node)));
               end if;

               return Create_Private_Reference
                 (Ref => To_Reference_Impl (Node),
                  HP  => Natural (Index),
                  MM  => MM.Mutable.Self.all'Unchecked_Access);
            end;
         else
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-hazard_pointers.adb: " &
               "Maximum number of local dereferences exceeded!");
         end if;
      end Create;

      ----------------------------------------------------------------------
      function "=" (Left, Right : in Private_Reference) return Boolean is
      begin
         return Basic_Reference_Operations."=" (Left, Right);
      end "=";

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
      procedure Scan (MM : in Memory_Manager) is
         use P_Sets;
         use type Primitives.Unsigned_32;
         use type TSS.Element_Access;

         Local       : Task_Local renames TLS.Get (MM.Local).all;
         New_D_List  : Node_Access;
         New_D_Count : Node_Count := 0;
         Node        : Node_Access;
         Ref         : Node_Access;
      begin
         Clear (Local.P_Set);
         --  Snapshot all hazard pointers.
         for P in Process_Ids.Process_ID_Type'Range loop
            declare
               Shared : constant TSS.Element_Access := TSS.Get (MM.Shared, P);
            begin
               if Shared /= null then
                  for I in HP_Index'Range loop
                     Ref := Shared.Hazard_Pointer (I);
                     if Ref /= null then
                        Insert (Ref, Local.P_Set);
                     end if;
                  end loop;
               end if;
            end;
         end loop;

         Primitives.Membar;
         --  Make sure the memory operations of the algorithm's phases are
         --  separated.

         while Local.D_List /= null loop
            Node := Local.D_List;

            if Verbose_Debug then
               Ada.Text_IO.Put_Line ("Scanning node " &
                                     Image (Managed_Node_Access (Node)));
            end if;
            if Integrity_Checking then
               begin
                  if Node /= null and then Node.MM_Next /= null then
                     --  Just to force Node to be dereferenced.
                     null;
                  end if;
               exception
                  when Storage_Error =>
                     Ada.Exceptions.Raise_Exception
                       (Constraint_Error'Identity,
                        "nbada-memory_reclamation-hazard_pointers.adb: " &
                          "Error: Invalid node pointer (" &
                          Image (Managed_Node_Access (Node)) &
                          " ) in deletion list!");
               end;
            end if;

            Local.D_List := Node_Access (Node.MM_Next);
            if Node.MM_Rescan or Member (Node, Local.P_Set) then
               Node.MM_Rescan := False;
               Node.MM_Next   := Managed_Node_Access (New_D_List);
               New_D_List     := Node;
               New_D_Count    := New_D_Count + 1;
            else
               --  Reclaim node storage.
               if Integrity_Checking then
                  if Node.MM_Magic /= MM_Deleted then
                     Ada.Exceptions.Raise_Exception
                       (Constraint_Error'Identity,
                        "nbada-memory_reclamation-hazard_pointers.adb: " &
                          "Error: Reclaiming an undeleted node!");
                  end if;
                  Node.MM_Magic := MM_Reclaimed;
               end if;
               Node.MM_Next  := null;

               if Verbose_Debug then
                  Ada.Text_IO.Put_Line ("Reclaiming node " &
                                        Image (Managed_Node_Access (Node)));
               end if;
               Free (Node);

               Primitives.Fetch_And_Add (MM.Mutable.Self.Reclaimed'Access, 1);
            end if;
         end loop;
         Local.D_List  := New_D_List;
         Local.D_Count := New_D_Count;
      end Scan;

      ----------------------------------------------------------------------
      procedure Validate (Node  : in Private_Reference;
                          Where : in String) is
         Testee : Node_Access := Deref (Node);
      begin
         if Testee /= null then
            declare
               Shared : Task_Shared renames TSS.Get (Node.MM.all.Shared).all;
            begin
               if Node.HP < 1 or Node.HP > Max_Number_Of_Dereferences then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-hazard_pointers.adb: " &
                       Where &
                       " an invalid private reference, " & Image (Node));
               elsif Shared.Hazard_Pointer (HP_Index (Node.HP)) /= Testee then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-hazard_pointers.adb: " &
                       Where &
                  " a released private reference, " & Image (Node));
               end if;
            end;
         end if;
      end Validate;

      ----------------------------------------------------------------------
      function Hash_Ref (Ref  : in Node_Access;
                         Size : in Natural) return Natural is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (Node_Access,
                                          Primitives.Standard_Unsigned);
         use type Primitives.Standard_Unsigned;
      begin
         return Natural ((To_Unsigned (Ref) / 4) mod
                           Primitives.Standard_Unsigned (Size));
      end Hash_Ref;

   end Reference_Operations;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Image (Node : in Managed_Node_Access) return String is
      function To_Unsigned is
         new Ada.Unchecked_Conversion (Managed_Node_Access,
                                       Primitives.Standard_Unsigned);
   begin
      return Primitives.Standard_Unsigned'Image (To_Unsigned (Node));
   end Image;

end NBAda.Memory_Reclamation.Hazard_Pointers;
