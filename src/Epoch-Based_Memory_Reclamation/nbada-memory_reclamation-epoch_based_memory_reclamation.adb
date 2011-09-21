-------------------------------------------------------------------------------
--  Epoch-based memory reclamation.
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
pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--  Filename        : nbada-memory_reclamation-epoch_based_memory_reclamation.adb
--  Description     : Implementation of Keir Fraser's epoch-based memory
--                    reclamation scheme.
--                    See Keir Fraser, "Practical lock-freedom",
--                    Technical Report 579, Computer Laboratory,
--                    University of Cambridge, 2004.
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;

with NBAda.Internals.Cleanup_Tools;

package body NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------
   subtype Processes is Process_Ids.Process_ID_Type;

   package body Reference_Operations is

      procedure Enter_Critical_Section (MM : in out Memory_Manager);
      procedure Exit_Critical_Section  (MM : in out Memory_Manager);

      procedure Update_Global_Epoch (MM : in out Memory_Manager);
      procedure Cleanup (MM    : in out Memory_Manager;
                         Epoch : in     Epoch_ID);

      function Create_Private_Reference (Ref  : in Reference_Impl;
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

      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl;
      pragma Inline (Get_Ref);
      pragma Inline_Always (Get_Ref);

      procedure CAS is
         new Primitives.Standard_Void_Compare_And_Swap (Epoch);

      function To_Reference_Impl is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Reference_Impl);
      type Shared_Reference_Access is access all Shared_Reference;
      type Reference_Impl_Access is access all Reference_Impl;
      function To_Reference_Impl_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Reference_Impl_Access);

      ----------------------------------------------------------------------
      function Null_Reference return Private_Reference is
      begin
         return (Basic_Reference_Operations.Private_Reference_Base
                 with MM => null);
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
              ")";

         else
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) &
              ")";
         end if;
      exception
         when Storage_Error =>
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) &
              ")";
      end Image;

      ----------------------------------------------------------------------
      function  Dereference (MM   : in     Memory_Manager;
                             Link : access Shared_Reference)
                            return Private_Reference is
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
         Node   : constant Reference_Impl :=
           Shared_Reference_Base (Link.all).Ref;
      begin
         if Integrity_Checking then
            if Node = 16#ffffffff# then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-" &
                    "epoch_based_memory_reclamation.adb: " &
                    "Dereference found a nonsense reference value." &
                    Reference_Impl'Image (Node));
            end if;
         end if;
         if Deref (Node) /= null then
            --  Check whether this task is already in a critical section.
            if Local.Dereferenced_Count = 0 then
               Enter_Critical_Section (MM.Mutable.Self.all);
            end if;
            Local.Dereferenced_Count := Local.Dereferenced_Count + 1;
         end if;

         return Create_Private_Reference
           (Ref => Node,
            MM  => MM.Mutable.Self.all'Unchecked_Access);
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
      begin
         if Deref (Node) /= null then
            declare
               Local  : Task_Local renames TLS.Get (Node.MM.Local).all;
            begin
               if Integrity_Checking then
                  Validate (Node, "Attempting to release");
               end if;
               if Local.Dereferenced_Count = 0 then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-" &
                     "epoch_based_memory_reclamation.adb: " &
                     "Fatal error: More private references released than " &
                     "dereferenced!");
               end if;
               Local.Dereferenced_Count := Local.Dereferenced_Count - 1;
               if Local.Dereferenced_Count = 0 then
                  Exit_Critical_Section (Node.MM.all);
               end if;
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
         end if;
         Void_Compare_And_Swap_Impl
           (Target =>
              To_Reference_Impl_Access (Link.all'Unchecked_Access),
            Old_Value => Get_Ref (Old_Value),
            New_Value => Get_Ref (New_Value));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Delete  (Node : in Private_Reference) is
         Shared  : Task_Shared renames TSS.Get (Node.MM.Shared).all;
         Local   : Task_Local  renames TLS.Get (Node.MM.Local).all;
         Epoch   : constant Epoch_ID := Shared.Current_Epoch.ID mod 3;
         Deleted : Node_Access       := Deref (Node);
      begin
         if Deref (Node) = null then
            return;
         end if;
         if Integrity_Checking then
            Validate (Node, "Attempting to delete");
         end if;

         Release (Node);
         Deleted.MM_Next       := Managed_Node_Access (Local.D_List (Epoch));
         Local.D_List  (Epoch) := Deleted;
         Local.D_Count         := Local.D_Count + 1;
      end Delete;

      ----------------------------------------------------------------------
      procedure Rescan  (Node : in Private_Reference) is
      begin
         null;
      end Rescan;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
      begin
         Link.all := To_Shared_Reference (Node);
      end Store;

      ----------------------------------------------------------------------
      function Create (MM : in Memory_Manager) return Private_Reference is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
         UNode  : constant User_Node_Access := new Managed_Node;
         Node   : constant Node_Access      := UNode.all'Unchecked_Access;
      begin
         --  Check whether this task is already in a critical section.
         if Local.Dereferenced_Count = 0 then
            Enter_Critical_Section (MM.Mutable.Self.all);
         end if;
         Local.Dereferenced_Count := Local.Dereferenced_Count + 1;

         Primitives.Fetch_And_Add (MM.Mutable.Self.Created'Access, 1);

         return Create_Private_Reference
           (Ref => To_Reference_Impl (Node),
            MM  => MM.Mutable.Self.all'Unchecked_Access);
      end Create;

      ----------------------------------------------------------------------
      function "=" (Left, Right : in Private_Reference) return Boolean is
      begin
         return Basic_Reference_Operations."=" (Left, Right);
      end "=";

      -------------------------------------------------------------------------
      --  Private operations.
      -------------------------------------------------------------------------

      -------------------------------------------------------------------------
      procedure Validate (Node  : in Private_Reference;
                          Where : in String) is
         Testee : Node_Access := Deref (Node);
      begin
         if Node.MM = null then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "nbada-memory_reclamation-" &
               "epoch_based_memory_reclamation.adb: " &
               Where &
               " the private reference is uninitialized!");
         end if;
         if Testee /= null then
            declare
               Shared : Task_Shared renames TSS.Get (Node.MM.Shared).all;
               Local  : Task_Local  renames TLS.Get (Node.MM.Local).all;
            begin
               if Local.Dereferenced_Count = 0 then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-" &
                     "epoch_based_memory_reclamation.adb: " &
                     Where &
                     " the private reference has not been aquired!");
               elsif Get_Ref (Node) = 16#ffffffff# then
                  Ada.Exceptions.Raise_Exception
                    (Constraint_Error'Identity,
                     "nbada-memory_reclamation-" &
                     "epoch_base_memory_reclamation.adb: " &
                     Where &
                     " the private reference has a nonsense value!");
               end if;
            end;
         end if;
      end Validate;

      -------------------------------------------------------------------------
      procedure Enter_Critical_Section (MM : in out Memory_Manager) is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
      begin
         --  Look for epoch change.
         --      Ada.Text_IO.Put ("l");
         declare
            Last_Epoch_ID : constant Epoch_ID := Shared.Current_Epoch.ID;
         begin
            Shared.Current_Epoch := MM.Global_Epoch;
            Primitives.Membar;
            if Shared.Current_Epoch.ID /= Last_Epoch_ID then
               --  Clean up the delete list of the epoch before
               --  Last_Epoch.
               Cleanup (MM, Last_Epoch_ID - 1);
               if Last_Epoch_ID /= Shared.Current_Epoch.ID - 1 then
                  --  Last epoch is also safe to clean.
                  Cleanup (MM, Last_Epoch_ID);
               end if;
               Local.CS_Count := 0;
            end if;

            --  Attempt to update the global epoch.
            --  The above step should probably be redone if successful.
            if Natural (Local.CS_Count) < Epoch_Update_Threshold then
               Local.CS_Count :=  Local.CS_Count + 1;
            else
               Update_Global_Epoch (MM);
            end if;
         end;
      end Enter_Critical_Section;

      -------------------------------------------------------------------------
      procedure Exit_Critical_Section  (MM : in out Memory_Manager) is
         Shared : Task_Shared renames TSS.Get (MM.Shared).all;
         Local  : Task_Local  renames TLS.Get (MM.Local).all;
      begin
         --  Look for epoch change.
         --      Ada.Text_IO.Put ("u");
         declare
            Last_Epoch_ID : constant Epoch_ID := Shared.Current_Epoch.ID;
         begin
            Primitives.Membar;
            --  Clear the active flag.
            Shared.Current_Epoch := (MM.Global_Epoch.ID, False);

            if Shared.Current_Epoch.ID /= Last_Epoch_ID then
               --  Clean up the delete list of the epoch before
               --  Last_Epoch.
               Cleanup (MM, Last_Epoch_ID - 1);
               if Last_Epoch_ID /= Shared.Current_Epoch.ID - 1 then
                  --  Last epoch is also safe to clean.
                  Cleanup (MM, Last_Epoch_ID);
               end if;
               Local.CS_Count := 0;
            end if;
         end;
      end Exit_Critical_Section;

      -------------------------------------------------------------------------
      procedure Update_Global_Epoch (MM : in out Memory_Manager) is
         use type TSS.Element_Access;
         Current_Epoch_ID : constant Epoch_ID := MM.Global_Epoch.ID;
      begin
         for P in Processes'Range loop
            if TSS.Get (MM.Shared, P) /= null then
               declare
                  Current_Epoch_P : Epoch renames
                    TSS.Get (MM.Shared, P).Current_Epoch;
               begin
                  if Current_Epoch_P.Active and
                    Current_Epoch_P.ID /= Current_Epoch_ID
                  then
                     --  Process P is lagging behind.
                     return;
                  end if;
               end;
            end if;
         end loop;

         CAS (Target    => MM.Global_Epoch'Access,
              Old_Value => (Current_Epoch_ID, True),
              New_Value => (Current_Epoch_ID + 1, True));
      end Update_Global_Epoch;

      -------------------------------------------------------------------------
      procedure Cleanup (MM    : in out Memory_Manager;
                         Epoch : in     Epoch_ID) is
         Local : Task_Local  renames TLS.Get (MM.Local).all;
         Node  : Node_Access;
      begin
         while Local.D_List (Epoch mod 3) /= null loop
            Node        := Local.D_List (Epoch mod 3);
            Local.D_List (Epoch mod 3) := Node_Access (Node.MM_Next);

            --  Reclaim node storage.
            Free (Node);

            Primitives.Fetch_And_Add (MM.Reclaimed'Access, 1);
            Local.D_Count := Local.D_Count - 1;
         end loop;
      end Cleanup;

      ----------------------------------------------------------------------
      function Create_Private_Reference (Ref  : in Reference_Impl;
                                         MM   : in Memory_Manager_Access)
                                        return Private_Reference is
         package BRO renames Basic_Reference_Operations;
      begin
         return
           Private_Reference'(BRO.Private_Reference_Base
                                (BRO.From_Shared_Reference
                                   (Shared_Reference
                                      (Shared_Reference_Base'(Ref => Ref))))
                              with MM => MM);
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
      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl is
      begin
         return Shared_Reference_Base (To_Shared_Reference (Node)).Ref;
      end Get_Ref;

   end Reference_Operations;
   ----------------------------------------------------------------------------

end NBAda.Memory_Reclamation.Epoch_Based_Memory_Reclamation;
