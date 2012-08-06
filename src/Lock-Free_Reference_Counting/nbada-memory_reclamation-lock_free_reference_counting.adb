-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - Lock-Free Reference Counting based on the
--  algorithm by Herlihy et al.
--  Copyright (C) 2006 - 2012  Anders Gidenstam
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
--  Filename        : nbada-memory_reclamation-lock_free_reference_counting.adb
--  Description     : Ada implementation of lock-free reference counting.
--                    Based on M. Herlihy, V. Luchango, P. Martin, M. Moir,
--                    "Nonblocking Memory Management Support for Dynamic-Sized
--                     Data Structures",  ACM Transactions on Computer Systems,
--                    23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Pass_The_Buck;
with NBAda.Internals.Cleanup_Tools;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;

package body NBAda.Memory_Reclamation.Lock_Free_Reference_Counting is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Reference_Count);

   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add_32;

   function Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                           Increment : in     Primitives.Unsigned_32)
                          return Primitives.Unsigned_32
     renames Primitives.Fetch_And_Add_32;

   package PTB is new Pass_The_Buck
     (Max_Number_Of_Guards => Integer (Process_Ids.Process_ID_Type'Last),
      Value_Type           => Managed_Node_Access,
      Null_Value           => null);
   --  FIXME: The pass the buck instance should really be per memory manager
   --         object.

   function Image (Node : in Managed_Node_Access) return String;

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   -----------------------------------------------------------------------
   function Is_Deleted (Node : access Managed_Node_Base)
                       return Boolean is
   begin
      return False;
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

      procedure Validate (Node  : in Private_Reference;
                          Where : in String);

      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl;
      pragma Inline (Get_Ref);
      pragma Inline_Always (Get_Ref);
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

      function To_Unsafe_Reference (Node : in Private_Reference)
        return Basic_Reference_Operations.Unsafe_Reference_Value;
      pragma Inline (To_Unsafe_Reference);
      pragma Inline_Always (To_Unsafe_Reference);

      procedure Clean_And_Liberate (Node : in Managed_Node_Access);

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
              BRO.Image (BRO.Private_Reference_Base (R)) & ")";
         else
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) & ")";
         end if;
      exception
         when Storage_Error =>
            return "(" &
              "@" &
              BRO.Image (BRO.Private_Reference_Base (R)) & ")";
      end Image;

      ----------------------------------------------------------------------
      function  Dereference (MM   : in     Memory_Manager'Class;
                             Link : access Shared_Reference)
                            return Private_Reference is

         Guard    : constant PTB.Guard_Type := PTB.Hire_Guard;
         Node_Ref : Reference_Impl;
         State    : MM_Magic_Type;
      begin
         Dereference :
         loop
            Node_Ref := Shared_Reference_Base (Link.all).Ref;

            exit Dereference when Deref (Node_Ref) = null;

            PTB.Post_Guard (Guard,
                            Managed_Node_Access (Deref (Node_Ref)));

            if Shared_Reference_Base (Link.all).Ref = Node_Ref then
               declare
                  use type Reference_Count;
                  Node_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (Deref (Node_Ref));
                  --  Base type view of the node.
                  Old_RC    : Reference_Count := Node_Base.MM_RC;
               begin
                  if Integrity_Checking then
                     State := Node_Base.MM_Magic;
                  end if;
                  Primitives.Membar;
                  --  The integrity check must precede the ordinary
                  --  loop exit test.
                  while Old_RC > 0 loop
                     exit Dereference when Compare_And_Swap
                       (Target    => Node_Base.MM_RC'Access,
                        Old_Value => Old_RC,
                        New_Value => Old_RC + 1);
                     Old_RC := Node_Base.MM_RC;
                  end loop;
               end;
            end if;
         end loop Dereference;

         PTB.Post_Guard (Guard, null);
         PTB.Fire_Guard (Guard);

         declare
            Result : constant Private_Reference :=
              Create_Private_Reference
              (Ref => Node_Ref,
               MM  => MM.Mutable.Self.all'Unchecked_Access);
         begin
            if Integrity_Checking then
               Validate (Result, "Dereferenced ");
            end if;
            return Result;
         end;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
         use type Reference_Count;
      begin
         if Deref (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Deref (Node));
               --  Base type view of the node.
            begin
               if Fetch_And_Add (Target    => Node_Base.MM_RC'Access,
                                 Increment => -1) = 1
               then
                  Clean_And_Liberate (Node_Base);
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
      function  Copy (Node : in Private_Reference) return Private_Reference is
         use type Reference_Count;
      begin
         if Deref (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Deref (Node));
               --  Base type view of the node.
            begin
               Fetch_And_Add (Target    => Node_Base.MM_RC'Access,
                              Increment => 1);
               return Node;
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
         --  Since we have dereferenced both Old_Value and New_Value they
         --  are guaranteed to have positive reference counts, regardless
         --  how the CAS goes.
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
                  end;
               end if;

               if Deref (Old_Value) /= null then
                  declare
                     Old_Value_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (Deref (Old_Value));
                     --  Base type view of the node.
                  begin
                     if Fetch_And_Add
                       (Target    => Old_Value_Base.MM_RC'Access,
                        Increment => -1) = 1
                     then
                        Clean_And_Liberate (Old_Value_Base);
                     end if;
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
      procedure Rescan  (Node : in Private_Reference) is
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to rescan");
         end if;
      end Rescan;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
         package BRO renames Basic_Reference_Operations;
         package ID  renames BRO.Implementation_Details;
         use type Reference_Count;
         Old : constant Node_Access :=
           Deref (Shared_Reference_Base (Link.all).Ref);
      begin
         if Integrity_Checking then
            Validate (Node, "Attempting to store");
         end if;
         Link.all :=
           ID.To_Shared_Reference (BRO.Private_Reference_Base (Node));

         if Deref (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Deref (Node));
               --  Base type view of the node.
            begin
               Fetch_And_Add (Node_Base.MM_RC'Access, 1);
            end;
         end if;

         if Old /= null then
            declare
               Old_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Old);
               --  Base type view of the node.
            begin
               if Fetch_And_Add (Target    => Old_Base.MM_RC'Access,
                                 Increment => -1) = 1
               then
                  Clean_And_Liberate (Old_Base);
               end if;
            end;
         end if;
      end Store;

      ----------------------------------------------------------------------
      function Create (MM : in Memory_Manager'Class)
                      return Private_Reference is
         UNode : constant User_Node_Access := new Managed_Node;
         Node  : constant Node_Access      := UNode.all'Unchecked_Access;
      begin
         if Collect_Statistics then
            Fetch_And_Add (MM.Mutable.Self.No_Nodes_Created'Access, 1);
         end if;

         return Create_Private_Reference
           (Ref => To_Reference_Impl (Node),
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
                        if Fetch_And_Add
                          (Target    => Old_Value_Base.MM_RC'Access,
                           Increment => -1) = 1
                        then
                           Clean_And_Liberate (Old_Value_Base);
                        end if;
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
      procedure Validate (Node  : in Private_Reference;
                          Where : in String) is
         use type Reference_Count;
         Testee : Managed_Node_Access := Managed_Node_Access (Deref (Node));
      begin
         if Testee /= null then
            if Testee.MM_RC < 1 then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-lock_free_reference_counting.adb: "
                  & Where &
                  " a private reference to a node with RC=0, " &
                  Image (Node) & "!");
            elsif Testee.MM_Magic /= MM_Live then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-lock_free_reference_counting.adb: "
                  & Where &
                  " a private reference to a non-live node, " &
                  Image (Node) & "!");
            end if;
         end if;
      end Validate;

      ----------------------------------------------------------------------
      function Get_Ref (Node : in Private_Reference)
                       return Reference_Impl is
         package BRO renames Basic_Reference_Operations;
         package ID  renames BRO.Implementation_Details;
      begin
         return
           Shared_Reference_Base
           (ID.To_Shared_Reference
              (BRO.Private_Reference_Base (Node))).Ref;
      end Get_Ref;

      ----------------------------------------------------------------------
      function Create_Private_Reference (Ref  : in Reference_Impl;
                                         MM   : in Memory_Manager_Access)
                                        return Private_Reference is
         package BRO renames Basic_Reference_Operations;
         package ID  renames BRO.Implementation_Details;
      begin
         return
           Private_Reference'(BRO.Private_Reference_Base
                                (ID.From_Shared_Reference
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
      procedure Clean_And_Liberate (Node : in Managed_Node_Access) is
      begin
         if Integrity_Checking then
            if Node = null or else Node.MM_Magic /= MM_Live then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-memory_reclamation-lock_free_reference_counting.adb: "
                  &
                  "Attempting to Clean_And_Liberate the already cleaned node "
                  & Image (Node) & "!");
            end if;
         end if;
         Node.MM_Magic := MM_Reclaimable;
         declare
            use type Reference_Count;
            Refs : constant Reference_Set := All_References (Node);
         begin
            Remove_References : loop
               declare
                  All_Null : Boolean := True;
               begin
                  for I in Refs'Range loop
                     declare
                        Next : constant Managed_Node_Access :=
                          Managed_Node_Access (Deref (Refs (I).all.Ref));
                     begin
                        if Next /= null then
                           --  Lower the Next's RC.
                           if Fetch_And_Add (Target    => Next.MM_RC'Access,
                                             Increment => -1) = 1
                           then
                              declare
                                 Next_Refs : constant Reference_Set :=
                                   All_References (Next);
                              begin
                                 All_Null              := False;
                                 Refs (I).all          := Next_Refs (I).all;
                                 Next_Refs (I).all.Ref := 0;
                                 Clean_And_Liberate (Next);
                              end;
                           else
                              Refs (I).all.Ref := 0;
                           end if;
                        end if;
                     end;
                  end loop;

                  exit Remove_References when All_Null;
               end;
            end loop Remove_References;
         end;

         declare
            Old : constant PTB.Value_Set (1 .. 1)
              := (1 => Node);
            VS  : constant PTB.Value_Set
              := PTB.Liberate (Old);
         begin
            for I in VS'Range loop
               if Integrity_Checking then
                  if
                    VS (I) = null or else VS (I).MM_Magic /= MM_Reclaimable
                  then
                     declare
                        MM_Magic : constant MM_Magic_Type := VS (I).MM_Magic;
                     begin
                        if MM_Magic = MM_Live then
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "nbada-memory_reclamation-" &
                              "lock_free_reference_counting.adb: " &
                              "Attempt to reclaim the live node " &
                              Image (VS (I)) & "!");
                        elsif  MM_Magic = MM_Reclaimed then
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "nbada-memory_reclamation-" &
                              "lock_free_reference_counting.adb: " &
                              "Attempt to reclaim the already reclaimed node "
                              & Image (VS (I)) & "!");
                        else
                           Ada.Exceptions.Raise_Exception
                             (Constraint_Error'Identity,
                              "nbada-memory_reclamation-" &
                              "lock_free_reference_counting.adb: " &
                              "Attempt to reclaim the non-valid node " &
                              Image (VS (I)) & "!");
                        end if;
                     end;
                  end if;
               end if;
               VS (I).MM_Magic := MM_Reclaimed;
               Free (VS (I));
            end loop;
--          if Collect_Statistics then
--             Fetch_And_Add (No_Nodes_Reclaimed'Access, VS'Length);
--          end if;
         end;
      end Clean_And_Liberate;

      ----------------------------------------------------------------------
   end Reference_Operations;

   ----------------------------------------------------------------------------
   function Image (Node : in Managed_Node_Access) return String is
      function To_Unsigned is new
        Ada.Unchecked_Conversion (Managed_Node_Access,
                                  Primitives.Standard_Unsigned);
      use type Primitives.Standard_Unsigned;
   begin
      return Primitives.Standard_Unsigned'Image (To_Unsigned (Node));
   end Image;

end NBAda.Memory_Reclamation.Lock_Free_Reference_Counting;
