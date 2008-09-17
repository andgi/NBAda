-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - Lock-Free Reference Counting based on the
--  algorithm by Herlihy et al.
--  Copyright (C) 2006 - 2008  Anders Gidenstam
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
--  Filename        : lock_free_reference_counting.adb
--  Description     : Ada implementation of lock-free reference counting.
--                    Based on M. Herlihy, V. Luchango, P. Martin, M. Moir,
--                    "Nonblocking Memory Management Support for Dynamic-Sized
--                     Data Structures",  ACM Transactions on Computer Systems,
--                    23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Nov 29 16:55:18 2006
--  $Id: nbada-lock_free_reference_counting.adb,v 1.14.2.1 2008/09/17 22:34:25 andersg Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with NBAda.Pass_The_Buck;
with NBAda.Internals.Cleanup_Tools;

with Ada.Unchecked_Conversion;
with Ada.Exceptions;
with Ada.Tags;
with Ada.Text_IO;

package body NBAda.Lock_Free_Reference_Counting is

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

   procedure Clean_And_Liberate (Node : in Managed_Node_Access);

   package PTB is new Pass_The_Buck
     (Max_Number_Of_Guards => Max_Number_Of_Guards,
      Value_Type           => Managed_Node_Access,
      Null_Value           => null);

   function Image (Node : in Managed_Node_Access) return String;

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Statistics counters.
   No_Nodes_Created   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Created);
   No_Nodes_Reclaimed : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Reclaimed);

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
   package body Operations is

      ----------------------------------------------------------------------
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Shared_Reference,
                                       Private_Reference);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Private_Reference);

      function To_Node_Access (X : Private_Reference)
                              return Node_Access;
      --  To_Node_Access strips any mark from X and returns an access type.
      pragma Inline (To_Node_Access);

      type Shared_Reference_Base_Access is access all Shared_Reference_Base;
      type Shared_Reference_Access is access all Shared_Reference;
      function To_Shared_Reference_Base_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Shared_Reference_Base_Access);

      function Compare_And_Swap is new
        Primitives.Standard_Boolean_Compare_And_Swap (Shared_Reference_Base);

      Mark_Mask  : constant Private_Reference := 2 ** Mark_Bits - 1;
      Ref_Mask   : constant Private_Reference := -(2 ** Mark_Bits);

      ----------------------------------------------------------------------
      function Image (R : Private_Reference) return String is
         type Node_Access is access all Managed_Node_Base'Class;
      begin
         if Deref (R) /= null then
            return
              Ada.Tags.External_Tag (Node_Access (Deref (R)).all'Tag) & "@" &
              Private_Reference'Image (R);
         else
            return "@" & Private_Reference'Image (R);
         end if;
      end Image;

      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference is

         Guard    : constant PTB.Guard_Type := PTB.Hire_Guard;
         Node_Ref : Private_Reference;
         State    : MM_Magic_Type;
      begin
         Dereference :
         loop
            Node_Ref := To_Private_Reference (Link.all);

            exit Dereference when To_Node_Access (Node_Ref) = null;

            PTB.Post_Guard (Guard,
                            Managed_Node_Access (To_Node_Access (Node_Ref)));

            if Integrity_Checking then
               if To_Private_Reference (Link.all) = Node_Ref then
                  declare
                     use type Reference_Count;
                     Node_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (To_Node_Access (Node_Ref));
                     --  Base type view of the node.
                  begin
                     State := Node_Base.MM_Magic;
                  end;
               end if;
               Primitives.Membar;
               --  The integrity check must precede the ordinary
               --  loop exit test.
            end if;

            if To_Private_Reference (Link.all) = Node_Ref then
               declare
                  use type Reference_Count;
                  Node_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (Node_Ref));
                  --  Base type view of the node.
                  Old_RC    : Reference_Count := Node_Base.MM_RC;
               begin
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

         if Integrity_Checking then
            if Unmark (Node_Ref) /= Null_Reference and then
              State /= MM_Live then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "lock_free_reference_counting.adb: " &
                  "Dereferenced the non-exsisting node " &
                  Image (Managed_Node_Access (To_Node_Access (Node_Ref))) &
                  "!");
            end if;
         end if;

         return Node_Ref;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
         use type Reference_Count;
      begin
         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
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
                        return Node_Access renames To_Node_Access;

      ----------------------------------------------------------------------
      function Deref (Node : in Private_Reference)
                     return Node_Access renames To_Node_Access;


      ----------------------------------------------------------------------
      function  Copy (Node : in Private_Reference) return Private_Reference is
         use type Reference_Count;
      begin
         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
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
         use type Reference_Count;
      begin
         --  Since we have dereferenced both Old_Value and New_Value they
         --  are guaranteed to have positive reference counts, regardless
         --  how the CAS goes.
         if
           Compare_And_Swap
           (Target    =>
              To_Shared_Reference_Base_Access (Link.all'Unchecked_Access),
            Old_Value => (Ref => Shared_Reference_Base_Impl (Old_Value)),
            New_Value => (Ref => Shared_Reference_Base_Impl (New_Value)))
         then
            if To_Node_Access (New_Value) /= To_Node_Access (Old_Value) then

               if To_Node_Access (New_Value) /= null then
                  declare
                     New_Value_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (To_Node_Access (New_Value));
                     --  Base type view of the node.
                  begin
                     Fetch_And_Add (New_Value_Base.MM_RC'Access, 1);
                  end;
               end if;

               if To_Node_Access (Old_Value) /= null then
                  declare
                     Old_Value_Base : constant Managed_Node_Access :=
                       Managed_Node_Access (To_Node_Access (Old_Value));
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
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
         use type Reference_Count;
         Old : constant Node_Access :=
           To_Node_Access (Unmark (To_Private_Reference (Link.all)));
      begin
         To_Shared_Reference_Base_Access (Link.all'Unchecked_Access).all :=
           (Ref => Shared_Reference_Base_Impl (Node));

         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
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

      exception
         when E : Storage_Error =>
            Ada.Text_IO.Put_Line ("raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E) &
                                  "   " &
                                  " in Lock_Free_Reference_Counting.Store");
      end Store;

      ----------------------------------------------------------------------
      function Create return Private_Reference is
         UNode : constant User_Node_Access := new Managed_Node;
         Node  : constant Node_Access      := UNode.all'Unchecked_Access;
      begin
         if Collect_Statistics then
            Fetch_And_Add (No_Nodes_Created'Access, 1);
         end if;

         return To_Private_Reference (Node);
      end Create;

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference) is
      begin
         Node := Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node := Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node and Mark_Mask) = Private_Reference'(1);
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean is
      begin
         return (To_Private_Reference (Node) and Mark_Mask) =
           Private_Reference'(1);
      end Is_Marked;

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";

      ----------------------------------------------------------------------
      ----------------------------------------------------------------------
      function Unsafe_Read (Link : access Shared_Reference)
                           return Unsafe_Reference_Value is
      begin
         return Unsafe_Reference_Value (To_Private_Reference (Link.all));
      end Unsafe_Read;

      ----------------------------------------------------------------------
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Unsafe_Reference_Value;
                                  New_Value : in Private_Reference)
                                 return Boolean is
      begin
         --  Since we have not dereferenced Old_Value it is not
         --  guaranteed to have a positive reference count.
         --  However, since we just successfully removed a link to that
         --  node it's reference count certainly should not be zero.
         return Compare_And_Swap (Link      => Link,
                                  Old_Value => Private_Reference (Old_Value),
                                  New_Value => New_Value);
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Unsafe_Reference_Value;
                                  New_Value : in     Private_Reference) is
      begin
         --  Since we have not dereferenced Old_Value it is not
         --  guaranteed to have a positive reference count.
         --  However, since we just successfully removed a link to that
         --  node it's reference count certainly should not be zero.
         if
           Compare_And_Swap (Link,
                             Private_Reference (Old_Value),
                             New_Value)
         then
            null;
         end if;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Unsafe_Reference_Value;
                                  New_Value : in Unsafe_Reference_Value)
                                 return Boolean is
      begin
         --  Since we have not dereferenced Old_Value it is not
         --  guaranteed to have a positive reference count.
         --  However, since we just successfully removed a link to that
         --  node it's reference count certainly should not be zero.
         return Compare_And_Swap (Link      => Link,
                                  Old_Value => Private_Reference (Old_Value),
                                  New_Value => Private_Reference (New_Value));
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Unsafe_Reference_Value;
                                  New_Value : in     Unsafe_Reference_Value) is
      begin
         --  Since we have not dereferenced Old_Value it is not
         --  guaranteed to have a positive reference count.
         --  However, since we just successfully removed a link to that
         --  node it's reference count certainly should not be zero.
         if
           Compare_And_Swap (Link,
                             Private_Reference (Old_Value),
                             Private_Reference (New_Value))
         then
            null;
         end if;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Unsafe_Reference_Value)
                          return Boolean is
      begin
         return (Private_Reference (Node) and Mark_Mask) =
           Private_Reference'(1);
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Unsafe_Reference_Value)
                          return Unsafe_Reference_Value is
      begin
         return Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      function "=" (Val : in     Unsafe_Reference_Value;
                    Ref : in     Private_Reference) return Boolean is
      begin
         return Ref = Private_Reference (Val);
      end "=";


      ----------------------------------------------------------------------
      function "=" (Ref : in     Private_Reference;
                    Val : in     Unsafe_Reference_Value) return Boolean is
      begin
         return Ref = Private_Reference (Val);
      end "=";

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Unsafe_Reference_Value) return Boolean is
      begin
         return To_Private_Reference (Link) = Private_Reference (Ref);
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Unsafe_Reference_Value;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Private_Reference (Ref);
      end "=";

      ----------------------------------------------------------------------
      ----------------------------------------------------------------------
      function To_Node_Access (X : Private_Reference)
                              return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Private_Reference,
                                          Node_Access);

      begin
         return To_Node_Access (X and Ref_Mask);
      end To_Node_Access;

      ----------------------------------------------------------------------
   end Operations;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
      use type Primitives.Unsigned_32;
   begin
      if Collect_Statistics then
         Ada.Text_IO.Put_Line
           ("Lock_Free_Reference_Counting.Print_Statistics:");
         Ada.Text_IO.Put_Line
           ("  #Created = " & Primitives.Unsigned_32'Image (No_Nodes_Created));
         Ada.Text_IO.Put_Line
           ("  #Reclaimed = " &
            Primitives.Unsigned_32'Image (No_Nodes_Reclaimed));
         Ada.Text_IO.Put_Line ("  #Not reclaimed = " &
                               Primitives.Unsigned_32'Image
                               (No_Nodes_Created - No_Nodes_Reclaimed));
      end if;
   end Print_Statistics;

   ----------------------------------------------------------------------------
   procedure Clean_And_Liberate (Node : in Managed_Node_Access) is

      -----------------------------------------------------------------
      function To_Managed_Node_Access (X : Shared_Reference_Base)
                                      return Managed_Node_Access;

      -----------------------------------------------------------------
      function To_Managed_Node_Access (X : Shared_Reference_Base)
                                      return Managed_Node_Access is

         function To_Managed_Node_Access is new
           Ada.Unchecked_Conversion (Shared_Reference_Base_Impl,
                                     Managed_Node_Access);

      begin
         return To_Managed_Node_Access (X.Ref and Ref_Mask);
      end To_Managed_Node_Access;

      -----------------------------------------------------------------
   begin
      if Integrity_Checking then
         if Node = null or else Node.MM_Magic /= MM_Live then
            Ada.Exceptions.Raise_Exception
              (Constraint_Error'Identity,
               "lock_free_reference_counting.adb: " &
               "Attempting to Clean_And_Liberate the already cleaned node " &
               Image (Node) & "!");
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
                       To_Managed_Node_Access (Refs (I).all);
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
               if VS (I) = null or else VS (I).MM_Magic /= MM_Reclaimable then
                  declare
                     MM_Magic : constant MM_Magic_Type := VS (I).MM_Magic;
                  begin
                     if MM_Magic = MM_Live then
                        Ada.Exceptions.Raise_Exception
                          (Constraint_Error'Identity,
                           "lock_free_reference_counting.adb: " &
                           "Attempt to reclaim the live node " &
                           Image (VS (I)) & "!");
                     elsif  MM_Magic = MM_Reclaimed then
                        Ada.Exceptions.Raise_Exception
                          (Constraint_Error'Identity,
                           "lock_free_reference_counting.adb: " &
                           "Attempt to reclaim the already reclaimed node " &
                           Image (VS (I)) & "!");
                     else
                        Ada.Exceptions.Raise_Exception
                          (Constraint_Error'Identity,
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
         if Collect_Statistics then
            Fetch_And_Add (No_Nodes_Reclaimed'Access, VS'Length);
         end if;
      end;
   end Clean_And_Liberate;

   ----------------------------------------------------------------------------
   function Image (Node : in Managed_Node_Access) return String is
      function To_Unsigned is new
        Ada.Unchecked_Conversion (Managed_Node_Access,
                                  Primitives.Standard_Unsigned);
      use type Primitives.Standard_Unsigned;
   begin
      return Primitives.Standard_Unsigned'Image (To_Unsigned (Node));
   end Image;

   ----------------------------------------------------------------------------
   procedure Finalize;

   procedure Finalize is
   begin
      if Collect_Statistics then
         Print_Statistics;
      end if;
   end Finalize;

   type Local_Action is access procedure;
   function Lope_Hole is new Ada.Unchecked_Conversion
     (Local_Action,
      NBAda.Internals.Cleanup_Tools.Action);

   Finally :
     NBAda.Internals.Cleanup_Tools.On_Exit (Lope_Hole (Finalize'Access));
--  NOTE: This is a really really dangerous idea!
--        Finally might be destroyed AFTER the node storage pool is destroyed!

end NBAda.Lock_Free_Reference_Counting;
