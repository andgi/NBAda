-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lockfree_reference_counting.adb
-- Description     : Lock-free reference counting.
-- Author          : Anders Gidenstam and Håkan Sundell
-- Created On      : Fri Nov 19 14:07:58 2004
-- $Id: nbada-lock_free_memory_reclamation.adb,v 1.1 2004/11/25 15:55:26 andersg Exp $
-------------------------------------------------------------------------------

with Primitives;
with Ada.Unchecked_Conversion;

package body Lockfree_Reference_Counting is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------
   subtype Processes is Process_Ids.Process_ID_Type;
   type    HP_Index  is new Integer range 1 .. Max_Number_Of_Dereferences;

   subtype Atomic_Node_Access is Shared_Reference;

   subtype Node_Count is Primitives.Unsigned_32;


   procedure Scan     (ID : in Processes);
   procedure Clean_Up (ID : in Processes);
   procedure Release_References
     (Node : access Reference_Counted_Node'Class);
   procedure Remove_Redundant_Ref_Chain
     (Node : access Reference_Counted_Node'Class);

   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add;
   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   Hazard_Pointer : array (Processes, HP_Index) of aliased Atomic_Node_Access;
   pragma Volatile (Hazard_Pointer);
   pragma Atomic_Components (Hazard_Pointer);

   --  Static process local storage.
   D_List  : array (Processes) of Node_Access;
   D_Count : array (Processes) of Node_Count := (others => 0);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function  Deref   (Link : access Shared_Reference) return Node_Access is
      ID    : constant Processes := Process_Ids.Process_ID;
      Index : HP_Index;
      Found : Boolean := False;
      Node  : Node_Access;
   begin
      --  Find a free hazard pointer.
      for I in Hazard_Pointer'Range (2) loop
         if Hazard_Pointer (ID, I) = null then
            Index := I;
            Found := True;
            exit;
         end if;
      end loop;
      --  Dereference node iff there is a free hazard pointer.
      if not Found then
         raise Constraint_Error;
      else
         loop
            Node := Node_Access (Link.all);
            Hazard_Pointer (ID, Index) := Atomic_Node_Access (Node);
            exit when Node_Access (Link.all) = Node;
         end loop;
      end if;
      return Node;
   end Deref;

   ----------------------------------------------------------------------------
   procedure Release (Node : in Node_Access) is
      ID : constant Processes := Process_Ids.Process_ID;
   begin
      --  Find and reset hazard pointer.
      for I in Hazard_Pointer'Range (2) loop
         if Hazard_Pointer (ID, I) = Atomic_Node_Access (Node) then
            Hazard_Pointer (ID, I) := null;
            exit;
         end if;
      end loop;
   end Release;

   ----------------------------------------------------------------------------
   procedure Delete  (Node : in Node_Access) is
      use type Node_Count;
      ID : constant Processes := Process_Ids.Process_ID;
   begin
      Node.MM_Next := Atomic_Node_Access (D_List (ID));
      D_List  (ID) := Node;
      D_Count (ID) := D_Count (ID) + 1;
      if D_Count (ID) <= Threshold_1 then
         Clean_Up (ID);
      end if;
      if D_Count (ID) <= Threshold_2 then
         Scan (ID);
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   procedure Store   (Link : access Shared_Reference;
                      Node : in Node_Access) is
      use type Reference_Count;
      Old : constant Node_Access := Node_Access (Link.all);
   begin
      Link.all := Shared_Reference (Node);
      if Old /= null then
         Fetch_And_Add (Old.MM_RC'Access, -1);
      end if;
      if Node /= null then
         Fetch_And_Add (Node.MM_RC'Access, 1);
      end if;
   end Store;

   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean is
      use type Reference_Count;
   begin
      if Compare_And_Swap (Link, Old_Value, New_Value) then
         if Old_Value /= null then
            Fetch_And_Add (Old_Value.MM_RC'Access, -1);
         end if;
         if New_Value /= null then
            Fetch_And_Add (New_Value.MM_RC'Access, 1);
         end if;
         return True;
      end if;
      return False;
   end Compare_And_Swap;

   ----------------------------------------------------------------------------
   --
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan     (ID : in Processes) is
      use type Reference_Count;

      type Unsigned_Int is mod 2**32;
      function To_Unsigned_Int is new Ada.Unchecked_Conversion (Node_Access,
                                                                Unsigned_Int);

      type Node_Access_Array is array (Positive range <>) of Node_Access;

      type HP_Set (Size : Natural) is
         record
            Set     : Node_Access_Array (1 .. Size);
            Current : Natural := 0;
         end record;

      function "<" (Left, Right : Node_Access) return Boolean is
      begin
         return To_Unsigned_Int (Left) < To_Unsigned_Int (Right);
      end "<";

      procedure Insert (Set  : in out HP_Set;
                        Node : in     Node_Access) is
      begin
         if Set.Current < Set.Size then
            Set.Current := Set.Current + 1;
            Set.Set (Set.Current) := Node;
         else
            raise Constraint_Error;
         end if;
      end Insert;

--       function Member (Set  : in HP_Set;
--                        Node : in Node_Access) return Boolean is
--       begin
--          if Set.Current >= Set.Size then
--             declare
--                High : Natural := HP_Set.Set'Last;
--                Low  : Natural := HP_Set.Set'First;
--             begin
--                while High > Low loop
--                   if Set.Set (High)
--                end loop;
--             end;
--          else
--             raise Constraint_Error;
--          end if;
--       end Member;
      function Member (Set  : in HP_Set;
                       Node : in Node_Access) return Boolean is
      begin
         return False;
      end Member;

      P_Set       : HP_Set (Natural (Process_Ids.Max_Number_Of_Processes *
                                     Max_Number_Of_Dereferences));
      New_D_List  : Node_Access := null;
      New_D_Count : Node_Count  := 0;
      Node        : Node_Access;
   begin
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Node := Node_Access (Hazard_Pointer (P, I));
            if Node /= null then
               Insert (P_Set, Node);
            end if;
         end loop;
      end loop;

      --  Prepare P_Set for lookups.
      --Sort (P_Set);

      while D_List (ID) /= null loop
         Node        := D_List (ID);
         D_List (ID) := Node_Access (Node.MM_Next);
         if Node.MM_RC > 0 or Member (P_Set, Node) then
            Node.MM_Next := Atomic_Node_Access (New_D_List);
            New_D_List   := Node;
            New_D_Count  := New_D_Count + 1;
         else
            null;
            Release_References (Node);
            --  Reclaim node storage.
         end if;
      end loop;
      D_List  (ID) := New_D_List;
      D_Count (ID) := New_D_Count;
   end Scan;

   ----------------------------------------------------------------------------
   procedure Clean_Up (ID : in Processes) is
      use type Processes;
   begin
      for P in Hazard_Pointer'Range (1) loop
         if P /= ID then
            for I in Hazard_Pointer'Range (2) loop
               declare
                  Node : constant Node_Access
                    := Deref (Hazard_Pointer (P, I)'Access);
               begin
                  if Node /= null then
                     Remove_Redundant_Ref_Chain (Node);
                     Release (Node);
                  end if;
               end;
            end loop;
         end if;
      end loop;
   end Clean_Up;

   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Release_References
     (Node : access Reference_Counted_Node'Class) is

      procedure Decrease_Reference_Count (Link_X : access Shared_Reference) is
         use type Reference_Count;
      begin
         if Link_X.all /= null then
            Fetch_And_Add (Link_X.all.MM_RC'Access, -1);
         end if;
      end Decrease_Reference_Count;

   begin
      For_Each_Reference_Of (Node,
                             Decrease_Reference_Count'Unrestricted_Access);
   end Release_References;

   ----------------------------------------------------------------------------
   procedure Remove_Redundant_Ref_Chain
     (Node : access Reference_Counted_Node'Class) is

      procedure Unlink (Link_X : access Shared_Reference) is
         use type Reference_Count;
      begin
         loop
            declare
               Node1 : constant Node_Access := Deref (Link_X);
               Node2 : Node_Access;
            begin
               if Is_Deleted (Node1) then
                  null;
               end if;
            end;
         end loop;
      end Unlink;

   begin
      null;
--      For_Each_Reference_Of (Node,
--                             Unlink'Unrestricted_Access);
   end Remove_Redundant_Ref_Chain;

end Lockfree_Reference_Counting;
