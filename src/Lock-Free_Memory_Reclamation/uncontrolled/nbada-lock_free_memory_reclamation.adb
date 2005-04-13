-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : lockfree_reference_counting.adb
-- Description     : Lock-free reference counting.
-- Author          : Anders Gidenstam and Håkan Sundell
-- Created On      : Fri Nov 19 14:07:58 2004
-- $Id: nbada-lock_free_memory_reclamation.adb,v 1.2 2005/04/13 16:51:19 anders Exp $
-------------------------------------------------------------------------------

with Primitives;
with Hash_Tables;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Lockfree_Reference_Counting is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------
   subtype Processes  is Process_Ids.Process_ID_Type;
   type    HP_Index   is new Integer range 1 .. Max_Number_Of_Dereferences;
   type    Node_Index is new Natural range 0 .. Threshold_1;
   subtype Valid_Node_Index is Node_Index range 1 .. Node_Index'Last;

   subtype Atomic_Node_Access is Shared_Reference;

   subtype Node_Count  is Natural;
   subtype Claim_Count is Primitives.Unsigned_32;


   procedure Scan           (ID : in Processes);
   procedure Clean_Up_Local (ID : in Processes);
   procedure Clean_Up_All   (ID : in Processes);

   function Hash_Ref (Ref  : in Node_Access;
                      Size : in Natural) return Natural;

   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add;
   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference);
   procedure Free is
      new Ada.Unchecked_Deallocation (Reference_Counted_Node'Class,
                                      Node_Access);

   package HP_Sets is new Hash_Tables (Node_Access, "=", Hash_Ref);

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
   D_List   : array (Processes) of Node_Index := (others => 0);
   D_Count  : array (Processes) of Node_Count := (others => 0);
   DL_Nexts : array (Processes, Valid_Node_Index) of Node_Index;
   pragma Atomic_Components (DL_Nexts);

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
      --  Find and clear hazard pointer.
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
      Index : Node_Index;
   begin
      Release (Node);
      Node.MM_Del   := True;
      Node.MM_Trace := False;

      --  Find a free index in DL_Nodes.
      for I in DL_Nodes'Range (2) loop
         if DL_Nodes (ID, I) = null then
            Index := I;
         end if;
      end loop;
      DL_Done  (ID, Index) := False;
      DL_Nodes (ID, Index) := Atomic_Node_Access (Node);
      DL_Nexts (ID, Index) := D_List (ID);

      D_List  (ID) := Index;
      D_Count (ID) := D_Count (ID) + 1;
      loop
         if D_Count (ID) = Threshold_1 then
            Clean_Up_Local (ID);
         end if;
         if D_Count (ID) >= Threshold_2 then
            Scan (ID);
         end if;
         if D_Count (ID) = Threshold_1 then
            Clean_Up_All (ID);
         else
            exit;
         end if;
      end loop;
   end Delete;

   ----------------------------------------------------------------------------
   function  Compare_And_Swap (Link      : access Shared_Reference;
                               Old_Value : in Node_Access;
                               New_Value : in Node_Access)
                              return Boolean is
      use type Reference_Count;
   begin
      if Compare_And_Swap (Link, Old_Value, New_Value) then
         if New_Value /= null then
            Fetch_And_Add (New_Value.MM_RC'Access, 1);
            New_Value.MM_Trace := False;
         end if;
         if Old_Value /= null then
            Fetch_And_Add (Old_Value.MM_RC'Access, -1);
         end if;
         return True;
      end if;
      return False;
   end Compare_And_Swap;

   ----------------------------------------------------------------------------
   procedure Store   (Link : access Shared_Reference;
                      Node : in Node_Access) is
      use type Reference_Count;
      Old : constant Node_Access := Node_Access (Link.all);
   begin
      Link.all := Shared_Reference (Node);
      if Node /= null then
         Fetch_And_Add (Node.MM_RC'Access, 1);
         Node.MM_Trace := False;
      end if;
      if Old /= null then
         Fetch_And_Add (Old.MM_RC'Access, -1);
      end if;
   end Store;

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Scan (ID : in Processes) is
      use type Reference_Count;
      use HP_Sets;

      P_Set : HP_Sets.Hash_Table
        (Size => 2 * Natural (Process_Ids.Max_Number_Of_Processes *
                              Max_Number_Of_Dereferences));
      Index       : Node_Index;
      Node        : Atomic_Node_Access;
      New_D_List  : Node_Index := 0;
      New_D_Count : Node_Count := 0;
   begin
      --  Set the trace bit on each deleted node with MM_RC = 0.
      Index := D_List (ID);
      while Index /= 0 loop
         Node := DL_Nodes (ID, Index);
         if Node.MM_RC = 0 then
            Node.MM_Trace := True;
            if Node.Mm_RC /= 0 then
               Node.MM_Trace := False;
            end if;
         end if;
         Index := DL_Nexts (ID, Index);
      end loop;

      --  Read all hazard pointers.
      for P in Hazard_Pointer'Range (1) loop
         for I in Hazard_Pointer'Range (2) loop
            Node := Hazard_Pointer (P, I);
            if Node /= null then
               declare
                  N : constant Node_Access := Node_Access (Node);
               begin
                  Insert (N, P_Set);
               end;
            end if;
         end loop;
      end loop;

      --  Attempt to reclaim nodes.
      while D_List (ID) /= 0 loop
         Index       := D_List (ID);
         Node        := DL_Nodes (ID, Index);
         D_List (ID) := DL_Nexts (ID, Index);

         if Node.MM_RC = 0 and Node.MM_Trace and
           not Member (Node_Access (Node), P_Set)
         then
            DL_Nodes (ID, Index) := null;
            if DL_Claims (ID, Index) = 0 then
               Dispose (Node, Concurrent => False);
               Free (Node_Access (Node));
            else
               Dispose (Node, Concurrent => True);
               DL_Done  (ID, Index) := True;
               DL_Nodes (ID, Index) := Node;
            end if;
         end if;

         DL_Nexts (ID, Index) := New_D_List;
         New_D_List   := Index;
         New_D_Count  := New_D_Count + 1;
      end loop;

      D_List  (ID) := New_D_List;
      D_Count (ID) := New_D_Count;
   end Scan;

   ----------------------------------------------------------------------------
   procedure Clean_Up_Local (ID : in Processes) is
      Index : Node_Index := D_List (ID);
      Node  : Atomic_Node_Access;
   begin
      while Index /= 0 loop
         Node  := DL_Nodes (ID, Index);
         Clean_Up (Node);
         Index := DL_Nexts (ID, Index);
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
                  Clean_Up (Node);
               end if;
               Fetch_And_Add (Target    => DL_Claims (P, Index)'Access,
                              Increment => -1);
            end if;
         end loop;
      end loop;
   end Clean_Up_All;

   ----------------------------------------------------------------------------
   function Hash_Ref (Ref  : in Node_Access;
                      Size : in Natural) return Natural is
      type Unsigned is mod 2**32;
      function To_Unsigned is new Ada.Unchecked_Conversion (Node_Access,
                                                            Unsigned);
   begin
      return Natural ((To_Unsigned (Ref)/4) mod Unsigned (Size));
   end Hash_Ref;

end Lockfree_Reference_Counting;
