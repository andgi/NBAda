-------------------------------------------------------------------------------
--  Lock-Free Priority Queues - An implementation of the lock-free skip-list
--                              algorithm by H. Sundell.
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
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_priority_queues.adb
--  Description     : Lock-free priority queue based on Håkan Sundell,
--                    "
--                    ",
--  Author          : Anders Gidenstam
--  Created On      : Wed Jun  6 15:51:34 2007
--  $Id: nbada-lock_free_priority_queues.adb,v 1.4 2007/10/31 17:21:54 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;

with Ada.Text_IO;

with Memory_Reclamation_Debug;

package body NBAda.Lock_Free_Priority_Queues is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
       (Block_Size => List_Node'Max_Size_In_Storage_Elements);

   type New_List_Node_Access is access List_Node;
   for New_List_Node_Access'Storage_Pool use Node_Pool;

   function Create_List_Node is new LFMR_Ops.Create (New_List_Node_Access);

   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Atomic_Boolean);

   Next_Level : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (Next_Level);

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   function Read_Next (Node  : access List_Node_Access;
                       Level : in     List_Level;
                       Queue : in     Priority_Queue_Type)
                      return List_Node_Access;
   --  After the call both Node and the returned node access must be releasd.

   procedure Help_Delete (Node  : in out List_Node_Access;
                          Level : in     List_Level;
                          Queue : in     Priority_Queue_Type);
   --  After the call Node must be released.

   procedure Scan_For_Key (Previous : access List_Node_Access;
                           Next     :    out List_Node_Access;
                           Level    : in     List_Level;
                           Key      : in     Element_Type;
                           Queue    : in     Priority_Queue_Type);
   --  After the call Previous and Next must be released.

   procedure Remove_Node (Node     : in     List_Node_Access;
                          Previous : access List_Node_Access;
                          Level    : in     List_Level;
                          Queue    : in     Priority_Queue_Type);
   --

   function Same_Node (Left, Right : List_Node_Access)
                      return Boolean;
   pragma Inline (Same_Node);

   function Same_Node (Left  : List_Node_Access;
                       Right : List_Node_Reference)
                      return Boolean;
   pragma Inline (Same_Node);

   function Random_Level return List_Level;

   --  Debugging stuff.

   package MR_Debug is new Memory_Reclamation_Debug
     (Private_Reference          => List_Node_Access,
      Max_Number_Of_Dereferences => 10,
      Process_Ids                => Process_Ids,
      Same_Node                  => Same_Node,
      Image                      => LFMR_Ops.Image);
   use MR_Debug;

   function Dereference (Link  : access List_Node_Reference;
                         Where : in String)
                        return List_Node_Access;
   --  Debug wrapper for Dereference.
   procedure Release (Node  : List_Node_Access;
                      Where : String);
   --  Debug wrapper for Release.


   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Initialize (Queue : in out Priority_Queue_Type) is
      use LFMR_Ops;
      Head : constant List_Node_Access := Create_List_Node;
      Tail : constant List_Node_Access := Create_List_Node;
   begin
      Add_Reference (Head, "Initialize: Head");
      Add_Reference (Tail, "Initialize: Tail");

      "+" (Head).Max_Level := Max_Levels;
      "+" (Tail).Max_Level := Max_Levels;
      for I in  "+" (Head).Next'Range loop
         Store ("+" (Head).Next (I)'Access, Tail);
      end loop;
      Store (Queue.Head'Access, Head);
      Store (Queue.Tail'Access, Tail);
      Release (Head, "Initialize: Head R1");
      Release (Tail, "Initialize: Tail R1");
      Verify_Quiescent ("Initialize");
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Insert  (Into    : in out Priority_Queue_Type;
                      Element : in     Element_Type) is
      use LFMR_Ops;
      New_Node   : List_Node_Access := Create_List_Node;
      Level      : constant List_Level := Random_Level;
      Saved_Node : array (List_Level range 1 .. List_Level'Last) of
        aliased List_Node_Access;
   begin
      Add_Reference (New_Node, "Insert: New Node");

      "+" (New_Node).Max_Level := Level;
      "+" (New_Node).Value     := Element;

      declare
         Before, After : aliased List_Node_Access;
      begin
         Before := Dereference (Into.Head'Access, "Insert: Before 1");
         for I in reverse List_Level'(2) .. List_Level'Last - 1 loop
            Scan_For_Key (Previous => Before'Access,
                          Next     => After,
                          Level    => I,
                          Key      => Element,
                          Queue    => Into);
            Release (After, "Insert: After R1");
            if I <= "+" (New_Node).Max_Level then
               Saved_Node (I) := Copy (Before);
               Add_Reference (Saved_Node (I),
                              "Insert: Saved_Node(" &
                              List_Level'Image (I) &
                              ")");
            end if;
         end loop;

         --  Insert at level 1.
         Insert : loop
            Scan_For_Key (Previous => Before'Access,
                          Next     => After,
                          Level    => 1,
                          Key      => Element,
                          Queue    => Into);

            declare
               After_Value : constant Element_Type := "+" (After).Value;
            begin
               if
                 not Boolean ("+" (After).Deleted) and then
                 After_Value = Element
               then
                  for I in 1 .. "+" (New_Node).Max_Level loop
                     Release (Saved_Node (I),
                              "Insert: Saved_Node(" &
                              List_Level'Image (I) &
                              ") R1");
                  end loop;
                  Release (Before, "Insert: Before R2");
                  Release (After, "Insert: After R2");
                  Release (New_Node, "Insert: New_Node R2");

                  --  What else can be done? This should never happen in
                  --  a priority queue!!! Break ties!
                  Verify_Quiescent ("Insert -> Already_Present");
                  raise Already_Present;

               else

                  Store ("+" (New_Node).Next (1)'Access, After);
                  Release (After, "Insert: After R3");

                  if
                    Compare_And_Swap
                    (Link      => "+" (Before).Next (1)'Access,
                     Old_Value => After,
                     New_Value => New_Node)
                  then
                     Release (Before, "Insert: Before R3");
                     exit Insert;

                  else
                     --  Back off.
                     delay 0.0;
                  end if;

               end if;
            end;
         end loop Insert;
      end;

      --  FIXME: Which local references should exist at this point?

      for I in 2 .. Level loop
         "+" (New_Node).Valid_Level := I;

         declare
            Before : aliased List_Node_Access := Saved_Node (I);
            After  : aliased List_Node_Access;
         begin
            Insert_At_Level_I : loop
               Scan_For_Key (Previous => Before'Access,
                             Next     => After,
                             Level    => I,
                             Key      => Element,
                             Queue    => Into);
               Store ("+" (New_Node).Next (I)'Access, After);
               Release (After, "Insert: After R4");

               if Boolean ("+" (New_Node).Deleted) or else
                 Compare_And_Swap
                 (Link      => "+" (Before).Next (I)'Access,
                  Old_Value => After,
                  New_Value => New_Node)
               then
                  Release (Before, "Insert: Before R4");
                  exit Insert_At_Level_I;

               else
                  --  Back off.
                  delay 0.0;
               end if;

            end loop Insert_At_Level_I;
         end;
      end loop;

      "+" (New_Node).Valid_Level := Level + 1;  --  !!??
      if "+" (New_Node).Deleted then
         Help_Delete (New_Node, 1, Into);
      end if;
      Release (New_Node, "Insert: New_Node R5");
      Verify_Quiescent ("Insert -> Done");
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete_Min (From    : in out Priority_Queue_Type;
                         Element :    out Element_Type) is
   begin
      Element := Delete_Min (From.Mutable.Self);
   end Delete_Min;

   ----------------------------------------------------------------------------
   function  Delete_Min (From : in Priority_Queue_Type)
                        return Element_Type is
   begin
      return Delete_Min (From.Mutable.Self);
   end Delete_Min;

   ----------------------------------------------------------------------------
   function  Delete_Min (From : access Priority_Queue_Type)
                        return Element_Type is
      use LFMR_Ops;
      Previous : aliased List_Node_Access :=
        Dereference (From.Mutable.Self.Head'Access, "Delete_Min: Previous 1");
      Node : aliased List_Node_Access;
      Value : Element_Type;
   begin
      --  Mark the affected node as deleted at level 1.
      Mark_Deleted : loop
         Node := Read_Next (Previous'Access, 1, From.all);
         if Same_Node (Node, From.Tail) then
            Release (Previous, "Delete_Min: Previous R1");
            Release (Node, "Delete_Min: Node R1");
            Verify_Quiescent ("Delete_Min -> Empty");
            raise Queue_Empty;
         end if;

         Value := "+" (Node).Value;
         if Same_Node (Node, "+" (Previous).Next (1)) then
            if not "+" (Node).Deleted then
               if
                 Compare_And_Swap
                 (Target    => "+" (Node).Deleted'Access,
                  Old_Value => False,
                  New_Value => True)
               then
                  Store ("+" (Node).Previous'Access, Previous);
                  exit Mark_Deleted;
               else
                  --  Retry. Starting from the top of Mark_Deleted
                  --  is a bit unnecessary.
                  Release (Node, "Delete_Min: Node R2");
               end if;
            else
               if "+" (Node).Deleted then
                  Help_Delete (Node, 1, From.all);
               end if;

               Release (Previous, "Delete_Min: Previous R2");
               Previous := Node;
               Node     := Null_Reference;
            end if;
         else
            Release (Node, "Delete_Min: Node R3");
         end if;
      end loop Mark_Deleted;

      --  Mark Node at all higher levels.
      for I in 1 .. "+" (Node).Max_Level loop
         Mark_Level : loop
            declare
               Node_Next : constant LFMR_Ops.Unsafe_Reference_Value :=
                 Unsafe_Read ("+" (Node).Next (I)'Access);
            begin
               exit Mark_Level when Is_Marked (Node_Next) or else
                 Compare_And_Swap (Link      => "+" (Node).Next (I)'Access,
                                   Old_Value => Node_Next,
                                   New_Value => Mark (Node_Next));
            end;
         end loop Mark_Level;
      end loop;

      --  Dechain Node.
      for I in reverse 1 .. "+" (Node).Max_Level loop
         Remove_Node (Node, Previous'Access, I, From.all);
      end loop;
      Release (Previous, "Delete_Min: Previous R4");
      Delete (Node);
      Remove_Reference (Node,  "Delete_Min: Delete Node");

      Verify_Quiescent ("Delete_Min -> Value");
      return Value;
   end Delete_Min;

   ----------------------------------------------------------------------------
   procedure Verify (Queue : in out Priority_Queue_Type;
                     Print : in     Boolean := False) is
      use LFMR_Ops;

      function Image (Node  : List_Node_Access) return String;

      ----------------------------------------------------------------------
      function Image (Node  : List_Node_Access) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (List_Node_Reference,
                                          Primitives.Standard_Unsigned);
         function To_Unsigned is
            new Ada.Unchecked_Conversion (LFMR_Ops.Node_Access,
                                          Primitives.Standard_Unsigned);
         use Primitives;
      begin
         return
           Standard_Unsigned'Image (To_Unsigned ("+" (Node))) &
           "@(" &
           "Next (1) => " &
           Standard_Unsigned'Image (To_Unsigned ("+" (Node).Next (1))) & ", " &
           "Previous => " &
           Standard_Unsigned'Image (To_Unsigned ("+" (Node).Previous)) &
           ")";
      end Image;

      ----------------------------------------------------------------------
      Node : List_Node_Access := Dereference (Queue.Head'Access);
      Prev : List_Node_Access := Null_Reference;
      N    : Natural := 0;
   begin
      loop
         if Print then
            Ada.Text_IO.Put_Line (Image (Node));
         end if;

         Release (Prev);
         Prev := Node;
         Node := Dereference ("+" (Node).Next (1)'Access);

         N := N + 1;

         exit when "+" (Node) = null;
      end loop;

      Ada.Text_IO.Put_Line ("lock_free_priority_queue.adb: Verify: " &
                            "Total number of nodes = " &
                            N'Img);
   end Verify;

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Read_Next (Node  : access List_Node_Access;
                       Level : in     List_Level;
                       Queue : in     Priority_Queue_Type)
                      return List_Node_Access is
      use LFMR_Ops;
   begin
      Enter;
      if "+" (Node.all).Deleted then
         Help_Delete (Node.all, Level, Queue);
      end if;
      loop
         declare
            Node_Next : constant List_Node_Access :=
              Dereference ("+" (Node.all).Next (Level)'Access,
                           "Read_Next: Node_Next");
         begin
            if Is_Marked (Node_Next) then
               Help_Delete (Node.all, Level, Queue);
            else
               Exit_Quiescent ("Read_Next",
                               Except => (Node.all, Node_Next));
               return Node_Next;
            end if;
         end;
      end loop;

   end Read_Next;

   ----------------------------------------------------------------------------
   procedure Help_Delete (Node  : in out List_Node_Access;
                          Level : in     List_Level;
                          Queue : in     Priority_Queue_Type) is
      use LFMR_Ops;
   begin
      for I in Level .. "+" (Node).Max_Level loop
         Mark_Level : loop
            declare
               Node_Next : constant Unsafe_Reference_Value :=
                 Unsafe_Read ("+" (Node).Next (I)'Access);
            begin
               exit Mark_Level when
                 Is_Marked (Node_Next) or else
                 Compare_And_Swap (Link      => "+" (Node).Next (I)'Access,
                                   Old_Value => Node_Next,
                                   New_Value => Mark (Node_Next));
            end;
         end loop Mark_Level;
      end loop;

      declare
         Previous : aliased List_Node_Access :=
           Dereference ("+" (Node).Previous'Access,
                        "Help_Delete: Previous 1");
      begin
         if
           Same_Node (Previous, Null_Reference) or
           else Level > "+" (Previous).Valid_Level
         then
            Release (Previous, "Help_Delete: Previous R1");
            Previous := Dereference (Queue.Mutable.Self.Head'Access,
                                     "Help_Delete: Previous 2");
            for I in reverse List_Level'Last .. Level loop
               declare
                  Tmp : List_Node_Access;
               begin
                  Scan_For_Key (Previous => Previous'Access,
                                Next     => Tmp,
                                Level    => I,
                                Key      => "+" (Node).Value,
                                Queue    => Queue);
                  Release (Tmp, "Help_Delete: Tmp R1");
               end;
            end loop;
         end if;
         Remove_Node (Node, Previous'Access, Level, Queue);
         Release (Node, "Help_Delete: Node R1");
         Node := Previous;
      end;
   end Help_Delete;

   ----------------------------------------------------------------------------
   procedure Scan_For_Key (Previous : access List_Node_Access;
                           Next     :    out List_Node_Access;
                           Level    : in     List_Level;
                           Key      : in     Element_Type;
                           Queue    : in     Priority_Queue_Type) is
      use LFMR_Ops;
   begin
      Enter;
      loop
         Next := Read_Next (Previous, Level, Queue);

         if Same_Node (Next, Queue.Tail) then
            exit;
         end if;

         if "+" (Next).Value < Key then
            Release (Previous.all, "Scan_For_Key: Previous R1");
            Previous.all := Next;
         else
            exit;
         end if;
      end loop;
      Exit_Quiescent ("Scan_For_Key",
                      Except => (Previous.all, Next));
   end Scan_For_Key;

   ----------------------------------------------------------------------------
   procedure Remove_Node (Node     : in     List_Node_Access;
                          Previous : access List_Node_Access;
                          Level    : in     List_Level;
                          Queue    : in     Priority_Queue_Type) is
      use LFMR_Ops;
      --  Håkans code does something strange here that will prevent
      --  traversal of logically deleted nodes. I think that
      --  is undesirable for efficiency.
   begin
      loop
         declare
            After  : List_Node_Access;
         begin
            Scan_For_Key (Previous => Previous,
                          Next     => After,
                          Level    => Level,
                          Key      => "+" (Node).Value,
                          Queue    => Queue);
            if not Same_Node (After, Node) then
               Release (After, "Remove_Node: After R1");
               return;
            else
               Release (After, "Remove_Node: After R1");
               declare
                  Node_Next : constant List_Node_Access :=
                    Dereference ("+" (Node).Next (Level)'Access,
                                 "Remove_Node: Node_Next 1");
               begin
                  if Compare_And_Swap
                    (Link      => "+" (Previous.all).Next (Level)'Access,
                     Old_Value => Unmark (Node),
                     New_Value => Unmark (Node_Next))
                    or else Is_Marked
                    (Unsafe_Read ("+" (Previous.all).Next (Level)'Access))
                  then
                     Release (Node_Next, "Remove_Node: Node_Next R1");
                     return;
                  end if;
                  Release (Node_Next, "Remove_Node: Node_Next R2");
               end;
            end if;
         end;
         --  Back off.
         delay 0.0;
      end loop;
   end Remove_Node;

   ----------------------------------------------------------------------------
   procedure Dispose  (Node       : access List_Node;
                       Concurrent : in     Boolean) is
      use LFMR_Ops;
   begin
      Node.Valid_Level := 0;
      if not Concurrent then
         Store (Node.Previous'Access, Null_Reference);
         for I in reverse Node.Next'Range loop
            Store (Node.Next (I)'Access, Null_Reference);
         end loop;
      else
         declare
            Tmp : LFMR_Ops.Unsafe_Reference_Value;
         begin
            loop
               Tmp := Unsafe_Read (Node.Previous'Access);
               exit when Compare_And_Swap (Link      => Node.Previous'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;

            for I in reverse Node.Next'Range loop
               loop
                  Tmp := Unsafe_Read (Node.Next (I)'Access);
                  exit when Compare_And_Swap
                    (Link      => Node.Next (I)'Access,
                     Old_Value => Tmp,
                     New_Value => Null_Reference);
               end loop;
            end loop;
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------------
   procedure Clean_Up (Node : access List_Node) is
   begin
      null;
   end Clean_Up;

   ----------------------------------------------------------------------------
   function All_References (Node : access List_Node)
                           return LFMR.Reference_Set is
      type Link_Access is access all List_Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  LFMR.Shared_Reference_Base_Access);

      Result : LFMR.Reference_Set (1 .. Integer (Node.Max_Level + 1));

   begin
      for I in reverse 1 .. Node.Max_Level loop
         Result (Integer (I)) :=
           To_Shared_Reference_Access (Node.Next (I)'Access);
      end loop;
      Result (Result'Last) :=
        To_Shared_Reference_Access (Node.Previous'Access);

      return Result;
   end All_References;

   ----------------------------------------------------------------------------
   procedure Free     (Node : access List_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (List_Node,
                                    New_List_Node_Access);

      function To_New_List_Node_Access is new
        Ada.Unchecked_Conversion (LFMR_Ops.Node_Access,
                                  New_List_Node_Access);

      X : New_List_Node_Access :=
        To_New_List_Node_Access (LFMR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   function Random_Level return List_Level is
      --  NOTE: This does not give the required level distribution yet.
      use type Primitives.Unsigned_32;
   begin
      return
        List_Level (1 + (Primitives.Fetch_And_Add_32 (Next_Level'Access, 1)
                         mod (Max_Levels - 1)));
   end Random_Level;

   ----------------------------------------------------------------------------
   function Same_Node (Left, Right : List_Node_Access)
                      return Boolean is
      use LFMR_Ops;
   begin
      return Unmark (Left) = Unmark (Right);
   end Same_Node;

   ----------------------------------------------------------------------------
   function Same_Node (Left  : List_Node_Access;
                       Right : List_Node_Reference)
                      return Boolean is
      use LFMR_Ops;
   begin
      return Unmark (Left) = Right;
   end Same_Node;

   ----------------------------------------------------------------------------
   function Dereference (Link  : access List_Node_Reference;
                         Where : in String)
                        return List_Node_Access is
      use LFMR_Ops;
   begin
      declare
         Tmp : constant List_Node_Access := LFMR_Ops.Dereference (Link);
      begin
         if "+" (Tmp) /= null then
            --  Don't track null references.
            Add_Reference (Tmp, Where);
         end if;
         return Tmp;
      end;

   exception
      when Constraint_Error =>
         Debug_IO.Put_Line ("Dereference at " & Where &
                            " raised Constraint_Error.");
         Dump_Local_References (Where);
         raise;
   end Dereference;

   ----------------------------------------------------------------------------
   procedure Release (Node  : List_Node_Access;
                      Where : String) is
      use LFMR_Ops;
   begin
      if "+" (Node) /= null then
         Remove_Reference (Node, Where);
      end if;
      LFMR_Ops.Release (Node);
   end Release;


end NBAda.Lock_Free_Priority_Queues;
