-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.adb
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 12:15:16 2002
-- $Id: nbada-lock_free_bounded_priority_queue.adb,v 1.3 2003/02/14 10:46:29 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Non_Blocking_Priority_Queue is

   ----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation (Heap_Entry,
                                                     Heap_Entry_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Heap_Status,
                                                     Heap_Status_Access);
   procedure Free is new Ada.Unchecked_Deallocation (Element_Type,
                                                     Element_Access);

   ----------------------------------------------------------------------------
   function CAS is new
     Primitives.Boolean_Compare_And_Swap_32 (Heap_Status_Access);
   function CAS is new
     Primitives.Boolean_Compare_And_Swap_32 (Heap_Entry_Access);

   ----------------------------------------------------------------------------
   -- Internal functions.

   subtype Real_Operation_Type is Operation_Type range INSERT .. DELETE_MIN;
   -- Enter preliminary phase.
   procedure Enter_PP (Queue      : in out Priority_Queue_Type;
                       Op         : in     Real_Operation_Type;
                       Arg        : in     Element_Access;
                       New_Status :    out Heap_Status_Access);
   -- Exit preliminary phase.
   procedure Exit_PP (Queue : in out Priority_Queue_Type);

   -- Preliminary phase of Insert.
   procedure Insert_PP (Queue  : in out Priority_Queue_Type;
                        Status : in     Heap_Status_Access);
   -- Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status_Access);
   -- Read and fix a heap entry, ie complete any unfinished operations on it,
   -- except for parts of Op_ID (to avoid recursive breakdown).
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Old_Entry  :    out Heap_Entry_Access);

   -- Sort two heap entries. Used in the sifting phase of delete min.
   -- Assumes that the parent is already involved.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean);

   -- Implicit_Sift_Down. Used by insert.
   -- Assumes that the leaf and is already involved in sifting.
   procedure Implicit_Sift_Down (Queue : in out Priority_Queue_Type;
                                 Leaf  : in     Heap_Index;
                                 Op_ID : in     Operation_ID;
                                 Done  :    out Boolean);

   ----------------------------------------------------------------------------
   procedure Insert (Queue   : in out Priority_Queue_Type;
                     Element : in     Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status_Access;
   begin
      Key := new Element_Type'(Element);

      -- Enter preliminary phase.
      Enter_PP (Queue,
                Op         => INSERT,
                Arg        => Key,
                New_Status => Status);
      Ada.Text_IO.Put_Line ("Insert: Entered PP.");

      -- Perform preliminary insert phase.
      Insert_PP (Queue,
                 Status => Status);
      Ada.Text_IO.Put_Line ("Insert: Done PP.");

      -- Exit preliminary phase.
      Exit_PP (Queue);
      Ada.Text_IO.Put_Line ("Insert: Exited PP.");

      Free (Key);

      -- Sift phase.
      declare
         Done  : Boolean := False;
      begin
         while not Done loop
--            Ada.Text_IO.Put_Line ("Insert: ");
--            Ada.Text_IO.Put_Line (Image (Queue));
            Implicit_Sift_Down
              (Queue,
               Leaf  => Status.Size,
               Op_ID => Status.Op_ID,
               Done  => Done);
--            Ada.Text_IO.Put_Line ("Insert: Continue?");
--            Ada.Text_IO.Skip_Line;
         end loop;
      end;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete_Min (Queue   : in out Priority_Queue_Type;
                         Element :    out Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status_Access;
      Sift   : Boolean;
   begin
      Key := new Element_Type;

      -- Enter preliminary phase.
      Enter_PP (Queue,
                Op         => DELETE_MIN,
                Arg        => Key,
                New_Status => Status);

      -- Perform preliminary delete min phase.
      Delete_Min_PP (Queue,
                     Status => Status);

      -- Sift only if there are nodes left.
      Sift := Status.Size > 0;

      -- Exit preliminary phase.
      Exit_PP (Queue);

      -- Sift phase.
      declare
         Parent : Heap_Index := 1;
         Done   : Boolean;
      begin
         while Sift loop
            --Ada.Text_IO.Put_Line ("Delete_Min: ");
            --Ada.Text_IO.Put_Line (Image (Queue));

            Sort_Parent_Child (Queue,
                               Parent => Parent,
                               Op_ID  => Status.Op_ID,
                               Done   => Done);
            Sift := not Done;

            -- Should choose left or right child with equal probability.
            -- This is WRONG since we can only move the smaller child to
            -- the parent position!
            Parent := Heap_Index (2 * Natural (Parent) +
                                  Natural (Status.Op_ID mod 2));

            --Ada.Text_IO.Put_Line ("Delete_Min: Continue?");
            --Ada.Text_IO.Skip_Line;
         end loop;
      end;

      Element := Key.all;
      Free (Key);

   exception
      when Queue_Empty | Queue_Full =>
         Free (Key);
         raise;
      when others =>
         Free (Key);
         raise;
   end Delete_Min;

   ----------------------------------------------------------------------------
   -- Image.
   function Image (Queue : Priority_Queue_Type) return String is

      use Ada.Characters.Latin_1;

      function Image (Status : Heap_Status_Access) return String is
      begin
         if Status = null then
            return "null";
         else
            return
              "(Size ="  & Natural'Image (Status.Size) &
              ", Op_ID = " & Operation_ID'Image (Status.Op_ID) &
              ", Op_Type = " & Operation_Type'Image (Status.Op_Type) &
              ")";
         end if;
      end Image;

      function Image (E : Heap_Entry_Access) return String is
      begin
         if E = null then
            return "null";
         else
            return
              "(Key = " & Image (E.Key) &
              ", Status = " & Entry_Status'Image (E.Status) &
              ", Old_Key = " & Image (E.Old_Key) &
              ", Op_ID = " & Operation_ID'Image (E.Op_ID) &
              ", Sift_Pos = " & Heap_Index'Image (E.Sift_Pos) &
              ")";
         end if;
      end Image;

      function Image (Heap : Heap_Array) return String is
      begin
         case Heap'Length is
            when 0 =>
               return "";
            when 1 =>
               return Image (Heap (Heap'First));
            when others =>
               return
                 " " & Image (Heap (Heap'First)) & ", " & LF &
                 Image (Heap (Heap'First + 1 .. Heap'Last));
         end case;
      end Image;

      Status : Heap_Status_Access := Queue.Status;
      Heap   : Heap_Array := Queue.Heap;
   begin
      return "( " &
        Image (Status) & ", " & LF &
        "[" & Image (Heap) & "] )";
   end Image;

   ----------------------------------------------------------------------------
   -- Internal functions.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Enter_PP (Queue      : in out Priority_Queue_Type;
                       Op         : in     Real_Operation_Type;
                       Arg        : in     Element_Access;
                       New_Status :    out Heap_Status_Access) is
      use type Primitives.Unsigned_32;
      Status     : Heap_Status_Access;
   begin
      New_Status := new Heap_Status;
      loop
         -- Help any ongoing preliminary phases of operations.
         loop
            -- Read heap status.
            Status := Queue.Status;

            case Status.Op_Type is
               when INSERT =>
                  Insert_PP (Queue,
                             Status);
               when DELETE_MIN =>
                  Delete_Min_PP (Queue,
                                 Status);
               when NONE =>
                  exit;
            end case;
         end loop;

         -- Attempt to start preliminary phase

         -- Set up new status record.
         New_Status.Op_Type := Op;
         New_Status.Op_ID   := Status.Op_ID + 1;
         case Op is
            when INSERT =>
               if Status.Size < Queue.Max_Size then
                  New_Status.Size := Status.Size + 1;
               else
                  Free (New_Status);
                  raise Queue_Full;
               end if;

            when DELETE_MIN =>
               if Status.Size > 0 then
                  New_Status.Size := Status.Size - 1;
               else
                  Free (New_Status);
                  raise Queue_Empty;
               end if;
         end case;
         New_Status.Op_Arg := Arg;

         -- Commit status record.
         exit when CAS (Target    => Queue.Status'Access,
                        Old_Value => Status,
                        New_Value => New_Status);
      end loop;
   end Enter_PP;

   ----------------------------------------------------------------------------
   -- Exit preliminary phase.
   -- Must be changed to recognize if it has been helped.
   procedure Exit_PP (Queue : in out Priority_Queue_Type) is
      Status     : Heap_Status_Access;
      New_Status : Heap_Status_Access;
   begin
      New_Status := new Heap_Status;
      loop
         -- Read heap status.
         Status := Queue.Status;

         -- Set up new status record.
         New_Status.Op_Type := NONE;
         New_Status.Op_ID   := Status.Op_ID;
         New_Status.Size    := Status.Size;
         New_Status.Op_Arg  := null;

         -- Commit status record.
         exit when CAS (Target    => Queue.Status'Access,
                        Old_Value => Status,
                        New_Value => New_Status);
      end loop;
   end Exit_PP;

   ----------------------------------------------------------------------------
   -- Preliminary phase of Insert.
   procedure Insert_PP (Queue  : in out Priority_Queue_Type;
                        Status : in     Heap_Status_Access) is
   begin
      -- Phase 1: Add new leaf.
      declare
         New_Leaf : Heap_Entry_Access;
      begin
         New_Leaf := new Heap_Entry;

         New_Leaf.Key      := Status.Op_Arg.all;
         New_Leaf.Status   := SIFTING_2;
         New_Leaf.Op_ID    := Status.Op_ID;
         New_Leaf.Sift_Pos := Heap_Index'First;

         if not CAS (Target    => Queue.Heap (Status.Size)'Access,
                     Old_Value => null,
                     New_Value => New_Leaf) then
            Ada.Text_IO.Put_Line ("Insert_PP: Heap.Size erroneous!");
            raise Constraint_Error;
         end if;
      end;

      -- Phase 2: Update root.
      declare
         Root     : Heap_Entry_Access;
         New_Root : Heap_Entry_Access;
      begin
         New_Root := new Heap_Entry;

         loop
            -- Read root.
            -- This should probably be done through the helping Fix function.
            Read_And_Fix (Queue,
                          Index      => Heap_Index'First,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Root,
                          Clean_Copy => New_Root.all);

            -- Skip if helped!
            -- Update root.
            New_Root.Status   := SIFTING_2;
            New_Root.Op_ID    := Status.Op_ID;
            New_Root.Sift_Pos := Status.Size;

            -- Commit root.
            exit when CAS (Target    => Queue.Heap (Heap_Index'First)'Access,
                           Old_Value => Root,
                           New_Value => New_Root);
         end loop;
      end;
   end Insert_PP;

   ----------------------------------------------------------------------------
   -- Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status_Access) is
      Leaf     : Heap_Entry_Access;
      New_Leaf : Heap_Entry_Access;
   begin
      New_Leaf := new Heap_Entry;

      -- Phase 1: Mark highest leaf DELETED.
      Phase_1 : loop
         -- Read leaf.
         Read_And_Fix (Queue,
                       Index      => Status.Size + 1,
                       Op_ID      => Status.Op_ID,
                       Old_Entry  => Leaf,
                       Clean_Copy => New_Leaf.all);

         if Leaf /= null then
            New_Leaf.Status  := DELETED;
            New_Leaf.Op_Id   := Status.Op_ID;
            New_Leaf.Old_Key := New_Leaf.Key;
         else
            -- We have been helped.
            exit Phase_1;
         end if;

         exit when CAS (Target    => Queue.Heap (Status.Size + 1)'Access,
                        Old_Value => Leaf,
                        New_Value => New_Leaf);
      end loop Phase_1;

      -- Print status of heap.
      Ada.Text_IO.Put_Line ("Delete_Min_PP: Removed highest leaf.");
      Ada.Text_IO.Put_Line (Image (Queue));

      -- Phase 2: Fix new root.
      if Status.Size > 0 then
         -- Update root.
         declare
            Root     : Heap_Entry_Access;
            New_Root : Heap_Entry_Access;
         begin
            New_Root := new Heap_Entry;
            loop
               -- Read root.
               -- This should probably be done through the helping Fix
               -- function.
               Read_And_Fix (Queue,
                             Index      => Heap_Index'First,
                             Op_ID      => Status.Op_ID,
                             Old_Entry  => Root,
                             Clean_Copy => New_Root.all);

               -- Store root key.
               Status.Op_Arg.all := New_Root.Key;

               -- Update root.
               New_Root.all      := New_Leaf.all;
               New_Root.Status   := SIFTING_1;
               New_Root.Op_ID    := Status.Op_ID;
               --New_Root.Sift_Pos := Status.Size; -- Choose branch to follow here. No, that is done in Delete_Min.

               -- Commit root.
               exit when CAS (Target    => Queue.Heap (Heap_Index'First)'Access,
                              Old_Value => Root,
                              New_Value => New_Root);
            end loop;
         end;
      else
         Status.Op_Arg.all := Leaf.Key;
      end if;

      -- The clean leaf was only temporary.
      --Free (New_Leaf);
   end Delete_Min_PP;

   ----------------------------------------------------------------------------
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Old_Entry  :    out Heap_Entry_Access) is
      Done : Boolean;
   begin
      -- Read entry.
      if Index <= Queue.Max_Size then
         Old_Entry := Queue.Heap (Index);
      else
         Ada.Text_IO.Put_Line ("Read_And_Fix: Node position too large: " &
                               Heap_Index'Image (Index) &
                               " op " & Operation_ID'Image (Op_ID));
         Old_Entry := null;
         return;
      end if;

      if Old_Entry = null then
         Ada.Text_IO.Put_Line ("Read_And_Fix: Read null entry!");
         return;
      end if;

      if Old_Entry.Op_ID /= Op_ID then
         case Old_Entry.Status is
            when STABLE =>
               -- Return copy.
               Clean_Copy := Old_Entry.all;

            when SIFTING_1 =>
               --if Old_Entry.Sift_Pos > Index then
               Sort_Parent_Child
                 (Queue,
                  Parent => Heap_Index (Index),
                  Op_ID => Old_Entry.Op_ID,
                  Done  => Done);
               --else
               --   Ada.Text_IO.Put_Line ("Read_And_Fix: Hit SIFTING child!");
               --   raise Constraint_Error;
               --end if;
               -- Return copy.
               Old_Entry := Queue.Heap (Index);
               Clean_Copy := Old_Entry.all;

            when SIFTING_2 =>
               if Old_Entry.Sift_Pos > Index then
                  Implicit_Sift_Down
                    (Queue,
                     Leaf  => Old_Entry.Sift_Pos,
                     Op_ID => Old_Entry.Op_ID,
                     Done  => Done);
                  -- Return copy.
                  Old_Entry := Queue.Heap (Index);
                  Clean_Copy := Old_Entry.all;
               else
                  Implicit_Sift_Down
                    (Queue,
                     Leaf  => Index,
                     Op_ID => Old_Entry.Op_ID,
                     Done  => Done);
                  -- Return copy.
                  Old_Entry := Queue.Heap (Index);
                  Clean_Copy := Old_Entry.all;
               end if;

            when SWAP_WITH_PARENT | SWAP_WITH_ANC =>
               Ada.Text_IO.Put_Line ("Read_And_Fix: Hit " &
                                     Entry_Status'Image (SWAP_WITH_ANC) &
                                     ". This should not happen!");
               -- This can also happen because of Delete_Min.
               -- Copy.
               Clean_Copy := Old_Entry.all;
               Clean_Copy.Status := STABLE;

            when others =>
               -- Helping not implemented!
               if Old_Entry.Status /= Stable then
                  Ada.Text_IO.Put_Line ("Reading unstable entry: " &
                                        Entry_Status'Image (Old_Entry.Status));
               end if;

               -- Copy.
               Clean_Copy := Old_Entry.all;
               Clean_Copy.Status := STABLE;
         end case;
      else
         -- Copy.
         Clean_Copy := Old_Entry.all;
         Clean_Copy.Status := STABLE;
      end if;
   end Read_And_Fix;

   ----------------------------------------------------------------------------
   -- Sort two heap entries.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean) is
      New_Entry    : Heap_Entry_Access;
      Parent_Entry : Heap_Entry_Access;
      Child_Entry  : Heap_Entry_Access;
      Child        : Heap_Index := Heap_Index (2 * Natural (Parent) +
                                               Natural (Op_ID mod 2));
   begin
      Done := False;

      -- Step 1: Set Child to SWAP iff Parent > Child.
      -- BOTH childs must be checked!!!
      New_Entry := new Heap_Entry;
      Phase_1 : loop
         -- Read Parent and Child entries.
         Parent_Entry := Queue.Heap (Parent);
         Read_And_Fix (Queue,
                       Index      => Child,
                       Op_ID      => Op_ID,
                       Old_Entry  => Child_Entry,
                       Clean_Copy => New_Entry.all);
         --Child_Entry  := Queue.Heap (Child);

         -- Check if this step needs to be done.
         if Parent_Entry /= null and then
           (Parent_Entry.Status = SIFTING_1 and
            Parent_Entry.Op_ID = Op_ID) then
            -- This operation is still unfinished.

            if Child_Entry /= null then
               -- There is a child.

               if Child_Entry.Status = STABLE and
                 Parent_Entry.Key > New_Entry.Key then
                  -- Swap.
                  -- Prepare new Child entry.
                  New_Entry.Status  := SWAP_WITH_PARENT;
                  New_Entry.Old_Key := Child_Entry.Key;
                  New_Entry.Key     := Parent_Entry.Key;
                  New_Entry.Op_ID   := Op_ID;

               elsif Child_Entry.Status /= STABLE then
                  Ada.Text_IO.Put_Line ("Sort_Parent_Child: Child not STABLE!");
                  raise Constraint_Error;
               else
                  -- No swap needed so we are finished.
                  Free (New_Entry);
                  Done := True;
                  exit Phase_1;
               end if;
            else
               -- No child, so we are finished.
               Free (New_Entry);
               Done := True;
               exit Phase_1;
            end if;
         else
            -- We have been helped.
            Free (New_Entry);
            exit Phase_1;
         end if;

         exit when CAS (Target    => Queue.Heap (Child)'Access,
                        Old_Value => Child_Entry,
                        New_Value => New_Entry);
      end loop Phase_1;

      -- Step 2: Update Parent.
      New_Entry := new Heap_Entry;
      Phase_2 : loop
         -- Read Parent and Child entries.
         -- Should be done through Fix?
         Parent_Entry := Queue.Heap (Parent);
         if Child in Queue.Heap'Range then
            Child_Entry  := Queue.Heap (Child);
         else
            Child_Entry := null;
         end if;

         -- Check if this step needs to be done.
         if Parent_Entry /= null and then
           Parent_Entry.Op_ID  = Op_ID then
            -- This operation is still unfinished.

            if Child_Entry /= null then

               if Parent_Entry.Status = SIFTING_1 and
                 Child_Entry.Op_ID = Op_ID and
                 Child_Entry.Status = SWAP_WITH_PARENT then
                  -- Swap parent key and make stable.
                  -- Prepare new Parent entry.
                  New_Entry.all := Parent_Entry.all;

                  New_Entry.Status  := STABLE;
                  New_Entry.Key     := Child_Entry.Old_Key;
                  New_Entry.Op_ID   := 0;

               elsif Parent_Entry.Status = SIFTING_1 and
                 Child_Entry.Status = STABLE then
                  -- Mark parent stable.
                  -- Prepare new Parent entry.
                  New_Entry.all := Parent_Entry.all;

                  New_Entry.Status  := STABLE;
                  New_Entry.Op_ID   := 0;

                  Done := True;
               else
                  -- Unknown state!
                  Ada.Text_IO.Put_Line ("Sort_Parent_Child: Unknown state!");
                  Ada.Text_IO.Put_Line
                    ("Parent = " & Heap_Index'Image (Parent) & ", " &
                     "Child  = " & Heap_Index'Image (Child));
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;
            else
               -- No child. We are done.
               -- Mark parent stable.
               New_Entry.all := Parent_Entry.all;
               New_Entry.Status  := STABLE;
               New_Entry.Op_ID   := 0;

               Done := True;
            end if;
         else
            -- We have been helped!
            Free (New_Entry);
            exit Phase_2;
         end if;

         exit when CAS (Target    => Queue.Heap (Parent)'Access,
                        Old_Value => Parent_Entry,
                        New_Value => New_Entry);
      end loop Phase_2;

      -- Step 3: Finish Child.
      New_Entry := new Heap_Entry;
      Phase_3 : loop
         -- Read Child entry.
         -- Should be done through Fix?
         if Child in Queue.Heap'Range then
            Child_Entry  := Queue.Heap (Child);
         else
            Child_Entry := null;
         end if;

         -- Check if this step needs to be done.
         if Child_Entry /= null and then
           Child_Entry.Op_ID = Op_ID then
            -- This operation is still unfinished.

            if Child_Entry.Status = SWAP_WITH_PARENT then

               -- Prepare new Child entry.
               New_Entry.all := Child_Entry.all;
               New_Entry.Status := SIFTING_1;
            else
               -- Nothing to be done.
               -- And according to the algorithm we can end the sift phase.
               Free (New_Entry);
               exit Phase_3;
            end if;
         else
            -- We have been helped!
            Free (New_Entry);
            exit Phase_3;
         end if;

         exit when CAS (Target    => Queue.Heap (Child)'Access,
                        Old_Value => Child_Entry,
                        New_Value => New_Entry);
      end loop Phase_3;
   end Sort_Parent_Child;

   ----------------------------------------------------------------------------
   -- Implicit_Sift_Down. Used by insert.
   -- Assumes that the leaf is already involved in sifting.
   procedure Implicit_Sift_Down (Queue : in out Priority_Queue_Type;
                                 Leaf  : in     Heap_Index;
                                 Op_ID : in     Operation_ID;
                                 Done  :    out Boolean) is

      function Next_Ancestor (Child, Ancestor : Heap_Index)
                             return Heap_Index is
         Next : Heap_Index := Child;
      begin
         while Next /= Ancestor loop
            if Next/2 = Ancestor then
               return Next;
            end if;
            Next := Next/2;
         end loop;
         return Child;
      end Next_Ancestor;

      New_Entry     : Heap_Entry_Access;
      Anc_Entry     : Heap_Entry_Access;
      Leaf_Entry    : Heap_Entry_Access;
      New_Anc_Entry : Heap_Entry_Access;
      Ancestor      : Heap_Index;
      New_Ancestor  : Heap_Index;
   begin
      Done := False;

      -- Phase 1: Update leaf for swap
      New_Entry := new Heap_Entry;
      Phase_1 : loop
         -- Read ancestor and leaf entries.
         Leaf_Entry  := Queue.Heap (Leaf);
         Ancestor    := Leaf_Entry.Sift_Pos;
         Anc_Entry   := Queue.Heap (Ancestor);

         -- Security check.
         if Anc_Entry = null or Leaf_Entry = null then
            Ada.Text_IO.Put_Line ("Implicit_Sift_Down: Null pointer!");
            raise Constraint_Error;
         end if;


         if Anc_Entry.Status = SIFTING_2 and
           Leaf_Entry.Status = SIFTING_2 and
           Anc_Entry.Op_ID = Leaf_Entry.Op_ID then
            -- The leaf's status is ok because of Op_Id

            if Anc_Entry.Key > Leaf_Entry.Key then
               -- Swap.
               -- Prepare new Leaf entry.

               New_Entry.all     := Leaf_Entry.all;
               New_Entry.Status  := SWAP_WITH_ANC;
               New_Entry.Old_Key := Leaf_Entry.Key;
               New_Entry.Key     := Anc_Entry.Key;
               New_Entry.Op_ID   := Leaf_Entry.Op_ID;

            elsif Leaf_Entry.Sift_Pos = Leaf then
               -- Done sifting. Mark leaf as stable.

               New_Entry.all     := Leaf_Entry.all;
               New_Entry.Status  := STABLE;
               Done := True;

            else
              -- Don't swap, increase sift_pos (but not yet).

              Free (New_Entry);
              exit Phase_1;
            end if;

         else
            -- We have been helped. Increase sift_pos (but not yet).

            Free (New_Entry);
            -- Check if we have been helped all the way.
            Done := Leaf_Entry.Status = STABLE;
            exit Phase_1;

         end if;

         exit when CAS (Target    => Queue.Heap (Leaf)'Access,
                        Old_Value => Leaf_Entry,
                        New_Value => New_Entry);
      end loop Phase_1;

      -- Phase 2: Update new ancestor.
      New_Entry := new Heap_Entry;

      Phase_2 : loop
         -- Read new ancestor and leaf entries.
         Leaf_Entry    := Queue.Heap (Leaf);
         New_Ancestor  := Next_Ancestor (Leaf, Leaf_Entry.Sift_Pos);
         --         New_Anc_Entry := Queue.Heap (New_Ancestor);
         -- What if the New_Ancestor and the Leaf is the same?!
         -- Won't Read_And_Fix freak out?! (But it should fix it.)
         Read_And_Fix (Queue,
                       Index      => New_Ancestor,
                       Op_ID      => Leaf_Entry.Op_ID,
                       Old_Entry  => New_Anc_Entry,
                       Clean_Copy => New_Entry.all);

         -- Security check.
         if Leaf_Entry = null or New_Anc_Entry = null then
            Ada.Text_IO.Put_Line ("Implicit_Sift_Down: Null pointer!");
            raise Constraint_Error;
         end if;

         if (Leaf_Entry.Status = SWAP_WITH_ANC or
             Leaf_Entry.Status = SIFTING_2) and
           New_Anc_Entry.Status = STABLE and
           Leaf_Entry.Op_ID = Op_ID then
            -- Mark new ancestor

            -- Prepare new ancestor entry.

            New_Entry.all      := New_Anc_Entry.all;
            New_Entry.Status   := SIFTING_2;
            New_Entry.Op_ID    := Leaf_Entry.Op_ID;
            New_Entry.Sift_Pos := Leaf;
         else
            -- We have been helped.

            Free (New_Entry);
            -- Check if we have been helped all the way.
            Done := Leaf_Entry.Status = STABLE;
            exit Phase_2;
         end if;

         exit when CAS (Target    => Queue.Heap (New_Ancestor)'Access,
                        Old_Value => New_Anc_Entry,
                        New_Value => New_Entry);
      end loop Phase_2;

      -- Phase 3: Update old ancestor.
      New_Entry := new Heap_Entry;

      Phase_3 : loop
         -- Read ancestor and leaf entries.
         Leaf_Entry    := Queue.Heap (Leaf);
         Ancestor      := Leaf_Entry.Sift_Pos;
         Anc_Entry     := Queue.Heap (Ancestor);
         New_Ancestor  := Next_Ancestor (Leaf, Ancestor);
         New_Anc_Entry := Queue.Heap (New_Ancestor);

         -- Security check.
         if Leaf_Entry = null or Anc_Entry = null or New_Anc_Entry = null Then
            Ada.Text_IO.Put_Line ("Implicit_Sift_Down: Null pointer!");
            raise Constraint_Error;
         end if;

         if Anc_Entry.Status = SIFTING_2 and
           (New_Anc_Entry.Status = SIFTING_2 or New_Ancestor = Leaf) and
           Leaf_Entry.Op_ID = Anc_Entry.Op_ID and
           Leaf_Entry.Op_ID = New_Anc_Entry.Op_ID then
            -- There might be work to do.

            if Leaf_Entry.Status = SIFTING_2 then
               -- Mark old ancestor as stable.

               New_Entry.all      := Anc_Entry.all;
               New_Entry.Status   := STABLE;
               New_Entry.Op_ID    := 0;

            elsif Leaf_Entry.Status = SWAP_WITH_ANC then
               -- Update key and mark old ancestor as stable

               New_Entry.all      := Anc_Entry.all;
               New_Entry.Key      := Leaf_Entry.Old_Key;
               New_Entry.Status   := STABLE;
               New_Entry.Op_ID    := 0;

            else
               -- We have been helped.
               Free (New_Entry);
               exit Phase_3;
            end if;

         else
            -- We have been helped.
            Free (New_Entry);
            -- Check if we have been helped all the way.
            Done := Leaf_Entry.Status = STABLE;
            exit Phase_3;
         end if;

         exit when CAS (Target    => Queue.Heap (Ancestor)'Access,
                        Old_Value => Anc_Entry,
                        New_Value => New_Entry);
      end loop Phase_3;

      -- Phase 4: Update leaf for new ancestor.
      New_Entry := new Heap_Entry;

      Phase_4 : loop
         -- Read ancestor and leaf entries.
         Leaf_Entry    := Queue.Heap (Leaf);
         Ancestor      := Leaf_Entry.Sift_Pos;
         Anc_Entry     := Queue.Heap (Ancestor);
         New_Ancestor  := Next_Ancestor (Leaf, Ancestor);
         New_Anc_Entry := Queue.Heap (New_Ancestor);

         -- Security check.
         if Leaf_Entry = null or Anc_Entry = null or New_Anc_Entry = null Then
            Ada.Text_IO.Put_Line ("Implicit_Sift_Down: Null pointer!");
            raise Constraint_Error;
         end if;

         if Anc_Entry.Op_ID /= Op_ID and
           (New_Anc_Entry.Status = SIFTING_2 or New_Ancestor = Leaf) and
           Leaf_Entry.Op_ID = New_Anc_Entry.Op_ID and
           (Leaf_Entry.Status = SIFTING_2 or
            Leaf_Entry.Status = SWAP_WITH_ANC) then

            if Leaf /= New_Ancestor then
               -- Update sift position for the leaf.

               New_Entry.all      := Leaf_Entry.all;
               New_Entry.Status   := SIFTING_2;
               New_Entry.Sift_Pos := New_Ancestor;

            else
               -- We are finished.
               New_Entry.all      := Leaf_Entry.all;
               New_Entry.Status   := STABLE;
               New_Entry.Op_ID    := 0;

               Done := True;
            end if;
         else
            -- We have been helped.
            Free (New_Entry);
            -- Check if we have been helped all the way.
            Done := Leaf_Entry.Status = STABLE;
            exit Phase_4;
         end if;

         exit when CAS (Target    => Queue.Heap (Leaf)'Access,
                        Old_Value => Leaf_Entry,
                        New_Value => New_Entry);
      end loop Phase_4;
   end Implicit_Sift_Down;

end Non_Blocking_Priority_Queue;
