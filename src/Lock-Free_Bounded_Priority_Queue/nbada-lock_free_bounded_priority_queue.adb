-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.adb
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 12:15:16 2002
-- $Id: nbada-lock_free_bounded_priority_queue.adb,v 1.23 2003/03/17 11:02:57 andersg Exp $
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
   procedure Exit_PP (Queue : in out Priority_Queue_Type;
                      Op_ID : in     Operation_ID);

   -- Preliminary phase of Insert.
   procedure Insert_PP (Queue  : in out Priority_Queue_Type;
                        Status : in     Heap_Status_Access);
   -- Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status_Access);
   -- Read and fix a heap entry, ie complete any unfinished older
   -- operations on it, except for parts of Op_ID (to avoid recursive
   -- breakdown).
   -- Helped is true if the Op_ID of the heap entry is larger than or
   -- equal to Op_ID.
   -- The Old_Entry and Clean_Copy is invalid when Helped is true.
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Helped     :    out Boolean;
                           Old_Entry  :    out Heap_Entry_Access;
                           Ignore_SIFTING_2_Leaf : in Boolean := False);
   -- Read a heap entry.
   -- Will attempt to detect helping. A null entry is interpreted as helped.
   -- The_Entry is invalid if helped.
   procedure Read (Queue      : in out Priority_Queue_Type;
                   Index      : in     Heap_Index;
                   Op_ID      : in     Operation_ID;
                   The_Entry  :    out Heap_Entry_Access;
                   Helped     :    out Boolean;
                   Ignore_SIFTING_2_Leaf : in Boolean := False);
   pragma Inline (Read);
   pragma Inline_Always (Read);

   -- Sort two heap entries. Used in the sifting phase of delete min.
   -- Assumes that the parent is already involved.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean);

   -- Implicit_Sift_Down.
   -- Used by Insert and Read_And_Fix to perform one sift down step.
   -- The leaf and the ancestor should already be involved in sifting.
   procedure Implicit_Sift_Down (Queue    : in out Priority_Queue_Type;
                                 Leaf     : in     Heap_Index;
                                 Ancestor : in     Heap_Index;
                                 Op_ID    : in     Operation_ID);

   ----------------------------------------------------------------------------
   procedure Insert (Queue   : in out Priority_Queue_Type;
                     Element : in     Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status_Access;
      Debug  : constant Boolean := False;
   begin
      Key := new Element_Type'(Element);

      -- Enter preliminary phase.
      Enter_PP (Queue,
                Op         => INSERT,
                Arg        => Key,
                New_Status => Status);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Entered PP.");
      end if;

      -- Perform preliminary insert phase.
      Insert_PP (Queue,
                 Status => Status);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Done PP.");
      end if;

      -- Exit preliminary phase.
      Exit_PP (Queue, Status.Op_ID);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Exited PP.");
      end if;

      Free (Key);

      -- Sift phase.
      -- Uses lazy sifting for now.
--       declare
--          Done  : Boolean := False;
--       begin
--          while not Done loop
-- --            Ada.Text_IO.Put_Line ("Insert: ");
-- --            Ada.Text_IO.Put_Line (Image (Queue));
--             Implicit_Sift_Down
--               (Queue,
--                Leaf  => Status.Size,
--                Op_ID => Status.Op_ID,
--                Done  => Done);
-- --            Ada.Text_IO.Put_Line ("Insert: Continue?");
-- --            Ada.Text_IO.Skip_Line;
--          end loop;
--       end;
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
      Exit_PP (Queue, Status.Op_ID);

      -- Sift phase.
      -- Uses lazy sifting for now.
--       declare
--          Parent : Heap_Index := 1;
--          Done   : Boolean;
--       begin
--          while Sift loop
--             --Ada.Text_IO.Put_Line ("Delete_Min: ");
--             --Ada.Text_IO.Put_Line (Image (Queue));

         -- We must sift at least one step to make the root stable.
         -- No, we already do that in Delete_Min_PP.
--         Sort_Parent_Child (Queue,
--                            Parent => Parent,
--                            Op_ID  => Status.Op_ID,
--                            Done   => Done);

--             Sift := not Done;

--             -- Should choose left or right child with equal probability.
--             -- This is WRONG since we can only move the smaller child to
--             -- the parent position!
--             Parent := Heap_Index (2 * Natural (Parent) +
--                                   Natural (Status.Op_ID mod 2));

--             --Ada.Text_IO.Put_Line ("Delete_Min: Continue?");
--             --Ada.Text_IO.Skip_Line;
--          end loop;
--      end;

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
            Exit_PP (Queue, Status.Op_ID);
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
   procedure Exit_PP (Queue : in out Priority_Queue_Type;
                      Op_ID : in     Operation_ID) is
      Status     : Heap_Status_Access;
      New_Status : Heap_Status_Access;
   begin
      New_Status := new Heap_Status;
      loop
         -- Read heap status.
         Primitives.Membar;
         Status := Queue.Status;

         if Status.Op_ID > Op_ID or Status.Op_Type = NONE then
            Free (New_Status);
            exit;
         end if;

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
            -- We must have been helped.
            -- Continue.
            null;
         end if;
      end;

      -- Phase 2: Update root.
      declare
         Root     : Heap_Entry_Access;
         New_Root : Heap_Entry_Access;
         Helped   : Boolean;
      begin
         New_Root := new Heap_Entry;

         Phase_2 : loop
            -- Read root.
            Read_And_Fix (Queue,
                          Index      => Heap_Index'First,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Root,
                          Clean_Copy => New_Root.all,
                          Helped     => Helped);

            -- Skip if helped.
            if (Helped or Root = null) or else
              (Root.Op_Id = Status.Op_Id and Root.Status = SIFTING_2)
            then
               Free (New_Root);
               exit Phase_2;
            end if;

            -- Update root.
            New_Root.Status   := SIFTING_2;
            New_Root.Op_ID    := Status.Op_ID;
            New_Root.Sift_Pos := Status.Size;

            -- Commit root.
            exit when CAS (Target    => Queue.Heap (Heap_Index'First)'Access,
                           Old_Value => Root,
                           New_Value => New_Root);
         end loop Phase_2;
      end;

   end Insert_PP;

   ----------------------------------------------------------------------------
   -- Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status_Access) is
      Debug : constant Boolean := False;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Initial.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;

      -- Phase 1: Mark highest leaf DELETED.
      declare
         Leaf     : Heap_Entry_Access;
         New_Leaf : Heap_Entry_Access;
         Helped   : Boolean;
      begin
         New_Leaf := new Heap_Entry;

         -- Fix the root to avoid problems if the leaf is directly
         -- below root. This only needs to be done once since no other
         -- preliminary phase may interfere with us.
         -- In fact this must always be done, since otherwise we might
         -- accidentaly exclude the smallest element in the heap from
         -- the helping by marking it as deleted.
         declare
            New_Root : Heap_Entry;
            Old_Root : Heap_Entry_Access;
         begin
            Read_And_Fix (Queue,
                          Index      => Queue.Heap'First,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Old_Root,
                          Clean_Copy => New_Root,
                          Helped     => Helped);
         end;

         Phase_1 : loop
            -- Read leaf.
            Read_And_Fix (Queue,
                          Index      => Status.Size + 1,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Leaf,
                          Clean_Copy => New_Leaf.all,
                          Helped     => Helped);

            -- Skip if helped.
            if Leaf = null or Helped then
               Free (New_Leaf);
               exit Phase_1;
            end if;

            -- Set leaf status to DELETED.
            New_Leaf.Status  := DELETED;
            New_Leaf.Op_Id   := Status.Op_ID;
            New_Leaf.Old_Key := New_Leaf.Key;

            exit when CAS (Target    => Queue.Heap (Status.Size + 1)'Access,
                           Old_Value => Leaf,
                           New_Value => New_Leaf);
         end loop Phase_1;
      end;

      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Phase 1.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;

      -- Phase 2: Update root.
      declare
            Root     : Heap_Entry_Access;
            Leaf     : Heap_Entry_Access;
            New_Root : Heap_Entry_Access;
            Helped_R, Helped_L : Boolean;
      begin
         if Status.Size > 0 then
            New_Root := new Heap_Entry;
            Phase_2 : loop
               -- Read root.
               -- This is done with helping Fix function although the
               -- root is fixed above.
               Read_And_Fix (Queue,
                             Index      => Heap_Index'First,
                             Op_ID      => Status.Op_ID,
                             Old_Entry  => Root,
                             Clean_Copy => New_Root.all,
                             Helped     => Helped_R);

               -- Read deleted leaf.
               Read(Queue,
                    Index     => Status.Size + 1,
                    Op_ID     => Status.Op_ID,
                    The_Entry => Leaf,
                    Helped    => Helped_L);

               -- Exit if helped.
               if (Helped_R or Helped_L) or else
                 (Leaf.Status /= DELETED) then
                  Free (New_Root);
                  exit Phase_2;
               end if;

               -- Safety checks!
               if Root = null then
                  Ada.Text_IO.Put_Line ("Delete_Min_PP: Null root.");
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;
               if New_Root.Status /= STABLE then
                  Ada.Text_IO.Put_Line ("Delete_Min_PP: Unstable root.");
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;

               -- Store root key.
               Status.Op_Arg.all := New_Root.Key;

               -- Update new root.
               New_Root.all      := Leaf.all;
               New_Root.Status   := SIFTING_1;
               New_Root.Op_ID    := Status.Op_ID;

               -- Commit root.
               exit when
                 CAS (Target    => Queue.Heap (Heap_Index'First)'Access,
                      Old_Value => Root,
                      New_Value => New_Root);
            end loop Phase_2;
         else
            Read(Queue,
                 Index     => Queue.Heap'First,
                 Op_ID     => Status.Op_ID,
                 The_Entry => Root,
                 Helped    => Helped_R);

            -- Store min if we haven't been helped.
            if not Helped_R then
               Status.Op_Arg.all := Root.Key;
               Primitives.Membar;
            end if;
         end if;
      end;

      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Phase 2.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;

      -- Phase 3: Remove deleted leaf.
      declare
         Leaf   : Heap_Entry_Access;
         Helped : Boolean;
      begin
         Phase_3 : loop
            -- Read deleted leaf.
            Read(Queue,
                 Index     => Status.Size + 1,
                 Op_ID     => Status.Op_ID,
                 The_Entry => Leaf,
                 Helped    => Helped);

            -- Skip if helped:
            exit Phase_3 when Helped;

            -- Commit empty leaf.
            exit Phase_3 when
              CAS (Target    => Queue.Heap (Status.Size + 1)'Access,
                   Old_Value => Leaf,
                   New_Value => null);
         end loop Phase_3;
      end;

      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Phase 3.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;
   end Delete_Min_PP;

   ----------------------------------------------------------------------------
   -- Old_Entry = null indicates that we read an empty slot.
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Helped     :    out Boolean;
                           Old_Entry  :    out Heap_Entry_Access;
                           Ignore_SIFTING_2_Leaf : in Boolean := False) is
      Done  : Boolean;
      Debug : constant Boolean := False;
   begin
      Helped := False;

      -- Read entry. Needs to be repeated until we read it stable.
      -- This might not be really optimal since any helping
      -- is commited before we return the clean copy.
      Help : loop
         -- Read current entry value.
         if Index <= Queue.Max_Size then
            Primitives.Membar;
            Old_Entry := Queue.Heap (Index);
         else
            if Debug then
               Ada.Text_IO.Put_Line
                 ("Read_And_Fix: Node position too large: " &
                  Heap_Index'Image (Index) &
                  " op " & Operation_ID'Image (Op_ID));
            end if;
            Old_Entry := null;
            return;
         end if;

          if Old_Entry = null then
             if Debug then
                Ada.Text_IO.Put_Line ("Read_And_Fix: Read null entry!");
             end if;

             return;
          end if;

         -- Check if there is a pending operation.
         -- Think HARD on whether this is the RIGHT WAY to detect this.
         if Old_Entry.Op_ID < Op_ID then
            case Old_Entry.Status is
               ----------------------------------------------------------------
               when STABLE =>
               -- Return copy.
               Clean_Copy := Old_Entry.all;
               exit Help;

               ----------------------------------------------------------------
               when SIFTING_1 =>
                  if Debug then
                     Ada.Text_IO.Put_Line
                       ("Read_And_Fix: Operation" &
                        Operation_ID'Image (Op_ID) &
                        " hit " &
                        Entry_Status'Image (Old_Entry.Status) &
                        " from " &
                        Operation_ID'Image (Old_Entry.Op_ID) &
                        " at" & Heap_Index'Image(Index) & ".");
                  end if;
                  Sort_Parent_Child
                    (Queue,
                     Parent => Index,
                     Op_ID  => Old_Entry.Op_ID,
                     Done   => Done);

               ----------------------------------------------------------------
               when SIFTING_2 =>
                  if Debug then
                     Ada.Text_IO.Put_Line
                       ("Read_And_Fix: Operation" &
                        Operation_ID'Image (Op_ID) &
                        " hit " &
                        Entry_Status'Image (Old_Entry.Status) &
                        " from " &
                        Operation_ID'Image (Old_Entry.Op_ID) &
                        " at" & Heap_Index'Image(Index) & ".");
                  end if;
                  if Old_Entry.Sift_Pos >= Index then
                     -- We hit the active ancestor to a SIFTING_2 leaf.
                     Implicit_Sift_Down
                       (Queue,
                        Leaf     => Old_Entry.Sift_Pos,
                        Ancestor => Index,
                        Op_ID    => Old_Entry.Op_ID);
                  else
                     -- We hit a SIFTING_2 leaf.
                     if Ignore_SIFTING_2_Leaf then
                        -- Treat it as an empty slot.
                        Old_Entry := null;
                        return;
                     else
                        Implicit_Sift_Down
                          (Queue,
                           Leaf     => Index,
                           Ancestor => Old_Entry.Sift_Pos,
                           Op_ID    => Old_Entry.Op_ID);
                     end if;
                  end if;

               ----------------------------------------------------------------
               when SWAP_WITH_PARENT =>
                  if Debug then
                     Ada.Text_IO.Put_Line
                       ("Read_And_Fix: Operation" &
                        Operation_ID'Image (Op_ID) &
                        " hit " &
                        Entry_Status'Image (Old_Entry.Status) &
                        " from " &
                        Operation_ID'Image (Old_Entry.Op_ID) &
                        " at" & Heap_Index'Image(Index) & ".");
                  end if;

                     Sort_Parent_Child
                       (Queue,
                        Parent => Index/2,
                        Op_ID  => Old_Entry.Op_ID,
                        Done   => Done);

               ----------------------------------------------------------------
               when SWAP_WITH_ANC =>
                  if Debug then
                     Ada.Text_IO.Put_Line
                       ("Read_And_Fix: Operation" &
                        Operation_ID'Image (Op_ID) &
                        " hit " &
                        Entry_Status'Image (Old_Entry.Status) &
                        " from " &
                        Operation_ID'Image (Old_Entry.Op_ID) &
                        " at" & Heap_Index'Image(Index) & ".");
                  end if;

                  if Old_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf then
                     -- We hit a SIFTING_2 leaf with parent above.
                     -- Treat it as an empty slot.
                     Old_Entry := null;
                     return;
                  else
                     Implicit_Sift_Down
                       (Queue,
                        Leaf     => Index,
                        Ancestor => Old_Entry.Sift_Pos,
                        Op_ID    => Old_Entry.Op_ID);
                  end if;

               ----------------------------------------------------------------
               when DELETED =>
                  -- We hit an ongoing preliminary phase.
                  -- Treat as empty.
                  Old_Entry := null;
                  return;

               ----------------------------------------------------------------
               when others =>
                  -- Helping not implemented!
                  Ada.Text_IO.Put_Line
                    ("Read_And_Fix: Operation" &
                     Operation_ID'Image (Op_ID) &
                     " hit " &
                     Entry_Status'Image (Old_Entry.Status) &
                     " from " &
                     Operation_ID'Image (Old_Entry.Op_ID) &
                     " at" & Heap_Index'Image(Index) & ".");
                  Ada.Text_IO.Put_Line (Image (Queue));

                  raise Constraint_Error;
            end case;
         elsif Old_Entry.Op_ID = Op_ID then
            -- Since Read_And_Fix is only used before an operation has
            -- managed to update an entry, Old_Entry.Op_ID = Op_ID
            -- indicates that the operation has been helped.

            Old_Entry := null;
            Helped := True;
            return;

--             case Old_Entry.Status is
--                when DELETED =>
--                   -- Ignore this entry.
--                   Old_Entry := null;

--                when STABLE =>
--                   -- We must have been helped since the entry is STABLE and
--                   -- Old_Entry.Op_ID = Op_ID, i.e. our op is finished with it.
--                   Clean_Copy := Old_Entry.all;
--                   Helped := True;

--                when others =>
--                   -- We need to finish with this entry.
--                   -- Copy.
--                   Clean_Copy := Old_Entry.all;
--                   --Helped := Clean_Copy.Op_ID > Op_ID;
--             end case;
--             return;
         else
            -- This operation has been helped.
            -- DOUBLE CHECK THIS DETECTION!!

            -- The helped operation should not use anything
            -- Read_And_Fix returns. The Clean copy is not guaranteed to
            -- be stable.

            -- Do not claim to be helped if we encounter and ignore a
            -- SIFTING_2 leaf.
            Helped     := (Old_Entry.Status /= SIFTING_2 and
                           Old_Entry.Status /= SWAP_WITH_ANC) or
              not (Old_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf);
            Old_Entry  := null;

            return;
         end if;
      end loop Help;
   end Read_And_Fix;

   ----------------------------------------------------------------------------
   -- Read.
   procedure Read (Queue      : in out Priority_Queue_Type;
                   Index      : in     Heap_Index;
                   Op_ID      : in     Operation_ID;
                   The_Entry  :    out Heap_Entry_Access;
                   Helped     :    out Boolean;
                   Ignore_SIFTING_2_Leaf : in Boolean := False) is
   begin
      Primitives.Membar;
      The_Entry := Queue.Heap (Index);

      -- Check if helped.
      Helped := The_Entry = null or else
        (The_Entry.Op_ID > Op_ID and
         ((The_Entry.Status /= SIFTING_2 and
           The_Entry.Status /= SWAP_WITH_ANC) or
          not (The_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf)));
   end Read;

   ----------------------------------------------------------------------------
   -- Sort two heap entries. Assumes that the Parent is in state SIFTING_1.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean) is
      Left_Child         : constant Heap_Index := 2 * Parent;
      Right_Child        : constant Heap_Index := Left_Child + 1;
      Child              : Heap_Index;
   begin
      Done := False;

      -- Phase 1: Set Child to SWAP_WITH_PARENT iff parent > smallest child.
      declare
         Parent_Entry       : Heap_Entry_Access;
         Left_Child_Entry   : Heap_Entry_Access;
         Right_Child_Entry  : Heap_Entry_Access;
         New_Left_Entry     : Heap_Entry_Access;
         New_Right_Entry    : Heap_Entry_Access;
         Child_Entry        : Heap_Entry_Access;
         New_Entry          : Heap_Entry_Access;
         Helped             : Boolean;
      begin
         New_Left_Entry  := new Heap_Entry;
         New_Right_Entry := new Heap_Entry;

         Phase_1 : loop
            -- Read Parent.
            Read(Queue,
                 Index     => Parent,
                 Op_ID     => Op_ID,
                 The_Entry => Parent_Entry,
                 Helped    => Helped);
            if Helped or else Parent_Entry.Status /= SIFTING_1 then
               Free (New_Left_Entry);
               Free (New_Right_Entry);
               exit Phase_1;
            end if;

            -- Read left child.
            Read_And_Fix (Queue,
                          Index      => Left_Child,
                          Op_ID      => Op_ID,
                          Old_Entry  => Left_Child_Entry,
                          Clean_Copy => New_Left_Entry.all,
                          Helped     => Helped,
                          Ignore_SIFTING_2_Leaf => True);
            if Helped then
               -- We have been helped.
               Free (New_Left_Entry);
               Free (New_Right_Entry);
               exit Phase_1;
            end if;

            -- Read right child.
            Read_And_Fix (Queue,
                          Index      => Right_Child,
                          Op_ID      => Op_ID,
                          Old_Entry  => Right_Child_Entry,
                          Clean_Copy => New_Right_Entry.all,
                          Helped     => Helped,
                          Ignore_SIFTING_2_Leaf => True);
            if Helped then
               -- We have been helped.
               Free (New_Left_Entry);
               Free (New_Right_Entry);
               exit Phase_1;
            end if;

            -- This operation is still unfinished.

            -- Select child.
            if Left_Child_Entry /= null and Right_Child_Entry /= null then
               if Right_Child_Entry.Key > Left_Child_Entry.Key then
                  Child_Entry := Left_Child_Entry;
                  Child       := Left_Child;
                  New_Entry   := New_Left_Entry;
               else
                  Child_Entry := Right_Child_Entry;
                  Child       := Right_Child;
                  New_Entry   := New_Right_Entry;
               end if;
            elsif Left_Child_Entry /= null then
               Child_Entry := Left_Child_Entry;
               Child       := Left_Child;
               New_Entry   := New_Left_Entry;
            else
               Child_Entry := Right_Child_Entry;
               Child       := Right_Child;
               New_Entry   := New_Right_Entry;
            end if;

            if Child_Entry /= null then
               -- There is a child.

               if Child_Entry.Status = STABLE then

                  if Parent_Entry.Key > New_Entry.Key then
                     -- Swap.
                     -- Prepare new Child entry.
                     New_Entry.Status  := SWAP_WITH_PARENT;
                     New_Entry.Old_Key := Child_Entry.Key;
                     New_Entry.Key     := Parent_Entry.Key;
                     New_Entry.Op_ID   := Op_ID;
                  else
                     -- Update Op_ID.
                     -- Prepare new Child entry.
                     New_Entry.all     := Child_Entry.all;
                     New_Entry.Op_ID   := Op_ID;
                  end if;

               else
                  Ada.Text_IO.Put_Line
                    ("Sort_Parent_Child: Child " &
                     Heap_Index'Image (Child) & " not STABLE! (" &
                     Entry_Status'Image (Child_Entry.Status) & ")");
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;
            else
               -- No child, so we are finished.
               Free (New_Left_Entry);
               Free (New_Right_Entry);
               Done := True;
               exit Phase_1;
            end if;

            if CAS (Target    => Queue.Heap (Child)'Access,
                    Old_Value => Child_Entry,
                    New_Value => New_Entry) then
               if New_Entry = New_Left_Entry then
                  Free (New_Right_Entry);
               else
                  Free (New_Left_Entry);
               end if;
               exit Phase_1;
            end if;
         end loop Phase_1;
      end;

      -- Step 2: Update Parent.
      declare
         Parent_Entry : Heap_Entry_Access;
         Child_Entry  : Heap_Entry_Access;
         New_Entry    : Heap_Entry_Access;
         Helped       : Boolean;
      begin
         New_Entry := new Heap_Entry;
         Phase_2 : loop
            -- Read Parent.
            Read(Queue,
                 Index     => Parent,
                 Op_ID     => Op_ID,
                 The_Entry => Parent_Entry,
                 Helped    => Helped);
            if Helped or else Parent_Entry.Status /= SIFTING_1 then
               Free (New_Entry);
               exit Phase_2;
            end if;

            -- Read the selected child.
            if Child in Queue.Heap'Range then
               Read(Queue,
                    Index     => Child,
                    Op_ID     => Op_ID,
                    The_Entry => Child_Entry,
                    Helped    => Helped,
                    Ignore_SIFTING_2_Leaf => True);
            else
               Child_Entry := null;
               Helped      := False;
            end if;
            -- In this case no child is good news and means that we can
            -- mark the parent as stable.
             if (Helped and Child_Entry /= null) or
               (Child_Entry /= null and then Child_Entry.Status = SIFTING_1)
             then
                Free (New_Entry);
                exit Phase_2;
             end if;

            -- This operation is still unfinished.
            -- Check whether we have a child entry or not.
            -- For some child states the child should be treated as empty.
            if Child_Entry /= null and then not
              (Child_Entry.Status = SIFTING_2 or
               Child_Entry.Status = SWAP_WITH_ANC)
            then

               if Child_Entry.Status = SWAP_WITH_PARENT then
                  -- Swap parent key and make stable.
                  -- Prepare new Parent entry.
                  New_Entry.all := Parent_Entry.all;

                  New_Entry.Status  := STABLE;
                  New_Entry.Key     := Child_Entry.Old_Key;

               elsif Child_Entry.Status = STABLE then
                  -- Mark parent stable.
                  -- Prepare new Parent entry.
                  New_Entry.all := Parent_Entry.all;

                  New_Entry.Status  := STABLE;

                  Done := True;
               else
                  -- Unknown state!
                  Ada.Text_IO.Put_Line
                    ("Sort_Parent_Child: Unknown child state " &
                     Entry_Status'Image (Child_Entry.Status) & " !");
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

               Done := True;
            end if;

            exit when CAS (Target    => Queue.Heap (Parent)'Access,
                           Old_Value => Parent_Entry,
                           New_Value => New_Entry);
         end loop Phase_2;
      end;

      -- Step 3: Finish Child.
      declare
         Child_Entry        : Heap_Entry_Access;
         New_Entry          : Heap_Entry_Access;
         Helped             : Boolean := True;
      begin
         New_Entry := new Heap_Entry;
         Phase_3 : loop
            -- Read Child entry.
            if Child in Queue.Heap'Range then
               Read(Queue,
                    Index     => Child,
                    Op_ID     => Op_ID,
                    The_Entry => Child_Entry,
                    Helped    => Helped);
            else
               Child_Entry := null;
            end if;
            -- Exit if the child has been helped or is nonexistant.
            -- We should probably set Done to true here.
            if Helped or else (Child_Entry.Status /= SWAP_WITH_PARENT) then
               Free (New_Entry);
               exit Phase_3;
            end if;

            -- This operation is still unfinished.

            if Child_Entry.Status = SWAP_WITH_PARENT then
               -- Prepare new Child entry.
               New_Entry.all := Child_Entry.all;
               New_Entry.Status := SIFTING_1;
            else
               -- Nothing to be done. The child should be stable
               -- and according to the algorithm we can end the sift phase.
               Free (New_Entry);
               exit Phase_3;
            end if;

            exit when CAS (Target    => Queue.Heap (Child)'Access,
                           Old_Value => Child_Entry,
                           New_Value => New_Entry);
         end loop Phase_3;
      end;
   end Sort_Parent_Child;

   ----------------------------------------------------------------------------
   -- Implicit_Sift_Down.
   procedure Implicit_Sift_Down (Queue    : in out Priority_Queue_Type;
                                 Leaf     : in     Heap_Index;
                                 Ancestor : in     Heap_Index;
                                 Op_ID    : in     Operation_ID) is

      function Next_Ancestor (Child, Ancestor : Heap_Index)
                             return Heap_Index is
         Next : Heap_Index := Child;
      begin
         while Next > Ancestor loop
            if Next/2 = Ancestor then
               return Next;
            end if;
            Next := Next/2;
         end loop;
         return Child;
      end Next_Ancestor;

      -- The new ancestor index.
      New_Ancestor : constant Heap_Index := Next_Ancestor (Leaf, Ancestor);
   begin

      -- Phase 1: Update leaf for swap
      declare
         New_Entry     : Heap_Entry_Access;
         Anc_Entry     : Heap_Entry_Access;
         Leaf_Entry    : Heap_Entry_Access;
         Helped_A, Helped_L : Boolean;
      begin
         New_Entry := new Heap_Entry;
         Phase_1 : loop
            -- Read ancestor and leaf entries.
            Read(Queue,
                 Index     => Leaf,
                 Op_ID     => Op_ID,
                 The_Entry => Leaf_Entry,
                 Helped    => Helped_L);

            -- Check if helped.
            if Helped_L then
               Free (New_Entry);
               exit Phase_1;
            end if;

            Read(Queue,
                 Index     => Ancestor,
                 Op_ID     => Op_ID,
                 The_Entry => Anc_Entry,
                 Helped    => Helped_A);

            -- Exit if helped.
            if (Helped_L or Helped_A) or else
              (Anc_Entry.Status /= SIFTING_2 or
               Leaf_Entry.Status /= SIFTING_2) then
               -- We have been helped. Try to increase sift_pos (but not yet).

               Free (New_Entry);
               exit Phase_1;
            end if;

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

            else
               -- Don't swap, increase the leaf's sift_pos (but not yet).

               Free (New_Entry);
               exit Phase_1;
            end if;

            exit when CAS (Target    => Queue.Heap (Leaf)'Access,
                           Old_Value => Leaf_Entry,
                           New_Value => New_Entry);
         end loop Phase_1;
      end;

      -- Phase 2: Update new ancestor.
      declare
         New_Entry     : Heap_Entry_Access;
         Leaf_Entry    : Heap_Entry_Access;
         New_Anc_Entry : Heap_Entry_Access;
         Helped        : Boolean;
      begin
         New_Entry := new Heap_Entry;

         Phase_2 : loop
            -- Read leaf.
            Read(Queue,
                 Index     => Leaf,
                 Op_ID     => Op_ID,
                 The_Entry => Leaf_Entry,
                 Helped    => Helped);

            -- Check if helped.
            if Helped or else Leaf_Entry.Sift_Pos /= Ancestor then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_2;
            end if;

            -- Is the New_Ancestor and the Leaf the same?
            if Leaf = New_Ancestor then
               -- Do nothing.
               Free (New_Entry);
               exit Phase_2;
            end if;

            -- Read new ancestor.
            Read_And_Fix (Queue,
                          Index      => New_Ancestor,
                          Op_ID      => Op_ID,
                          Old_Entry  => New_Anc_Entry,
                          Clean_Copy => New_Entry.all,
                          Helped     => Helped);

            -- Exit if helped.
            if (Helped or Leaf_Entry = null or New_Anc_Entry = null) or else
              ((Leaf_Entry.Status /= SWAP_WITH_ANC and
                Leaf_Entry.Status /= SIFTING_2) or
               New_Anc_Entry.Status /= STABLE)
            then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_2;
            end if;

            -- Prepare new ancestor entry.
            New_Entry.all      := New_Anc_Entry.all;
            New_Entry.Status   := SIFTING_2;
            New_Entry.Op_ID    := Leaf_Entry.Op_ID;
            New_Entry.Sift_Pos := Leaf;

            exit when CAS (Target    => Queue.Heap (New_Ancestor)'Access,
                           Old_Value => New_Anc_Entry,
                           New_Value => New_Entry);
         end loop Phase_2;
      end;

      -- Phase 3: Update old ancestor.
      declare
         New_Entry     : Heap_Entry_Access;
         Anc_Entry     : Heap_Entry_Access;
         Leaf_Entry    : Heap_Entry_Access;
         New_Anc_Entry : Heap_Entry_Access;
         Helped        : Boolean;
      begin
         New_Entry := new Heap_Entry;

         Phase_3 : loop
            -- Read ancestor and leaf entries.
            Read(Queue,
                 Index     => Leaf,
                 Op_ID     => Op_ID,
                 The_Entry => Leaf_Entry,
                 Helped    => Helped);

            -- Check if helped.
            if Helped or else Leaf_Entry.Sift_Pos /= Ancestor then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_3;
            end if;

            -- Read ancestor.
            Read(Queue,
                 Index     => Ancestor,
                 Op_ID     => Op_ID,
                 The_Entry => Anc_Entry,
                 Helped    => Helped);

            -- Exit if helped.
            if Helped or else Anc_Entry.Status /= SIFTING_2 then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_3;
            end if;

            Read(Queue,
                 Index     => New_Ancestor,
                 Op_ID     => Op_ID,
                 The_Entry => New_Anc_Entry,
                 Helped    => Helped);

            -- Exit if helped.
            if Helped or else
              (New_Anc_Entry.Status /= SIFTING_2 and New_Ancestor /= Leaf)
            then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_3;
            end if;

            -- There is work to do.

            if Leaf_Entry.Status = SIFTING_2 then
               -- Mark old ancestor as stable.

               New_Entry.all      := Anc_Entry.all;
               New_Entry.Status   := STABLE;

            elsif Leaf_Entry.Status = SWAP_WITH_ANC then
               -- Update key and mark old ancestor as stable

               New_Entry.all      := Anc_Entry.all;
               New_Entry.Key      := Leaf_Entry.Old_Key;
               New_Entry.Status   := STABLE;

            else
               -- We have been helped. (Should never get here.)
               Free (New_Entry);
               exit Phase_3;
            end if;

            exit when CAS (Target    => Queue.Heap (Ancestor)'Access,
                           Old_Value => Anc_Entry,
                           New_Value => New_Entry);
         end loop Phase_3;
      end;

      -- Phase 4: Update leaf for new ancestor.
      declare
         New_Entry     : Heap_Entry_Access;
         Leaf_Entry    : Heap_Entry_Access;
         New_Anc_Entry : Heap_Entry_Access;
         Helped        : Boolean;
      begin
         New_Entry := new Heap_Entry;

         Phase_4 : loop
            -- Read leaf entry.
            Read(Queue,
                 Index     => Leaf,
                 Op_ID     => Op_ID,
                 The_Entry => Leaf_Entry,
                 Helped    => Helped);

            -- Check if helped.
            if Helped or else Leaf_Entry.Sift_Pos /= Ancestor then
               -- We have been helped.
               Free (New_Entry);
               exit Phase_4;
            end if;

            -- Read new ancestor entry.
            Read(Queue,
                 Index     => New_Ancestor,
                 Op_ID     => Op_ID,
                 The_Entry => New_Anc_Entry,
                 Helped    => Helped);

            -- Exit if helped.
            if Helped or else
              ((New_Anc_Entry.Status /= SIFTING_2 and New_Ancestor /= Leaf) or
               (Leaf_Entry.Status /= SIFTING_2 and
                Leaf_Entry.Status /= SWAP_WITH_ANC)) then

               -- We have been helped.
               Free (New_Entry);
               exit Phase_4;
            end if;

            if Leaf /= New_Ancestor then
               -- Update sift position for the leaf.

               New_Entry.all      := Leaf_Entry.all;
               New_Entry.Status   := SIFTING_2;
               New_Entry.Sift_Pos := New_Ancestor;

            else
               -- We are finished.
               New_Entry.all      := Leaf_Entry.all;
               New_Entry.Status   := STABLE;
               New_Entry.Sift_Pos := Heap_Index'Last;
            end if;

            exit when CAS (Target    => Queue.Heap (Leaf)'Access,
                           Old_Value => Leaf_Entry,
                           New_Value => New_Entry);
         end loop Phase_4;
      end;
   end Implicit_Sift_Down;

   ----------------------------------------------------------------------------
   -- Stabilize_Heap.
   -- Use only for debugging purposes.
   procedure Stabilize_Heap (Queue : in out Priority_Queue_Type) is
      Status    : Heap_Status_Access;
      Old_Entry : Heap_Entry_Access;
      New_Entry : Heap_Entry_Access;
      Helped    : Boolean;
   begin
      Status := Queue.Status;
      for I in Queue.Heap'Range loop
         New_Entry := new Heap_Entry;
         loop
            Read_And_Fix (Queue,
                          Index      => I,
                          Op_ID      => Status.Op_ID + 1,
                          Old_Entry  => Old_Entry,
                          Clean_Copy => New_Entry.all,
                          Helped     => Helped);

            if Old_Entry /= null then
               Ada.Text_IO.Put_Line ("Stabilize_Heap:");
               Ada.Text_IO.Put_Line (Image (Queue));
               Ada.Text_IO.Skip_Line;

               if Helped then
                  Free (New_Entry);
                  exit;
               end if;

               exit when CAS (Target    => Queue.Heap (I)'Access,
                              Old_Value => Old_Entry,
                              New_Value => New_Entry);
            else
               exit;
            end if;
         end loop;
      end loop;
   end Stabilize_Heap;

end Non_Blocking_Priority_Queue;
