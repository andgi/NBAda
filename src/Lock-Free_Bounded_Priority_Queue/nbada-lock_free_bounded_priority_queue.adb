-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.adb
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 12:15:16 2002
-- $Id: nbada-lock_free_bounded_priority_queue.adb,v 1.26 2007/09/04 09:42:38 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Characters.Latin_1;
with Ada.Text_IO;

package body Lock_Free_Bounded_Priority_Queue is

   ----------------------------------------------------------------------------
   procedure Free is new Ada.Unchecked_Deallocation (Element_Type,
                                                     Element_Access);

   ----------------------------------------------------------------------------
   --  Internal functions.

   subtype Real_Operation_Type is Operation_Type range INSERT .. DELETE_MIN;

   procedure Enter_PP (Queue      : in out Priority_Queue_Type;
                       Op         : in     Real_Operation_Type;
                       Arg        : in     Element_Access;
                       New_Status :    out Heap_Status);
   --  Enter preliminary phase.
   procedure Exit_PP (Queue : in out Priority_Queue_Type;
                      Op_ID : in     Operation_ID);
   --  Exit preliminary phase.

   --  Preliminary phase of Insert.
   procedure Insert_PP (Queue  : in out Priority_Queue_Type;
                        Status : in     Heap_Status);
   --  Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status);
   --  Read and fix a heap entry, ie complete any unfinished older
   --  operations on it, except for parts of Op_ID (to avoid recursive
   --  breakdown).
   --  Helped is true if the Op_ID of the heap entry is larger than or
   --  equal to Op_ID.
   --  The Old_Entry and Clean_Copy is invalid when Helped is true.
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Helped     :    out Boolean;
                           Old_Entry  :    out Heap_Entry;
                           Ignore_SIFTING_2_Leaf : in Boolean := False);
   --  Read a heap entry.
   --  Will attempt to detect helping. A null entry is interpreted as helped.
   --  The_Entry is invalid if helped.
   procedure Read (Queue      : in out Priority_Queue_Type;
                   Index      : in     Heap_Index;
                   Op_ID      : in     Operation_ID;
                   The_Entry  :    out Heap_Entry;
                   Helped     :    out Boolean;
                   Ignore_SIFTING_2_Leaf : in Boolean := False);
   pragma Inline (Read);
   pragma Inline_Always (Read);

   --  Sort two heap entries. Used in the sifting phase of delete min.
   --  Assumes that the parent is already involved.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean);

   --  Implicit_Sift_Down.
   --  Used by Insert and Read_And_Fix to perform one sift down step.
   --  The leaf and the ancestor should already be involved in sifting.
   procedure Implicit_Sift_Down (Queue    : in out Priority_Queue_Type;
                                 Leaf     : in     Heap_Index;
                                 Ancestor : in     Heap_Index;
                                 Op_ID    : in     Operation_ID);


   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Initialize (Queue   : in out Priority_Queue_Type) is
   begin
      declare
         use Heap_Status_LL_SC;
         Tmp : Heap_Status;
      begin
         Initialize (Queue.Status, Tmp);
      end;
      declare
         use Heap_Entry_LL_SC;
         Tmp : Heap_Entry;
      begin
         for I in Queue.Heap'Range loop
            Initialize (Queue.Heap (I), Tmp);
         end loop;
      end;
   end Initialize;

   ----------------------------------------------------------------------------
   procedure Insert (Queue   : in out Priority_Queue_Type;
                     Element : in     Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status;
      Debug  : constant Boolean := False;
   begin
      Key := new Element_Type'(Element);

      --  Enter preliminary phase.
      Enter_PP (Queue,
                Op         => INSERT,
                Arg        => Key,
                New_Status => Status);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Entered PP.");
      end if;

      --  Perform preliminary insert phase.
      Insert_PP (Queue,
                 Status => Status);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Done PP.");
      end if;

      --  Exit preliminary phase.
      Exit_PP (Queue, Status.Op_ID);
      if Debug then
         Ada.Text_IO.Put_Line ("Insert: Exited PP.");
      end if;

      Free (Key);

      --  Sift phase.
      --  Uses lazy sifting for now.
--       declare
--          Done  : Boolean := False;
--       begin
--          while not Done loop
--  --            Ada.Text_IO.Put_Line ("Insert: ");
--  --            Ada.Text_IO.Put_Line (Image (Queue));
--             Implicit_Sift_Down
--               (Queue,
--                Leaf  => Status.Size,
--                Op_ID => Status.Op_ID,
--                Done  => Done);
--  --            Ada.Text_IO.Put_Line ("Insert: Continue?");
--  --            Ada.Text_IO.Skip_Line;
--          end loop;
--       end;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete_Min (Queue   : in out Priority_Queue_Type;
                         Element :    out Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status;
      Sift   : Boolean;
   begin
      Key := new Element_Type;

      --  Enter preliminary phase.
      Enter_PP (Queue,
                Op         => DELETE_MIN,
                Arg        => Key,
                New_Status => Status);

      --  Perform preliminary delete min phase.
      Delete_Min_PP (Queue,
                     Status => Status);

      --  Sift only if there are nodes left.
      Sift := Status.Size > 0;

      --  Exit preliminary phase.
      Exit_PP (Queue, Status.Op_ID);

      --  Sift phase.
      --  Uses lazy sifting for now.
--       declare
--          Parent : Heap_Index := 1;
--          Done   : Boolean;
--       begin
--          while Sift loop
--             --Ada.Text_IO.Put_Line ("Delete_Min: ");
--             --Ada.Text_IO.Put_Line (Image (Queue));

         --  We must sift at least one step to make the root stable.
         --  No, we already do that in Delete_Min_PP.
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
   --  Image.
   function Image (Queue : Priority_Queue_Type) return String is

      use Ada.Characters.Latin_1;

      function Image (Status : Heap_Status) return String;
      function Image (E : Heap_Entry) return String;
      function Image (Heap : Heap_Array) return String;

      function Image (Status : Heap_Status) return String is
      begin
         return
           "(Size ="  & Natural'Image (Status.Size) &
           ", Op_ID = " & Operation_ID'Image (Status.Op_ID) &
           ", Op_Type = " & Operation_Type'Image (Status.Op_Type) &
           ")";
      end Image;

      function Image (E : Heap_Entry) return String is
      begin
         return
           "(Key = " & Image (E.Key) &
           ", Status = " & Entry_Status'Image (E.Status) &
           ", Old_Key = " & Image (E.Old_Key) &
           ", Op_ID = " & Operation_ID'Image (E.Op_ID) &
           ", Sift_Pos = " & Heap_Index'Image (E.Sift_Pos) &
           ")";
      end Image;

      function Image (Heap : Heap_Array) return String is
         use Heap_Entry_LL_SC;
      begin
         case Heap'Length is
            when 0 =>
               return "";
            when 1 =>
               return Image (Load_Linked (Heap (Heap'First)));
            when others =>
               return
                 " " & Image (Load_Linked (Heap (Heap'First))) & ", " & LF &
                 Image (Heap (Heap'First + 1 .. Heap'Last));
         end case;
      end Image;

      use Heap_Status_LL_SC;

      Status : Heap_Status := Load_Linked (Queue.Status);
      Heap   : Heap_Array renames Queue.Heap;
   begin
      return "( " &
        Image (Status) & ", " & LF &
        "[" & Image (Heap) & "] )";
   end Image;

   ----------------------------------------------------------------------------
   --  Internal functions.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Is_Null (HE : Heap_Entry) return Boolean is
   begin
      return HE.Status = Nul;
   end Is_Null;

   ----------------------------------------------------------------------------
   procedure Enter_PP (Queue      : in out Priority_Queue_Type;
                       Op         : in     Real_Operation_Type;
                       Arg        : in     Element_Access;
                       New_Status :    out Heap_Status) is
      use type Primitives.Unsigned_32;
      use Heap_Status_LL_SC;
      Status : Heap_Status;
   begin
      loop
         --  Help any ongoing preliminary phases of operations.
         loop
            --  Read heap status.
            Status := Load_Linked (Queue.Status);

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

         --  Attempt to start preliminary phase

         --  Set up new status record.
         New_Status.Op_Type := Op;
         New_Status.Op_ID   := Status.Op_ID + 1;
         case Op is
            when INSERT =>
               if Status.Size < Queue.Max_Size then
                  New_Status.Size := Status.Size + 1;
               else
                  raise Queue_Full;
               end if;

            when DELETE_MIN =>
               if Status.Size > 0 then
                  New_Status.Size := Status.Size - 1;
               else
                  raise Queue_Empty;
               end if;
         end case;
         New_Status.Op_Arg := Arg;

         --  Commit status record.
         exit when Store_Conditional (Target => Queue.Status,
                                      Value  => New_Status);
      end loop;
   end Enter_PP;

   ----------------------------------------------------------------------------
   --  Exit preliminary phase.
   --  Must be changed to recognize if it has been helped.
   procedure Exit_PP (Queue : in out Priority_Queue_Type;
                      Op_ID : in     Operation_ID) is
      use Heap_Status_LL_SC;
      Status     : Heap_Status;
      New_Status : Heap_Status;
   begin
      loop
         --  Read heap status.
         Status := Load_Linked (Queue.Status);

         if Status.Op_ID > Op_ID or Status.Op_Type = NONE then
            exit;
         end if;

         --  Set up new status record.
         New_Status.Op_Type := NONE;
         New_Status.Op_ID   := Status.Op_ID;
         New_Status.Size    := Status.Size;
         New_Status.Op_Arg  := null;

         --  Commit status record.
         exit when Store_Conditional (Target => Queue.Status,
                                      Value  => New_Status);
      end loop;
   end Exit_PP;

   ----------------------------------------------------------------------------
   --  Preliminary phase of Insert.
   procedure Insert_PP (Queue  : in out Priority_Queue_Type;
                        Status : in     Heap_Status) is
   begin
      --  Phase 1: Add new leaf.
      declare
         use Heap_Entry_LL_SC;
         New_Leaf : Heap_Entry;
      begin
         if Is_Null (Load_Linked (Target => Queue.Heap (Status.Size))) then
            New_Leaf.Key      := Status.Op_Arg.all;
            New_Leaf.Status   := SIFTING_2;
            New_Leaf.Op_ID    := Status.Op_ID;
            New_Leaf.Sift_Pos := Heap_Index'First;

            Store_Conditional (Target => Queue.Heap (Status.Size),
                               Value  => New_Leaf);
         else
            --  We must have been helped.
            --  Continue.
            null;
         end if;
      end;

      --  Phase 2: Update root.
      declare
         use Heap_Entry_LL_SC;
         Root     : Heap_Entry;
         New_Root : Heap_Entry;
         Helped   : Boolean;
      begin
         Phase_2 : loop
            --  Read root.
            Read_And_Fix (Queue,
                          Index      => Heap_Index'First,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Root,
                          Clean_Copy => New_Root,
                          Helped     => Helped);

            --  Skip if helped.
            if (Helped or Is_Null (Root)) or else
              (Root.Op_ID = Status.Op_ID and Root.Status = SIFTING_2)
            then
               exit Phase_2;
            end if;

            --  Update root.
            New_Root.Status   := SIFTING_2;
            New_Root.Op_ID    := Status.Op_ID;
            New_Root.Sift_Pos := Status.Size;

            --  Commit root.
            exit Phase_2 when
              Store_Conditional (Target => Queue.Heap (Heap_Index'First),
                                 Value  => New_Root);
         end loop Phase_2;
      end;

   end Insert_PP;

   ----------------------------------------------------------------------------
   --  Preliminary phase of Delete_Min.
   procedure Delete_Min_PP (Queue  : in out Priority_Queue_Type;
                            Status : in     Heap_Status) is
      Debug : constant Boolean := False;
   begin
      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Initial.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;

      --  Phase 1: Mark highest leaf DELETED.
      declare
         use Heap_Entry_LL_SC;
         Leaf     : Heap_Entry;
         New_Leaf : Heap_Entry;
         Helped   : Boolean;
      begin
         --  Fix the root to avoid problems if the leaf is directly
         --  below root. This only needs to be done once since no other
         --  preliminary phase may interfere with us.
         --  In fact this must always be done, since otherwise we might
         --  accidentaly exclude the smallest element in the heap from
         --  the helping by marking it as deleted.
         declare
            New_Root : Heap_Entry;
            Old_Root : Heap_Entry;
         begin
            Read_And_Fix (Queue,
                          Index      => Queue.Heap'First,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Old_Root,
                          Clean_Copy => New_Root,
                          Helped     => Helped);
         end;

         Phase_1 : loop
            --  Read leaf.
            Read_And_Fix (Queue,
                          Index      => Status.Size + 1,
                          Op_ID      => Status.Op_ID,
                          Old_Entry  => Leaf,
                          Clean_Copy => New_Leaf,
                          Helped     => Helped);

            --  Skip if helped.
            if Is_Null (Leaf) or Helped then
               exit Phase_1;
            end if;

            --  Set leaf status to DELETED.
            New_Leaf.Status  := DELETED;
            New_Leaf.Op_ID   := Status.Op_ID;
            New_Leaf.Old_Key := New_Leaf.Key;

            exit Phase_1 when
              Store_Conditional (Target => Queue.Heap (Status.Size + 1),
                                 Value  => New_Leaf);
         end loop Phase_1;
      end;

      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Phase 1.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;

      --  Phase 2: Update root.
      declare
         use Heap_Entry_LL_SC;
         Root     : Heap_Entry;
         Leaf     : Heap_Entry;
         New_Root : Heap_Entry;
         Helped_R, Helped_L : Boolean;
      begin
         if Status.Size > 0 then
            Phase_2 : loop
               --  Read root.
               --  This is done with helping Fix function although the
               --  root is fixed above.
               Read_And_Fix (Queue,
                             Index      => Heap_Index'First,
                             Op_ID      => Status.Op_ID,
                             Old_Entry  => Root,
                             Clean_Copy => New_Root,
                             Helped     => Helped_R);

               --  Read deleted leaf.
               Read (Queue,
                     Index     => Status.Size + 1,
                     Op_ID     => Status.Op_ID,
                     The_Entry => Leaf,
                     Helped    => Helped_L);

               --  Exit if helped.
               if (Helped_R or Helped_L) or else
                 (Leaf.Status /= DELETED) then
                  exit Phase_2;
               end if;

               --  Safety checks!
               if Is_Null (Root) then
                  Ada.Text_IO.Put_Line ("Delete_Min_PP: Null root.");
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;
               if New_Root.Status /= STABLE then
                  Ada.Text_IO.Put_Line ("Delete_Min_PP: Unstable root.");
                  Ada.Text_IO.Put_Line (Image (Queue));
                  raise Constraint_Error;
               end if;

               --  Store root key.
               Status.Op_Arg.all := New_Root.Key;
               Primitives.Membar;

               --  Update new root.
               New_Root          := Leaf;
               New_Root.Status   := SIFTING_1;
               New_Root.Op_ID    := Status.Op_ID;

               --  Commit root.
               exit Phase_2 when
                 Store_Conditional (Target => Queue.Heap (Heap_Index'First),
                                    Value  => New_Root);
            end loop Phase_2;
         else
            Read (Queue,
                  Index     => Queue.Heap'First,
                  Op_ID     => Status.Op_ID,
                  The_Entry => Root,
                  Helped    => Helped_R);

            --  Store min if we haven't been helped.
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

      --  Phase 3: Remove deleted leaf.
      declare
         use Heap_Entry_LL_SC;
         Leaf   : Heap_Entry;
         Helped : Boolean;
      begin
         Phase_3 : loop
            --  Read deleted leaf.
            Read (Queue,
                  Index     => Status.Size + 1,
                  Op_ID     => Status.Op_ID,
                  The_Entry => Leaf,
                  Helped    => Helped);

            --  Skip if helped:
            exit Phase_3 when Helped;

            --  Commit empty leaf.
            Leaf.Status := NUL;
            exit Phase_3 when
              Store_Conditional (Target => Queue.Heap (Status.Size + 1),
                                 Value  => Leaf);
         end loop Phase_3;
      end;

      if Debug then
         Ada.Text_IO.Put_Line ("Delete_Min_PP: Phase 3.");
         Ada.Text_IO.Put_Line (Image (Queue));
      end if;
   end Delete_Min_PP;

   ----------------------------------------------------------------------------
   --  Old_Entry = null indicates that we read an empty slot.
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Op_ID      : in     Operation_ID;
                           Clean_Copy : in out Heap_Entry;
                           Helped     :    out Boolean;
                           Old_Entry  :    out Heap_Entry;
                           Ignore_SIFTING_2_Leaf : in Boolean := False) is
      use Heap_Entry_LL_SC;
      Done  : Boolean;
      Debug : constant Boolean := False;
   begin
      Helped := False;

      --  Read entry. Needs to be repeated until we read it stable.
      --  This might not be really optimal since any helping
      --  is commited before we return the clean copy.
      Help : loop
         --  Read current entry value.
         if Index <= Queue.Max_Size then
            Old_Entry := Load_Linked (Queue.Heap (Index));
         else
            if Debug then
               Ada.Text_IO.Put_Line
                 ("Read_And_Fix: Node position too large: " &
                  Heap_Index'Image (Index) &
                  " op " & Operation_ID'Image (Op_ID));
            end if;
            Old_Entry.Status := Nul;
            return;
         end if;

         if Is_Null (Old_Entry) then
            if Debug then
               Ada.Text_IO.Put_Line ("Read_And_Fix: Read null entry!");
            end if;

            return;
         end if;

         --  Check if there is a pending operation.
         --  Think HARD on whether this is the RIGHT WAY to detect this.
         if Old_Entry.Op_ID < Op_ID then
            case Old_Entry.Status is
               ----------------------------------------------------------------
               when STABLE =>
                  --  Return copy.
                  Clean_Copy := Old_Entry;
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
                        " at" & Heap_Index'Image (Index) & ".");
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
                        " at" & Heap_Index'Image (Index) & ".");
                  end if;
                  if Old_Entry.Sift_Pos >= Index then
                     --  We hit the active ancestor to a SIFTING_2 leaf.
                     Implicit_Sift_Down
                       (Queue,
                        Leaf     => Old_Entry.Sift_Pos,
                        Ancestor => Index,
                        Op_ID    => Old_Entry.Op_ID);
                  else
                     --  We hit a SIFTING_2 leaf.
                     if Ignore_SIFTING_2_Leaf then
                        --  Treat it as an empty slot.
                        Old_Entry.Status := NUL;
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
                        " at" & Heap_Index'Image (Index) & ".");
                  end if;

                  Sort_Parent_Child
                    (Queue,
                     Parent => Index / 2,
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
                        " at" & Heap_Index'Image (Index) & ".");
                  end if;

                  if Old_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf then
                     --  We hit a SIFTING_2 leaf with parent above.
                     --  Treat it as an empty slot.
                     Old_Entry.Status := NUL;
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
                  --  We hit an ongoing preliminary phase.
                  --  Treat as empty.
                  Old_Entry.Status := NUL;
                  return;

               ----------------------------------------------------------------
               when others =>
                  --  Helping not implemented!
                  Ada.Text_IO.Put_Line
                    ("Read_And_Fix: Operation" &
                     Operation_ID'Image (Op_ID) &
                     " hit " &
                     Entry_Status'Image (Old_Entry.Status) &
                     " from " &
                     Operation_ID'Image (Old_Entry.Op_ID) &
                     " at" & Heap_Index'Image (Index) & ".");
                  Ada.Text_IO.Put_Line (Image (Queue));

                  raise Constraint_Error;
            end case;
         elsif Old_Entry.Op_ID = Op_ID then
            --  Since Read_And_Fix is only used before an operation has
            --  managed to update an entry, Old_Entry.Op_ID = Op_ID
            --  indicates that the operation has been helped.

            Old_Entry.Status := NUL;
            Helped := True;
            return;

--             case Old_Entry.Status is
--                when DELETED =>
--                   -- Ignore this entry.
--                   Old_Entry := null;

--                when STABLE =>
--                --  We must have been helped since the entry is STABLE and
--                --  Old_Entry.Op_ID = Op_ID, i.e. our op is finished with it.
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
            --  This operation has been helped.
            --  DOUBLE CHECK THIS DETECTION!!

            --  The helped operation should not use anything
            --  Read_And_Fix returns. The Clean copy is not guaranteed to
            --  be stable.

            --  Do not claim to be helped if we encounter and ignore a
            --  SIFTING_2 leaf.
            Helped     := (Old_Entry.Status /= SIFTING_2 and
                           Old_Entry.Status /= SWAP_WITH_ANC) or
              not (Old_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf);
            Old_Entry.Status := NUL;

            return;
         end if;
      end loop Help;
   end Read_And_Fix;

   ----------------------------------------------------------------------------
   --  Read.
   procedure Read (Queue      : in out Priority_Queue_Type;
                   Index      : in     Heap_Index;
                   Op_ID      : in     Operation_ID;
                   The_Entry  :    out Heap_Entry;
                   Helped     :    out Boolean;
                   Ignore_SIFTING_2_Leaf : in Boolean := False) is
      use Heap_Entry_LL_SC;
   begin
      The_Entry := Load_Linked (Queue.Heap (Index));

      --  Check if helped.
      Helped := Is_Null (The_Entry) or else
        (The_Entry.Op_ID > Op_ID and
         ((The_Entry.Status /= SIFTING_2 and
           The_Entry.Status /= SWAP_WITH_ANC) or
          not (The_Entry.Sift_Pos < Index and Ignore_SIFTING_2_Leaf)));
      Primitives.Membar;
   end Read;

   ----------------------------------------------------------------------------
   --  Sort two heap entries. Assumes that the Parent is in state SIFTING_1.
   procedure Sort_Parent_Child (Queue  : in out Priority_Queue_Type;
                                Parent : in     Heap_Index;
                                Op_ID  : in     Operation_ID;
                                Done   :    out Boolean) is
      Left_Child         : constant Heap_Index := 2 * Parent;
      Right_Child        : constant Heap_Index := Left_Child + 1;
      Child              : Heap_Index;
   begin
      Done := False;

      --  Phase 1: Set Child to SWAP_WITH_PARENT iff parent > smallest child.
      declare
         use Heap_Entry_LL_SC;
         Parent_Entry       : Heap_Entry;
         Left_Child_Entry   : Heap_Entry;
         Right_Child_Entry  : Heap_Entry;
         New_Left_Entry     : Heap_Entry;
         New_Right_Entry    : Heap_Entry;
         Child_Entry        : Heap_Entry;
         New_Entry          : Heap_Entry;
         Helped             : Boolean;
      begin
         Phase_1 : loop
            --  Read Parent.
            Read (Queue,
                  Index     => Parent,
                  Op_ID     => Op_ID,
                  The_Entry => Parent_Entry,
                  Helped    => Helped);
            if Helped or else Parent_Entry.Status /= SIFTING_1 then
               exit Phase_1;
            end if;

            --  Read left child.
            Read_And_Fix (Queue,
                          Index      => Left_Child,
                          Op_ID      => Op_ID,
                          Old_Entry  => Left_Child_Entry,
                          Clean_Copy => New_Left_Entry,
                          Helped     => Helped,
                          Ignore_SIFTING_2_Leaf => True);
            if Helped then
               --  We have been helped.
               exit Phase_1;
            end if;

            --  Read right child.
            Read_And_Fix (Queue,
                          Index      => Right_Child,
                          Op_ID      => Op_ID,
                          Old_Entry  => Right_Child_Entry,
                          Clean_Copy => New_Right_Entry,
                          Helped     => Helped,
                          Ignore_SIFTING_2_Leaf => True);
            if Helped then
               --  We have been helped.
               exit Phase_1;
            end if;

            --  This operation is still unfinished.

            --  Select child.
            if
              not Is_Null (Left_Child_Entry) and
              not Is_Null (Right_Child_Entry)
            then
               if Right_Child_Entry.Key > Left_Child_Entry.Key then
                  Child_Entry := Left_Child_Entry;
                  Child       := Left_Child;
                  New_Entry   := New_Left_Entry;
               else
                  Child_Entry := Right_Child_Entry;
                  Child       := Right_Child;
                  New_Entry   := New_Right_Entry;
               end if;
            elsif not Is_Null (Left_Child_Entry) then
               Child_Entry := Left_Child_Entry;
               Child       := Left_Child;
               New_Entry   := New_Left_Entry;
            else
               Child_Entry := Right_Child_Entry;
               Child       := Right_Child;
               New_Entry   := New_Right_Entry;
            end if;

            if not Is_Null (Child_Entry) then
               --  There is a child.

               if Child_Entry.Status = STABLE then

                  if Parent_Entry.Key > New_Entry.Key then
                     --  Swap.
                     --  Prepare new Child entry.
                     New_Entry.Status  := SWAP_WITH_PARENT;
                     New_Entry.Old_Key := Child_Entry.Key;
                     New_Entry.Key     := Parent_Entry.Key;
                     New_Entry.Op_ID   := Op_ID;
                  else
                     --  Update Op_ID.
                     --  Prepare new Child entry.
                     New_Entry         := Child_Entry;
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
               --  No child, so we are finished.
               Done := True;
               exit Phase_1;
            end if;

            if Store_Conditional (Target => Queue.Heap (Child),
                                  Value  => New_Entry) then
               exit Phase_1;
            end if;
         end loop Phase_1;
      end;

      --  Step 2: Update Parent.
      declare
         use Heap_Entry_LL_SC;
         Parent_Entry : Heap_Entry;
         Child_Entry  : Heap_Entry;
         New_Entry    : Heap_Entry;
         Helped       : Boolean;
      begin
         Phase_2 : loop
            --  Read Parent.
            Read (Queue,
                  Index     => Parent,
                  Op_ID     => Op_ID,
                  The_Entry => Parent_Entry,
                  Helped    => Helped);
            if Helped or else Parent_Entry.Status /= SIFTING_1 then
               exit Phase_2;
            end if;

            --  Read the selected child.
            if Child in Queue.Heap'Range then
               Read (Queue,
                     Index     => Child,
                     Op_ID     => Op_ID,
                     The_Entry => Child_Entry,
                     Helped    => Helped,
                     Ignore_SIFTING_2_Leaf => True);
            else
               Child_Entry.Status := NUL;
               Helped             := False;
            end if;
            --  In this case no child is good news and means that we can
            --  mark the parent as stable.
            if
              (Helped and not Is_Null (Child_Entry)) or
              (not Is_Null (Child_Entry)
               and then Child_Entry.Status = SIFTING_1)
            then
               exit Phase_2;
            end if;

            --  This operation is still unfinished.
            --  Check whether we have a child entry or not.
            --  For some child states the child should be treated as empty.
            if not Is_Null (Child_Entry) and then not
              (Child_Entry.Status = SIFTING_2 or
               Child_Entry.Status = SWAP_WITH_ANC)
            then

               if Child_Entry.Status = SWAP_WITH_PARENT then
                  --  Swap parent key and make stable.
                  --  Prepare new Parent entry.
                  New_Entry := Parent_Entry;

                  New_Entry.Status  := STABLE;
                  New_Entry.Key     := Child_Entry.Old_Key;

               elsif Child_Entry.Status = STABLE then
                  --  Mark parent stable.
                  --  Prepare new Parent entry.
                  New_Entry := Parent_Entry;

                  New_Entry.Status  := STABLE;

                  Done := True;
               else
                  --  Unknown state!
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
               --  No child. We are done.
               --  Mark parent stable.
               New_Entry := Parent_Entry;
               New_Entry.Status  := STABLE;

               Done := True;
            end if;

            exit Phase_2 when Store_Conditional (Target => Queue.Heap (Parent),
                                                 Value  => New_Entry);
         end loop Phase_2;
      end;

      --  Step 3: Finish Child.
      declare
         use Heap_Entry_LL_SC;
         Child_Entry        : Heap_Entry;
         New_Entry          : Heap_Entry;
         Helped             : Boolean := True;
      begin
         Phase_3 : loop
            --  Read Child entry.
            if Child in Queue.Heap'Range then
               Read (Queue,
                     Index     => Child,
                     Op_ID     => Op_ID,
                     The_Entry => Child_Entry,
                     Helped    => Helped);
            else
               Child_Entry.Status := Nul;
            end if;
            --  Exit if the child has been helped or is nonexistant.
            --  We should probably set Done to true here.
            if Helped or else (Child_Entry.Status /= SWAP_WITH_PARENT) then
               exit Phase_3;
            end if;

            --  This operation is still unfinished.

            if Child_Entry.Status = SWAP_WITH_PARENT then
               --  Prepare new Child entry.
               New_Entry := Child_Entry;
               New_Entry.Status := SIFTING_1;
            else
               --  Nothing to be done. The child should be stable
               --  and according to the algorithm we can end the sift phase.
               exit Phase_3;
            end if;

            exit Phase_3 when Store_Conditional (Target => Queue.Heap (Child),
                                                 Value  => New_Entry);
         end loop Phase_3;
      end;
   end Sort_Parent_Child;

   ----------------------------------------------------------------------------
   --  Implicit_Sift_Down.
   procedure Implicit_Sift_Down (Queue    : in out Priority_Queue_Type;
                                 Leaf     : in     Heap_Index;
                                 Ancestor : in     Heap_Index;
                                 Op_ID    : in     Operation_ID) is

      function Next_Ancestor (Child, Ancestor : Heap_Index)
                             return Heap_Index;

      function Next_Ancestor (Child, Ancestor : Heap_Index)
                             return Heap_Index is
         Next : Heap_Index := Child;
      begin
         while Next > Ancestor loop
            if Next / 2 = Ancestor then
               return Next;
            end if;
            Next := Next / 2;
         end loop;
         return Child;
      end Next_Ancestor;

      --  The new ancestor index.
      New_Ancestor : constant Heap_Index := Next_Ancestor (Leaf, Ancestor);
   begin

      --  Phase 1: Update leaf for swap
      declare
         use Heap_Entry_LL_SC;
         New_Entry  : Heap_Entry;
         Anc_Entry  : Heap_Entry;
         Leaf_Entry : Heap_Entry;
         Helped     : Boolean;
      begin
         Phase_1 : loop
            --  Read leaf entry.
            Read (Queue,
                  Index     => Leaf,
                  Op_ID     => Op_ID,
                  The_Entry => Leaf_Entry,
                  Helped    => Helped);

            --  Check if helped.
            if Helped then
               exit Phase_1;
            end if;

            --  Read ancestor entry.
            Read (Queue,
                  Index     => Ancestor,
                  Op_ID     => Op_ID,
                  The_Entry => Anc_Entry,
                  Helped    => Helped);

            --  Exit if helped.
            if Helped or else (Anc_Entry.Status    /= SIFTING_2 or
                               Leaf_Entry.Status   /= SIFTING_2 or
                               Leaf_Entry.Sift_Pos /= Ancestor)
            then
               --  We have been helped. Try to increase sift_pos (but not yet).
               exit Phase_1;
            end if;

            --  The leaf's status is ok because of Op_Id
            if Anc_Entry.Key > Leaf_Entry.Key then
               --  Swap.
               --  Prepare new Leaf entry.

               New_Entry         := Leaf_Entry;
               New_Entry.Status  := SWAP_WITH_ANC;
               New_Entry.Old_Key := Leaf_Entry.Key;
               New_Entry.Key     := Anc_Entry.Key;
               New_Entry.Op_ID   := Leaf_Entry.Op_ID;

            elsif Leaf_Entry.Sift_Pos = Leaf then
               --  Done sifting. Mark leaf as stable.

               New_Entry        := Leaf_Entry;
               New_Entry.Status := STABLE;

            else
               --  Don't swap, increase the leaf's sift_pos (but not yet).
               exit Phase_1;
            end if;

            exit Phase_1 when Store_Conditional (Target => Queue.Heap (Leaf),
                                                 Value  => New_Entry);
         end loop Phase_1;
      end;

      --  Phase 2: Update new ancestor.
      declare
         use Heap_Entry_LL_SC;
         New_Entry     : Heap_Entry;
         Leaf_Entry    : Heap_Entry;
         New_Anc_Entry : Heap_Entry;
         Helped        : Boolean;
      begin
         Phase_2 : loop
            --  Read leaf.
            Read (Queue,
                  Index     => Leaf,
                  Op_ID     => Op_ID,
                  The_Entry => Leaf_Entry,
                  Helped    => Helped);

            --  Check if helped.
            if Helped or else (Leaf_Entry.Sift_Pos /= Ancestor or
                               Leaf_Entry.Status = STABLE) then
               --  We have been helped (or are finished).
               exit Phase_2;
            end if;

            --  Is the New_Ancestor and the Leaf the same?
            if Leaf = New_Ancestor then
               --  Do nothing.
               exit Phase_2;
            end if;

            --  Read new ancestor.
            Read_And_Fix (Queue,
                          Index      => New_Ancestor,
                          Op_ID      => Op_ID,
                          Old_Entry  => New_Anc_Entry,
                          Clean_Copy => New_Entry,
                          Helped     => Helped);

            --  Exit if helped.
            if (Helped or Is_Null (New_Anc_Entry)) or else
--              ((Leaf_Entry.Status /= SWAP_WITH_ANC and
--              Leaf_Entry.Status /= SIFTING_2) or
              (New_Anc_Entry.Status /= STABLE)
            then
               if not Is_Null (New_Anc_Entry) and then
                 (New_Anc_Entry.Status /= STABLE and
                  New_Anc_Entry.Op_ID < Op_ID)
               then
                  Ada.Text_IO.Put_Line ("Read_And_Fix faild to clean " &
                                        Heap_Index'Image (New_Ancestor) & "!");
                  raise Constraint_Error;
               end if;

               --  We have been helped.
               exit Phase_2;
            end if;

            --  Prepare new ancestor entry.
            New_Entry          := New_Anc_Entry;
            New_Entry.Status   := SIFTING_2;
            New_Entry.Op_ID    := Leaf_Entry.Op_ID;
            New_Entry.Sift_Pos := Leaf;

            exit Phase_2 when
              Store_Conditional (Target => Queue.Heap (New_Ancestor),
                                 Value  => New_Entry);
         end loop Phase_2;
      end;

      --  Phase 3: Update old ancestor.
      declare
         use Heap_Entry_LL_SC;
         New_Entry     : Heap_Entry;
         Anc_Entry     : Heap_Entry;
         Leaf_Entry    : Heap_Entry;
         New_Anc_Entry : Heap_Entry;
         Helped        : Boolean;
      begin
         Phase_3 : loop
            --  Read ancestor and leaf entries.
            Read (Queue,
                  Index     => Leaf,
                  Op_ID     => Op_ID,
                  The_Entry => Leaf_Entry,
                  Helped    => Helped);

            --  Check if helped.
            if Helped or else Leaf_Entry.Sift_Pos /= Ancestor then
               --  We have been helped.
               exit Phase_3;
            end if;

            --  Read ancestor.
            Read (Queue,
                  Index     => Ancestor,
                  Op_ID     => Op_ID,
                  The_Entry => Anc_Entry,
                  Helped    => Helped);

            --  Exit if helped.
            if Helped or else Anc_Entry.Status /= SIFTING_2 then
               --  We have been helped.
               exit Phase_3;
            end if;

            Read (Queue,
                  Index     => New_Ancestor,
                  Op_ID     => Op_ID,
                  The_Entry => New_Anc_Entry,
                  Helped    => Helped);

            --  Exit if helped.
            if Helped or else
              (New_Anc_Entry.Status /= SIFTING_2 and New_Ancestor /= Leaf)
            then
               --  We have been helped.
               exit Phase_3;
            end if;

            --  There is work to do.

            if Leaf_Entry.Status = SIFTING_2 then
               --  Mark old ancestor as stable.

               New_Entry        := Anc_Entry;
               New_Entry.Status := STABLE;

            elsif Leaf_Entry.Status = SWAP_WITH_ANC then
               --  Update key and mark old ancestor as stable

               New_Entry        := Anc_Entry;
               New_Entry.Key    := Leaf_Entry.Old_Key;
               New_Entry.Status := STABLE;

            else
               --  We have been helped. (Should never get here.)
               exit Phase_3;
            end if;

            exit Phase_3 when
              Store_Conditional (Target => Queue.Heap (Ancestor),
                                 Value  => New_Entry);
         end loop Phase_3;
      end;

      --  Phase 4: Update leaf for new ancestor.
      declare
         use Heap_Entry_LL_SC;
         New_Entry     : Heap_Entry;
         Leaf_Entry    : Heap_Entry;
         New_Anc_Entry : Heap_Entry;
         Helped        : Boolean;
      begin
         Phase_4 : loop
            --  Read leaf entry.
            Read (Queue,
                  Index     => Leaf,
                  Op_ID     => Op_ID,
                  The_Entry => Leaf_Entry,
                  Helped    => Helped);

            --  Check if helped.
            if Helped or else (Leaf_Entry.Sift_Pos /= Ancestor or
                               (Leaf_Entry.Status /= SIFTING_2 and
                                Leaf_Entry.Status /= SWAP_WITH_ANC))
            then
               --  We have been helped.
               exit Phase_4;
            end if;

            --  Read new ancestor entry.
            Read (Queue,
                  Index     => New_Ancestor,
                  Op_ID     => Op_ID,
                  The_Entry => New_Anc_Entry,
                  Helped    => Helped);

            --  Exit if helped.
            if Helped or else
              (New_Anc_Entry.Status /= SIFTING_2 and New_Ancestor /= Leaf)
            then
               --  We have been helped.
               exit Phase_4;
            end if;

            if Leaf /= New_Ancestor then
               --  Update sift position for the leaf.

               New_Entry          := Leaf_Entry;
               New_Entry.Status   := SIFTING_2;
               New_Entry.Sift_Pos := New_Ancestor;

            else
               --  We are finished.
               New_Entry          := Leaf_Entry;
               New_Entry.Status   := STABLE;
               New_Entry.Sift_Pos := Heap_Index'Last;
            end if;

            exit Phase_4 when Store_Conditional (Target => Queue.Heap (Leaf),
                                                 Value  => New_Entry);
         end loop Phase_4;
      end;
   end Implicit_Sift_Down;

   ----------------------------------------------------------------------------
   --  Stabilize_Heap.
   --  Use only for debugging purposes.
   procedure Stabilize_Heap (Queue : in out Priority_Queue_Type) is
      use Heap_Status_LL_SC;
      use Heap_Entry_LL_SC;
      Status    : Heap_Status;
      Old_Entry : Heap_Entry;
      New_Entry : Heap_Entry;
      Helped    : Boolean;
   begin
      Status := Load_Linked (Queue.Status);
      for I in Queue.Heap'Range loop
         loop
            Read_And_Fix (Queue,
                          Index      => I,
                          Op_ID      => Status.Op_ID + 1,
                          Old_Entry  => Old_Entry,
                          Clean_Copy => New_Entry,
                          Helped     => Helped);

            if not Is_Null (Old_Entry) then
               Ada.Text_IO.Put_Line ("Stabilize_Heap:");
               Ada.Text_IO.Put_Line (Image (Queue));
               Ada.Text_IO.Skip_Line;

               if Helped then
                  exit;
               end if;

               exit when Store_Conditional (Target => Queue.Heap (I),
                                            Value  => New_Entry);
            else
               exit;
            end if;
         end loop;
      end loop;
   end Stabilize_Heap;

end Lock_Free_Bounded_Priority_Queue;
