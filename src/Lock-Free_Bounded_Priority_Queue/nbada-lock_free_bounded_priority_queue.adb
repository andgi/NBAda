-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : non_blocking_priority_queue.adb
-- Description     : Non-blocking priority queue.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jul 11 12:15:16 2002
-- $Id: nbada-lock_free_bounded_priority_queue.adb,v 1.1 2002/07/12 15:03:40 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
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
   -- Read and fix a heap entry, ie complete any unfinished operations on it.
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Clean_Copy : in     Heap_Entry_Access;
                           Old_Entry  :    out Heap_Entry_Access);

   -- Sort two heap entries. Used in the sifting phase of delete min.
   procedure Sort_Parent_Child (Queue : in out Priority_Queue_Type;
                                Child : in     Heap_Index;
                                Op_ID : in     Operation_ID;
                                Done  :    out Boolean);
   --

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

      -- Perform preliminary insert phase.
      Insert_PP (Queue,
                 Status => Status);

      -- Exit preliminary phase.
      Exit_PP (Queue);
      Free (Key);

      -- Sift phase.
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete_Min (Queue   : in out Priority_Queue_Type;
                         Element :    out Element_Type) is
      Key    : Element_Access;
      Status : Heap_Status_Access;
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

      -- Exit preliminary phase.
      Exit_PP (Queue);

      -- Sift phase.
      declare
         Done  : Boolean := False;
         -- Should choose left or right child with equal probability.
         Child : Heap_Index := 2;
      begin
         while Child <= Queue.Max_Size and not Done loop
            Sort_Parent_Child (Queue,
                               Child => Child,
                               Op_ID => Status.Op_ID,
                               Done  => Done);

            -- Should choose left or right child with equal probability.
            Child := 2 * Child;
         end loop;
      end;

      Element := Key.all;
      Free (Key);
   end Delete_Min;

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
               New_Status.Size := Status.Size + 1;

            when DELETE_MIN =>
               New_Status.Size := Status.Size - 1;
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
      -- Add new leaf.
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

      -- Update root.
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
                          Old_Entry  => Root,
                          Clean_Copy => New_Root);

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

      -- Remove highest leaf.
      loop
         -- Read leaf.
         -- This should probably be done through the helping Fix
         -- function.
         Read_And_Fix (Queue,
                       Index      => Status.Size + 1,
                       Old_Entry  => Leaf,
                       Clean_Copy => New_Leaf);

         -- Remove leaf.
         exit when CAS (Target    => Queue.Heap (Status.Size + 1)'Access,
                        Old_Value => Leaf,
                        New_Value => null);
      end loop;

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
                             Old_Entry  => Root,
                             Clean_Copy => New_Root);

               -- Store root key.
               Status.Op_Arg.all := New_Root.Key;

               -- Update root.
               New_Root.all      := New_Leaf.all;
               New_Root.Status   := SIFTING_1;
               New_Root.Op_ID    := Status.Op_ID;
               New_Root.Sift_Pos := Status.Size; -- Choose branch to follow here.

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
      Free (New_Leaf);
   end Delete_Min_PP;

   ----------------------------------------------------------------------------
   procedure Read_And_Fix (Queue      : in out Priority_Queue_Type;
                           Index      : in     Heap_Index;
                           Clean_Copy : in     Heap_Entry_Access;
                           Old_Entry  :    out Heap_Entry_Access) is
   begin
      -- Read entry.
      Old_Entry := Queue.Heap (Index);

      -- Helping not implemented!
      if Old_Entry.Status /= Stable then
         Ada.Text_IO.Put_Line ("Reading unstable entry: " &
                               Entry_Status'Image (Old_Entry.Status));
      end if;

      -- Allocate copy.
      Clean_Copy.all := Old_Entry.all;
   end Read_And_Fix;

   ----------------------------------------------------------------------------
   -- Sort two heap entries.
   procedure Sort_Parent_Child (Queue : in out Priority_Queue_Type;
                                Child : in     Heap_Index;
                                Op_ID : in     Operation_ID;
                                Done  :    out Boolean) is
      New_Entry    : Heap_Entry_Access;
      Parent_Entry : Heap_Entry_Access;
      Child_Entry  : Heap_Entry_Access;
      Parent       : Heap_Index := Child/2;
   begin
      -- Step 1: Set Child to SWAP iff Parent > Child.
      New_Entry := new Heap_Entry;
      loop
         -- Read Parent and Child entries.
         Parent_Entry := Queue.Heap (Parent);
         --Read_And_Fix (Queue,
         --              Index      => Child,
         --              Old_Entry  => Child_Entry,
         --              Clean_Copy => New_Entry);
         Child_Entry  := Queue.Heap (Child);

         if (Parent_Entry.Status = SIFTING_1 and Parent_Entry.Op_ID = Op_ID and
             Child_Entry /= null) and then
           Parent_Entry.Key > New_Entry.Key then
            -- Swap.
            -- Prepare new Child entry.
            --New_Entry.all := Child_Entry.all;

            New_Entry.Status  := SWAP_WITH_PARENT;
            New_Entry.Old_Key := Child_Entry.Key;
            New_Entry.Key     := Parent_Entry.Key;
            New_Entry.Op_ID   := Op_ID;
         else
            -- Nothing to be done.
            -- And according to the algorithm we can end the sift phase??
            Free (New_Entry);
            Done := True;
            return;
         end if;

         exit when CAS (Target    => Queue.Heap (Child)'Access,
                        Old_Value => Child_Entry,
                        New_Value => New_Entry);
      end loop;

      -- Step 2: Change Parent.
      New_Entry := new Heap_Entry;
      loop
         -- Read Parent and Child entries.
         -- Should be done through Fix?
         Parent_Entry := Queue.Heap (Parent);
         Child_Entry  := Queue.Heap (Child);

         if (Parent_Entry.Op_ID = Op_ID and
             Child_Entry /= null) and then
            Child_Entry.Status = SWAP_WITH_PARENT then

            -- Prepare new Parent entry.
            New_Entry.all := Parent_Entry.all;

            New_Entry.Status  := STABLE;
            New_Entry.Key     := Child_Entry.Old_Key;
            New_Entry.Op_ID   := 0;
         else
            -- Nothing to be done.
            -- And according to the algorithm we can end the sift phase.
            Free (New_Entry);
            Done := Child_Entry = null;
            return;
         end if;

         exit when CAS (Target    => Queue.Heap (Parent)'Access,
                        Old_Value => Parent_Entry,
                        New_Value => New_Entry);
      end loop;

      -- Step 3: Finish Child.
      New_Entry := new Heap_Entry;
      loop
         -- Read Child entry.
         -- Should be done through Fix.
         Child_Entry  := Queue.Heap (Child);

         if (Parent_Entry.Op_ID = Op_ID and
             Child_Entry /= null) and then
           Child_Entry.Status = SWAP_WITH_PARENT then

            -- Prepare new Child entry.
            New_Entry.all := Child_Entry.all;
            New_Entry.Status := SIFTING_1;
         else
            -- Nothing to be done.
            -- And according to the algorithm we can end the sift phase.
            Free (New_Entry);
            Done :=  Child_Entry = null;
            return;
         end if;

         exit when CAS (Target    => Queue.Heap (Child)'Access,
                        Old_Value => Child_Entry,
                        New_Value => New_Entry);
      end loop;
      Done := False;
   end Sort_Parent_Child;


end Non_Blocking_Priority_Queue;
