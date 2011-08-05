-------------------------------------------------------------------------------
--  Lock-free bag - An implementation of  the lock-free bag algorithm
--  by H. Sundell, A. Gidenstam, M. Papatriantafilou and P. Tsigas.
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
--                              -*- Mode: Ada -*-
--  Filename        : nbada-lock_free_queues.adb
--  Description     : A lock-free bag algorithm based on
--                    H. Sundell, A. Gidenstam, M. Papatriantafilou and
--                    P. Tsigas,
--                    "A Lock-Free Algorithm for Concurrent Bags",
--                    Proceedings of the 23rd Annual Symposium on Parallelism
--                    in Algorithms and Architectures (SPAA 2011),
--                    pages 335 - 344, ACM, 2011. 
--  Author          : Anders Gidenstam
--  Created On      : Thu Aug 02 17:10:00 2011
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;

with Ada.Text_IO;

package body NBAda.Lock_Free_Bags is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Bag_Node'Max_Size_In_Storage_Elements);

   type New_Bag_Node_Access is access Bag_Node;
   for New_Bag_Node_Access'Storage_Pool use Node_Pool;

   function New_Node is new MR_Ops.Create
     (User_Node_Access => New_Bag_Node_Access);

   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap
      (Element => Element_Type);

   function Compare_And_Swap is
      new Primitives.Standard_Boolean_Compare_And_Swap
      (Element => Bitfield);

   procedure Verify_HP (Local   : access Thread_Local;
                        Message : in     String);
   function Try_Steal_Block (From      : access Bag_Type;
                             Local     : access Thread_Local;
                             Round     : in     Natural;
                             Result    : access Element_Type;
                             Found_Add : access Boolean)
                            return Boolean;
   function Next_Steal_Block (From      : access Bag_Type;
                              Local     : access Thread_Local;
                              Block     : MR_Ops.Private_Reference)
                            return MR_Ops.Private_Reference;

   procedure Notify_All (Block : in MR_Ops.Private_Reference);
   procedure Notify_Start (Block : in MR_Ops.Private_Reference);
   function  Notify_Check (Block : in MR_Ops.Private_Reference)
                          return Boolean;

   procedure Set_Mark_A (Block : in MR_Ops.Private_Reference);

   ----------------------------------------------------------------------------
   --  Public operations
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function  Try_Remove_Any (From   : access Bag_Type;
                             Result : access Element_Type) return Boolean is
      use MR_Ops;
      Local : constant TLS.Element_Access := TLS.Get (From.Thread_Local);
      Block : Private_Reference := Local.Own_Block;
      Head  : Element_Index     := Local.Own_Index - 1;
   begin
      loop
         if
           Block = Null_Reference or else
           (Head < 0 and then "+" (Block).Next = Mark (Null_Reference, A))
         then
            --  Attempt to steal an element from another thread.
            declare
               Round     : Natural := 0;
               I         : Natural;
               Found_Add : aliased Boolean := False;
            begin
               loop
                  I := Natural (Process_Ids.Process_ID_Type'Last);
                  while I > 0 loop
                     if
                       Try_Steal_Block (From, Local, Round,
                                        Result, Found_Add'Access)
                     then
                        return True;
                     elsif Found_Add then
                        I     := Natural (Process_Ids.Process_ID_Type'Last);
                        Round := I;
                     elsif Local.Steal_Block = Null_Reference then
                        I := I - 1;
                     end if;
                  end loop;
                  Round := Round + 1;
                  if Round = Natural (Process_Ids.Process_ID_Type'Last) then
                     --  The bag is empty.
                     Verify_HP (Local, "Try_Remove_Any@Empty");
                     return False;
                  end if;
               end loop;
            end;

         elsif
           Head < 0
         then
            --  Remove the empty block from the own list and move to the next.
            declare
               Shared : constant TSS.Element_Access :=
                 TSS.Get (From.Thread_Shared);
            begin
               Set_Mark_A (Block);
               loop
                  declare
                     Next : constant Private_Reference :=
                       Dereference ("+" (Block).Next'Access);
                  begin
                     if Is_Marked (Next, B) then
                        --  Help setting mark1 on the block Next.
                        Set_Mark_A (Next);
                     end if;
                     if Is_Marked (Next, A) then
                        Notify_All (Next);
                        if
                          Compare_And_Swap (Shared.Head'Access,
                                            Old_Value => Block,
                                            New_Value => Unmark (Next))
                        then
                           Store ("+" (Block).Next'Access,
                                  Mark (Null_Reference, A));
                           Delete (Block);
                           Rescan (Next);
                           Block := Unmark (Next);
                        else
                           Block := Dereference (Shared.Head'Access);
                        end if;
                     else
                        exit;
                     end if;
                  end;
               end loop;
               Local.Own_Block := Block;
               Local.Own_Index := Block_Size;
               Head            := Block_Size - 1;
            end;

         else
            --  Search the current own block.
            Result.all := "+" (Block).Element (Head);
            if Result.all = Null_0 then
               Head := Head - 1;
            elsif
              Compare_And_Swap ("+" (Block).Element (Head)'Access,
                                Old_Value => Result.all,
                                New_Value => Null_0)
            then
               Local.Own_Index := Head;
               Verify_HP (Local, "Try_Remove_Any@Success");
               return True;
            end if;
         end if;
      end loop;
   end Try_Remove_Any;

   ----------------------------------------------------------------------------
   procedure Insert (On      : in out Bag_Type;
                     Element : in     Element_Type) is
      use MR_Ops;
      Local : constant TLS.Element_Access := TLS.Get (On.Thread_Local);
   begin
      --  Initialize this thread if needed.
      if Local.Own_Block = Null_Reference then
         declare
            Shared : constant TSS.Element_Access := TSS.Get (On.Thread_Shared);
         begin
            Local.Own_Block := New_Node;
            Store (Shared.Head'Access, Local.Own_Block);
         end;
      end if;

      declare
         Head  : Element_Index     := Local.Own_Index;
         Block : Private_Reference := Local.Own_Block;
      begin
         loop
            if Head = Block_Size then
               declare
                  Shared : constant TSS.Element_Access :=
                    TSS.Get (On.Thread_Shared);
                  Old_Block : Private_Reference        := Block;
               begin
                  Block := New_Node;
                  Store ("+" (Block).Next'Access, Old_Block);
                  Store (Shared.Head'Access, Block);
                  Local.Own_Block := Block;
                  Head            := 0;
                  Release (Old_Block);
               end;

            elsif "+" (Block).Element (Head) = Null_0 then
               Notify_All (Block);
               "+" (Block).Element (Head) := Element;
               Local.Own_Index            := Head + 1;

               Verify_HP (Local, "Insert");

               return;

            else
               Head := Head + 1;
            end if;
         end loop;
      end;
   end Insert;

   ----------------------------------------------------------------------------
   --  Internal operations
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Verify_HP (Local   : access Thread_Local;
                        Message : in     String) is
      use MR_Ops;

      procedure Validate (R : in Private_Reference;
                          M : in String);
      procedure Validate (R : in Private_Reference;
                          M : in String) is
      begin
         if "+" (R) /= null then
            null;
         end if;
      exception
         when others =>
            Ada.Text_IO.Put_Line (M);
            raise;
      end Validate;

   begin
      Validate (Local.Own_Block,
                "nbada-lock_free_bags.adb " & Message &
                "the reference Local.Own_Block is invalid.");
      Validate (Local.Steal_Block,
                "nbada-lock_free_bags.adb " & Message &
                "the reference Local.Steal_Block is invalid.");
      Validate (Local.Own_Block,
                "nbada-lock_free_bags.adb " & Message &
                "the reference Local.Prev_Steal_Block is invalid.");
      if MR.Number_Of_HPs_Held > 3 then
         Ada.Text_IO.Put_Line
           ("nbada-lock_free_bags.adb " & Message &
            "Too many HPs held, " &
            Natural'Image (MR.Number_Of_HPs_Held) & ".");
      end if;
   end Verify_HP;

   ----------------------------------------------------------------------------
   function Try_Steal_Block (From      : access Bag_Type;
                             Local     : access Thread_Local;
                             Round     : in     Natural;
                             Result    : access Element_Type;
                             Found_Add : access Boolean)
                            return Boolean is
      use MR_Ops;
      Head  : Element_Index     := Local.Steal_Index;
      Block : Private_Reference := Local.Steal_Block;
   begin
      Found_Add.all := False;
      if Block = Null_Reference then
         declare
            use type TSS.Element_Access;
            Shared : constant TSS.Element_Access :=
              TSS.Get (From.Thread_Shared, Local.Steal_Thread);
         begin
            if Shared /= null then
               Block             := Dereference (Shared.Head'Access);
               Local.Steal_Block := Block;
               Local.Steal_Index := 0;
               Head              := 0;
            end if;
         end;
      end if;
      if Head = Block_Size then
         Block := Next_Steal_Block (From, Local, Block);
         Local.Steal_Block := Block;
         Head  := 0;
      end if;
      if Block = Null_Reference then
         declare
            T : constant Integer := Integer (Local.Steal_Thread);
            N : constant Integer :=
              Integer (Process_Ids.Max_Number_Of_Processes) - 1;
         begin
            Local.Steal_Thread :=
              Process_Ids.Process_ID_Type (T mod N + 1);
         end;
         Local.Steal_Index := 0;
         Release (Local.Steal_Block);
         Release (Local.Prev_Steal_Block);
         Local.Steal_Block      := Null_Reference;
         Local.Prev_Steal_Block := Null_Reference;
         return False;

      else

         if Round = 1 then
            Notify_Start (Block);
         elsif Round > 1 and then Notify_Check (Block) then
            Found_Add.all := True;
         end if;

         loop
            if Head = Block_Size then
               Local.Steal_Index := Head;
               return False;

            else
               --  Search the current steal block.
               Result.all := "+" (Block).Element (Head);
               if Result.all = Null_0 then
                  Head := Head + 1;
               elsif
                 Compare_And_Swap ("+" (Block).Element (Head)'Access,
                                   Old_Value => Result.all,
                                   New_Value => Null_0)
               then
                  Local.Steal_Index := Head;
                  return True;
               end if;
            end if;
         end loop;
      end if;
   end Try_Steal_Block;

   ----------------------------------------------------------------------------
   function Next_Steal_Block (From      : access Bag_Type;
                              Local     : access Thread_Local;
                              Block     : in     MR_Ops.Private_Reference)
                             return MR_Ops.Private_Reference is
      use MR_Ops;
      use type TSS.Element_Access;

      procedure Reset (Block : in out MR_Ops.Private_Reference);

      Shared : constant TSS.Element_Access :=
        TSS.Get (From.Thread_Shared, Local.Steal_Thread);

      procedure Reset (Block : in out MR_Ops.Private_Reference) is
      begin
         Release (Local.Prev_Steal_Block);
         Local.Prev_Steal_Block := Null_Reference;
         Release (Block);
         Block := Dereference (Shared.Head'Access);
      end Reset;

      Old    : MR_Ops.Private_Reference    := Block;
   begin
      loop
         if Old = Null_Reference then
            if Shared /= null then
               return Dereference (Shared.Head'Access);
            else
               return Null_Reference;
            end if;

         else
            declare
               Next         : constant Private_Reference :=
                 Dereference ("+" (Old).Next'Access);
               Move_Forward : Boolean := True;
            begin
               if Is_Marked (Next, B) then
                  Set_Mark_A (Next);
               end if;
               if
                 Local.Prev_Steal_Block = Null_Reference or
                 Unmark (Next) = Null_Reference
               then
                  if Is_Marked (Next, A) then
                     if Unmark (Next) /= Null_Reference then
                        Notify_All (Next);
                     end if;
                     if
                       Compare_And_Swap (Shared.Head'Access,
                                         Old_Value => Old,
                                         New_Value => Unmark (Next))
                     then
                        Store ("+" (Old).Next'Access,
                               Mark (Null_Reference, A));
                        Delete (Old);
                        Rescan (Next);
                     else
                        Release (Next);
                        Reset (Old);
                        Move_Forward := False;
                     end if;
                  else
                     Release (Local.Prev_Steal_Block);
                     Local.Prev_Steal_Block := Old;
                  end if;

               else
                  if Is_Marked (Next, A) then
                     --  Old has mark1 and should be dechained.
                     declare
                        Prev_Next : Private_Reference := Old;
                     begin
                        if
                          Is_Marked ("+" (Local.Prev_Steal_Block).Next, B)
                        then
                           Mark (Prev_Next, B);
                        end if;
                        if
                          Compare_And_Swap
                          ("+" (Local.Prev_Steal_Block).Next'Access,
                           Old_Value => Prev_Next,
                           New_Value => Unmark (Next))
                        then
                           Store ("+" (Old).Next'Access,
                                  Mark (Null_Reference, A));
                           Delete (Old);
                           Rescan (Next);
                        else
                           Release (Next);
                           Reset (Old);
                           Move_Forward := False;
                        end if;
                     end;

                  elsif Old = Local.Steal_Block then
                     if
                       Compare_And_Swap
                       ("+" (Local.Prev_Steal_Block).Next'Access,
                        Old_Value => Old,
                        New_Value => Mark (Old, B))
                     then
                        Set_Mark_A (Old);
                     else
                        Reset (Old);
                     end if;
                     Release (Next);
                     Move_Forward := False;
                  else
                     Release (Local.Prev_Steal_Block);
                     Local.Prev_Steal_Block := Old;
                  end if;
               end if;

               if Move_Forward then
                  if Old = Local.Steal_Block then
                     return Unmark (Next);
                  elsif Unmark (Next) = Local.Steal_Block then
                     --  Release (Old);
                     return Unmark (Next);
                  end if;
                  --  Apparently Old is always already released here.
                  Old := Unmark (Next);
               end if;
            end;
         end if;
      end loop;
   end Next_Steal_Block;

   ----------------------------------------------------------------------------
   procedure Notify_All (Block : in MR_Ops.Private_Reference) is
      use MR_Ops;
   begin
      "+" (Block).Notify := 0;
   end Notify_All;

   ----------------------------------------------------------------------------
   procedure Notify_Start (Block : in MR_Ops.Private_Reference) is
      use MR_Ops;
      ID : constant Process_Ids.Process_ID_Type := Process_Ids.Process_ID;
   begin
      loop
         declare
            Old : Bitfield := "+" (Block).Notify;
         begin
            exit when
              Compare_And_Swap ("+" (Block).Notify'Access,
                                Old_Value => Old,
                                New_Value => Old or 2 ** Integer (ID));
         end;
      end loop;
   end Notify_Start;

   function Notify_Check (Block : in MR_Ops.Private_Reference)
                         return Boolean is
      use MR_Ops;
      ID : constant Process_Ids.Process_ID_Type := Process_Ids.Process_ID;
   begin
      return ("+" (Block).Notify and 2 ** Integer (ID)) = 0;
   end Notify_Check;

   ----------------------------------------------------------------------------
   procedure Set_Mark_A (Block : in MR_Ops.Private_Reference) is
      use MR_Ops;
   begin
      loop
         declare
            Next : constant Private_Reference :=
              Dereference ("+" (Block).Next'Access);
         begin
            if
              Unmark (Next) = Null_Reference or else
              Is_Marked (Next, A) or else
              Compare_And_Swap ("+" (Block).Next'Access,
                                Old_Value => Next,
                                New_Value => Mark (Next, A))
            then
               Release (Next);
               return;
            else
               Release (Next);
            end if;
         end;
      end loop;
   end Set_Mark_A;

   ----------------------------------------------------------------------------
   procedure Free (Node : access Bag_Node) is
      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Bag_Node,
                                    New_Bag_Node_Access);
      function To_New_Bag_Node_Access is new
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_Bag_Node_Access);
      X : New_Bag_Node_Access :=
        To_New_Bag_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Queue_Node type from the New_Queue_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

end NBAda.Lock_Free_Bags;
