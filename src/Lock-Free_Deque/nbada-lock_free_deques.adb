-------------------------------------------------------------------------------
--  Lock-Free Deques - An Ada implementation of the lock-free deque algorithm
--                     by H. Sundell and P. Tsigas.
--
--  Copyright (C) 2006 - 2011  Anders Gidenstam
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
--  Filename        : nbada-lock_free_deques.adb
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:59:45 2006
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

with NBAda.Lock_Free_Growing_Storage_Pools;
with NBAda.Primitives;

with Ada.Exceptions;
with Ada.Text_IO;

package body NBAda.Lock_Free_Deques is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
       (Block_Size => Deque_Node'Max_Size_In_Storage_Elements);

   type New_Deque_Node_Access is access Deque_Node;
   for New_Deque_Node_Access'Storage_Pool use Node_Pool;

   function New_Node is new MR_Ops.Create (New_Deque_Node_Access);

   ----------------------------------------------------------------------------
   package Reference_Marks is
      new MR_Ops.Basic_Reference_Operations.Reference_Mark_Operations
          (MR_Ops.Private_Reference);
   use Reference_Marks;

   ----------------------------------------------------------------------------
   --  Internal operations.
   ----------------------------------------------------------------------------

   Debug : constant Boolean := False;

   procedure Push_Common (Deque      : in Deque_Type;
                          Node, Next : in Private_Reference);
   --  Note: Push_Common releases both Node and Next.

   procedure Help_Insert (Deque : in     Deque_Type;
                          Node  : in     Private_Reference;
                          After : in out Private_Reference);
   --  Updates Node.Previous to point to After (or a predecessor
   --  to After in case After is deleted).
   --  Note: Help_Insert updates After to point to the node
   --        in front of Node (if needed). The old value of After
   --        is released.
   --  Note: Uses at most 2 + Help_Delete additional dereferences.
   --        (With recursive helping disabled).

   procedure Help_Delete (Deque         : in Deque_Type;
                          Node          : in Private_Reference;
                          Recurse_Limit : in Natural := 2);
   --  Fully mark Node as logically deleted and unlinks Node from the active
   --  forward list structure.
   --  Note: The reference to Node is not released by Help_Delete.
   --        Node.Next should be marked before the call to Help_Delete.
   --  Note: Uses at most 3 additional dereferences.

   function Same_Node (Left  : Private_Reference;
                       Right : Private_Reference) return Boolean;
   --  Returns true if Left and Right refer to the same node.
   pragma Inline (Same_Node);
   function Same_Node (Left  : Private_Reference;
                       Right : Deque_Node_Reference) return Boolean;
   --  Returns true if Left and Right refer to the same node.
   pragma Inline (Same_Node);
   function Different_Nodes (Left, Right : Private_Reference) return Boolean;
   --  Returns true if Left and Right refer to different nodes.
   pragma Inline (Different_Nodes);

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------
   procedure Init    (Deque : in out Deque_Type) is
      use MR_Ops;
      Head : constant Private_Reference := New_Node (Deque.MM);
      Tail : constant Private_Reference := New_Node (Deque.MM);
   begin
      Store ("+"(Head).Next'Access, Tail);
      Store ("+"(Tail).Previous'Access, Head);
      Store (Deque.Head'Access, Head);
      Store (Deque.Tail'Access, Tail);
      Release (Head);
      Release (Tail);
   end Init;

   ----------------------------------------------------------------------
   function  Pop_Right  (Deque : access Deque_Type) return Element_Type is
      use MR_Ops;
      Next : constant Private_Reference :=
        Dereference (Deque.MM,
                     Deque.Tail'Access);
      Node : Private_Reference;
   begin
      Node := Dereference (Deque.MM,
                           "+"(Next).Previous'Access);

      loop
         if "+"(Node).Next /= Next then
            --  Node is no longer at the right head.
            --  Correct Tail.Previous.
            Help_Insert (Deque.all,
                         Node  => Next,
                         After => Node);

         elsif Same_Node (Node, Deque.Head) then
            --  The deque is empty.

            Release (Node);
            Release (Next);
            raise Deque_Empty;

         elsif Compare_And_Swap (Link      => "+"(Node).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Mark (Next))
         then
            --  We marked Node as deleted.
            --  Unlink Node from the forward chain.
            Help_Delete (Deque.all, Node);
            declare
               Prev : Private_Reference :=
                 Dereference (Deque.MM,
                              "+"(Node).Previous'Access);
            begin
               --  Unlink Node from the backward chain.
               Help_Insert (Deque.all,
                            Node  => Next,
                            After => Prev);
               Release (Prev);
            end;

            declare
               Element : constant Element_Type := "+"(Node).Value;
            begin
--               Clean_Up (Deque.MM,
--                         "+" (Node));

               Release (Next);
               Delete (Node);
               return Element;
            end;
         end if;

         --  Back-off.
         delay 0.0;

      end loop;

   end Pop_Right;

   ----------------------------------------------------------------------
   procedure Push_Right (Deque : in out Deque_Type;
                         Element : in     Element_Type) is
      use MR_Ops;
      Next, Prev : Private_Reference;
      Node       : constant Private_Reference := New_Node (Deque.MM);
   begin
      "+"(Node).Value := Element;
      Next := Dereference (Deque.MM,
                           Deque.Tail'Access);
      Prev := Dereference (Deque.MM,
                           "+"(Next).Previous'Access);

      loop
         if "+"(Prev).Next = Unmark (Next) then

            Store ("+"(Node).Previous'Access, Unmark (Prev));
            Store ("+"(Node).Next'Access, Unmark (Next));

            if Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Unmark (Node))
            then
               exit;
            else
               --  Back-off.
               delay 0.0;
            end if;

         else

            Help_Insert (Deque,
                         Node  => Next,
                         After => Prev);


         end if;

      end loop;

      Release (Prev);

      Push_Common (Deque,
                   Node, Next);

   end Push_Right;

   ----------------------------------------------------------------------
   function  Pop_Left  (Deque : access Deque_Type) return Element_Type is
      use MR_Ops;
      Prev  : Private_Reference;
   begin
      Prev := Dereference (Deque.MM,
                           Deque.Head'Access);
      loop
         declare
            Node  : constant Private_Reference :=
              Dereference (Deque.MM,
                           "+"(Prev).Next'Access);
            Element : Element_Type;
         begin
            if Same_Node (Node, Deque.Tail) then
               Release (Node);
               Release (Prev);
               raise Deque_Empty;
            end if;

            declare
               package BRO renames MR_Ops.Basic_Reference_Operations;
               Node_Next : constant BRO.Unsafe_Reference_Value :=
                 BRO.Unsafe_Read ("+"(Node).Next'Access);
            begin
               if Is_Marked (Node_Next) then
                  Help_Delete (Deque.all,
                               Node);
                  Release (Node);

               elsif Compare_And_Swap (Link      => "+"(Node).Next'Access,
                                       Old_Value => Node_Next,
                                       New_Value => Mark (Node_Next))
               then
                  --  We marked Node as logically deleted.
                  --  Now unlink it.
                  Help_Delete (Deque.all,
                               Node);

                  declare
                     Next : constant Private_Reference :=
                       Dereference (Deque.MM,
                                    "+"(Node).Next'Access);
                  begin
                     Help_Insert (Deque.all,
                                  Node  => Next,
                                  After => Prev);

                     Release (Prev);
                     Release (Next);
                  end;

                  Element := "+"(Node).Value;
--                  Clean_Up (Deque.MM,
--                            "+" (Node));
                  Delete (Node);
                  return Element;
               else
                  Release (Node);

                  --  Back-off.
                  delay 0.0;

               end if;
            end;
         end;
      end loop;
   end Pop_Left;

   ----------------------------------------------------------------------
   procedure Push_Left (Deque   : in out Deque_Type;
                        Element : in     Element_Type) is
      use MR_Ops;
      Node : constant Private_Reference := New_Node (Deque.MM);
      Prev : constant Private_Reference := Dereference (Deque.MM,
                                                        Deque.Head'Access);
      Next : Private_Reference;
   begin
      "+"(Node).Value := Element;
      Next := Dereference (Deque.MM,
                           "+"(Prev).Next'Access);

      loop
         if "+"(Prev).Next = Unmark (Next) then

            Store ("+"(Node).Previous'Access, Unmark (Prev));
            Store ("+"(Node).Next'Access,     Unmark (Next));

            if Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Unmark (Node))
            then
               exit;
            else
               --  Back-off.
               delay 0.0;
            end if;

         else
            --  Prev.head has changed.

            Release (Next);
            Next := Dereference (Deque.MM,
                                 "+"(Prev).Next'Access);

         end if;

      end loop;

      Release (Prev);

      Push_Common (Deque,
                   Node, Next);
   end Push_Left;

   ----------------------------------------------------------------------------
   procedure Verify (Deque : in out Deque_Type;
                     Print : in     Boolean := False) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
      use type BRO.Unsafe_Reference_Value;

      function Find  (Value : BRO.Unsafe_Reference_Value) return Natural;
      function Image (Node  : Private_Reference) return String;

      -----------------------------------------------------------------
      function Find (Value : BRO.Unsafe_Reference_Value) return Natural is
         Node : Private_Reference := Dereference (Deque.MM,
                                                  Deque.Head'Access);
         N    : Natural := 0;
      begin
         loop
            if Private_Reference (Node) = Value then
               Release (Node);
               return N;
            end if;

            declare
               Next : constant Private_Reference :=
                 Dereference (Deque.MM,
                              "+" (Node).Next'Access);
            begin
               Release (Node);
               Node := Next;
            end;

            exit when Node = Null_Reference;

            N := N + 1;

         end loop;
         return Natural'Last;
      end Find;

      -----------------------------------------------------------------
      function Image (Node  : Private_Reference) return String is
         function To_Unsigned is
            new Ada.Unchecked_Conversion (Deque_Node_Reference,
                                          Primitives.Standard_Unsigned);
         function To_Unsigned is
            new Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                          Primitives.Standard_Unsigned);
         use Primitives;
      begin
         return
           Standard_Unsigned'Image (To_Unsigned ("+" (Node))) &
           "@(" &
           "Previous = " &
           Standard_Unsigned'Image (To_Unsigned ("+" (Node).Previous)) & ", " &
           "Next = " &
           Standard_Unsigned'Image (To_Unsigned ("+" (Node).Next)) &
           ")";
      end Image;

      -----------------------------------------------------------------
      Prev : Private_Reference := Null_Reference;
      Node : Private_Reference := Dereference (Deque.MM,
                                               Deque.Head'Access);
      N    : Natural := 0;
   begin
      loop
         if Print then
            Ada.Text_IO.Put_Line (Image (Node));
         end if;
         if "+" (Node).Previous /= Prev then
            if  "+" (Node).Previous = Null_Reference then
               Ada.Text_IO.Put_Line
                 ("nbada-lock_free_deque.adb: Verify: " &
                  "Bad null Node.Previous pointer found at " &
                  N'Img);
            else
               Ada.Text_IO.Put_Line ("nbada-lock_free_deque.adb: Verify: " &
                                     "Bad Node.Previous pointer found at " &
                                     N'Img);
               Ada.Text_IO.Put_Line
                 ("nbada-lock_free_deque.adb: Verify: " &
                  "It points to node " &
                  Find (BRO.Unsafe_Read ("+" (Node).Previous'Access))'Img);
            end if;
         end if;
         if Is_Marked ("+" (Node).Next'Access) then
            Ada.Text_IO.Put_Line ("nbada-lock_free_deque.adb: Verify: " &
                                  "Marked node found at " &
                                  N'Img);
         end if;

         if "+" (Prev) /= null then
            Release (Prev);
         end if;
         Prev := Node;
         Node := Dereference (Deque.MM,
                              "+" (Node).Next'Access);

         N := N + 1;

         exit when "+" (Node) = null;
      end loop;

      Ada.Text_IO.Put_Line ("nbada-lock_free_deque.adb: Verify: " &
                            "Total number of nodes = " &
                            N'Img);

   end Verify;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------
   procedure Dispose (Node       : access Deque_Node;
                      Concurrent : in     Boolean) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
   begin
      if not Concurrent then
         Store (Node.Previous'Access, Null_Reference);
         Store (Node.Next'Access, Null_Reference);
      else
         declare
            Tmp : BRO.Unsafe_Reference_Value;
         begin
            loop
               Tmp := BRO.Unsafe_Read (Node.Previous'Access);
               exit when Compare_And_Swap (Link      => Node.Previous'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;

            loop
               Tmp := BRO.Unsafe_Read (Node.Next'Access);
               exit when Compare_And_Swap (Link      => Node.Next'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
            end loop;
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------
   procedure Clean_Up (MM   : in     MR.Memory_Manager_Base'Class;
                       Node : access Deque_Node) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
      Done : Boolean := False;
   begin
      while not Done loop
         Done := True;

         declare
            Node_Prev  : constant BRO.Unsafe_Reference_Value :=
              BRO.Unsafe_Read (Node.Previous'Access);
            Prev       : Private_Reference :=
              Dereference (Memory_Manager (MM),
                           Node.Previous'Access);
         begin
            loop
               if
                 "+"(Prev) /= null and then
                 Is_Marked ("+"(Prev).Next'Access)
               then
                  --  Prev is logically deleted.
                  declare
                     Prev2 : constant Private_Reference :=
                       Dereference (Memory_Manager (MM),
                                    "+"(Prev).Previous'Access);
                  begin
                     Release (Prev);
                     Prev := Prev2;
                  end;
               else
                  exit;
               end if;
            end loop;

            if "+"(Prev) /= null then
               --  There is a live Prev.
               if
                 not Compare_And_Swap (Link      => Node.Previous'Access,
                                       Old_Value => Node_Prev,
                                       New_Value => Mark (Prev))
               then
                  Done := False;
               end if;
            else
               --  Can this ever happen?
               null;
            end if;
            Release (Prev);
         end;

         declare
            Node_Next  : constant BRO.Unsafe_Reference_Value :=
              BRO.Unsafe_Read (Node.Next'Access);
            Next       : Private_Reference :=
              Dereference (Memory_Manager (MM),
                           Node.Next'Access);
         begin
            loop
               if
                 "+"(Next) /= null and then
                 Is_Marked ("+"(Next).Next'Access)
               then
                  --  Next is logically deleted.
                  declare
                     Next2 : constant Private_Reference :=
                       Dereference (Memory_Manager (MM),
                                    "+"(Next).Next'Access);
                  begin
                     Release (Next);
                     Next := Next2;
                  end;
               else
                  exit;
               end if;
            end loop;
            if "+"(Next) /= null then
               --  There is a live Next.
               if
                 not Compare_And_Swap (Link      => Node.Next'Access,
                                       Old_Value => Node_Next,
                                       New_Value => Mark (Next))
               then
                  Done := False;
               end if;
            else
               --  Can this ever happen?
               null;
            end if;
            Release (Next);
         end;
      end loop;
   end Clean_Up;

   ----------------------------------------------------------------------
   procedure Free (Node : access Deque_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (Deque_Node,
                                    New_Deque_Node_Access);

      function To_New_Deque_Node_Access is new
        Ada.Unchecked_Conversion (MR_Ops.Node_Access,
                                  New_Deque_Node_Access);

      X : New_Deque_Node_Access :=
        To_New_Deque_Node_Access (MR_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------
   function All_References (Node : access Deque_Node)
                           return MR.Reference_Set is
      type Link_Access is access all Deque_Node_Reference;
      function To_Shared_Reference_Access is new
        Ada.Unchecked_Conversion (Link_Access,
                                  MR.Shared_Reference_Base_Access);

      Result : constant MR.Reference_Set (1 .. 2) :=
        (1 => To_Shared_Reference_Access (Node.Next'Access),
         2 => To_Shared_Reference_Access (Node.Previous'Access));
   begin
      return Result;
   end All_References;

   ----------------------------------------------------------------------
   procedure Push_Common (Deque      : in Deque_Type;
                          Node, Next : in Private_Reference) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
      use type BRO.Unsafe_Reference_Value;
   begin
      loop
         declare
            Next_Prev : constant BRO.Unsafe_Reference_Value :=
              BRO.Unsafe_Read ("+"(Next).Previous'Access);
         begin
            if Is_Marked (Next_Prev) or "+"(Node).Next /= Unmark (Next) then
               --  Next or Node has been marked deleted.
               exit;
            end if;

            if Compare_And_Swap (Link      => "+"(Next).Previous'Access,
                                 Old_Value => Next_Prev,
                                 New_Value => Unmark (Node))
            then
               if Is_Marked ("+"(Node).Previous'Access) then
                  --  Node has become marked as deleted. Help remove it.
                  declare
                     Tmp : Private_Reference := Copy (Node);
                  begin
                     Help_Insert (Deque,
                                  Node  => Next,
                                  After => Tmp);
                     Release (Tmp);
                  end;
               end if;

               exit;

            end if;
         end;

         --  Back-off.
         delay 0.0;

      end loop;
      Release (Next);
      Release (Node);
   end Push_Common;

   ----------------------------------------------------------------------
   procedure Help_Insert (Deque : in     Deque_Type;
                          Node  : in     Private_Reference;
                          After : in out Private_Reference) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
      use type BRO.Unsafe_Reference_Value;
      Prev      : Private_Reference renames After;
      Last_Link : Boolean := True;
   begin
      loop
         --  Debug check.
         if Debug then
            if "+" (Prev) = null then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-lock_free_deques.adb:464  Help_Insert: " &
                  "Prev is null! This should not happen!");
            end if;
         end if;

         declare
            Prev_Next : constant Private_Reference :=
              Dereference (Deque.MM,
                           "+" (Prev).Next'Access);
         begin
            if "+" (Prev_Next) = null then
               --  Prev is the right dummy node.
               --  This case is different from [Sundell, Tsigas, 2004].
               Release (Prev_Next);
               return;

            elsif Is_Marked (Prev_Next) then
               --  Prev is marked deleted. Move Prev to Prev.Previous.
               Release (Prev_Next);

               if not Last_Link then
                  Help_Delete (Deque,
                               Prev);
                  Last_Link := True;
               end if;

               declare
                  Prev_Prev : constant Private_Reference :=
                    Dereference (Deque.MM,
                                 "+" (Prev).Previous'Access);
               begin
                  Release (Prev);
                  Prev := Prev_Prev;
               end;

            else
               --  Prev_Next is a valid link to an active node.

               declare
                  Node_Prev : constant BRO.Unsafe_Reference_Value :=
                    BRO.Unsafe_Read ("+" (Node).Previous'Access);
               begin

                  if Is_Marked (Node_Prev) then
                     --  Node has been marked deleted. Give up.
                     Release (Prev_Next);
                     return;
                  end if;

                  if Different_Nodes (Prev_Next, Node) then
                     --  Move Prev to Prev_Next.
                     Last_Link := False;
                     Release (Prev);
                     Prev := Prev_Next;

                  else
                     --  Node is Prev_Next.
                     Release (Prev_Next);

                     if
                       Compare_And_Swap
                       (Link      => "+" (Node).Previous'Access,
                        Old_Value => Node_Prev,
                        New_Value => Unmark (Prev))
                     then

                        if not Is_Marked ("+" (Prev).Previous'Access) then
                           --  Done.
                           return;
                        end if;

                     end if;

                     --  Back-off.
                     delay 0.0;

                  end if;

               end;

            end if;
         end;
      end loop;
   end Help_Insert;

   ----------------------------------------------------------------------
   procedure Help_Delete (Deque         : in Deque_Type;
                          Node          : in Private_Reference;
                          Recurse_Limit : in Natural := 2) is
      package BRO renames MR_Ops.Basic_Reference_Operations;
      use MR_Ops;
      use type BRO.Unsafe_Reference_Value;
   begin
      --  Set logically deleted mark on Node.Previous.
      --  Node.Next should already be marked.
      loop
         declare
            Old_Link : constant BRO.Unsafe_Reference_Value :=
              BRO.Unsafe_Read ("+"(Node).Previous'Access);
         begin
            if Is_Marked (Old_Link) or else
              Compare_And_Swap (Link      => "+"(Node).Previous'Access,
                                Old_Value => Old_Link,
                                New_Value => Mark (Old_Link))
            then
               exit;
            end if;
         end;
      end loop;

      --  Unlink Node from the active next chain.
      declare
         Prev, Next : Private_Reference;
         Last_Link  : Boolean := True;
      begin
         Prev := Dereference (Deque.MM,
                              "+"(Node).Previous'Access);
         Next := Dereference (Deque.MM,
                              "+"(Node).Next'Access);

         --  Debug checks.
         if Debug then
            if "+" (Next) = null then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-lock_free_deques.adb: Help_Delete: " &
                  "Node.next is null! This should not happen!");
            end if;
            if "+" (Prev) = null then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "nbada-lock_free_deques.adb: Help_Delete: " &
                  "Node.previous is null! This should not happen!");
            end if;
         end if;

         Find : loop
            --  Exit if we didn't find Node, i.e. it is already unlinked.
            exit Find when Same_Node (Prev, Next);

            if Is_Marked ("+"(Next).Next'Access) then
               --  Next is deleted. Move Next to the next next node.
               declare
                  Next_Next : constant Private_Reference :=
                    Dereference (Deque.MM,
                                 "+"(Next).Next'Access);
               begin
                  Release (Next);
                  Next := Next_Next;
               end;
            else
               declare
                  Prev_Next : constant Private_Reference :=
                    Dereference (Deque.MM,
                                 "+"(Prev).Next'Access);
               begin
                  if "+" (Prev_Next) = null then
                     --  Prev is the right dummy node.
                     --  This case is different from [Sundell, Tsigas, 2004].
                     Release (Prev_Next);
                     exit Find;

                  elsif Is_Marked (Prev_Next) then
                     --  Prev has been marked deleted.
                     Release (Prev_Next);

                     if not Last_Link then
                        if Recurse_Limit > 0 then
                           Help_Delete (Deque,
                                        Prev,
                                        Recurse_Limit => Recurse_Limit - 1);
                        end if;
                        Last_Link := True;
                     end if;

                     --  Move Prev to Prev.Previous.
                     declare
                        Prev_Prev : constant Private_Reference :=
                          Dereference (Deque.MM,
                                       "+" (Prev).Previous'Access);
                     begin
                        Release (Prev);

                        if "+" (Prev_Prev) = null then
                           --  The left dummy node has been marked deleted!
                           Ada.Text_IO.Put_Line
                             ("nbada-lock_free_deques.adb: " &
                              "Help_Delete: " &
                              "Setting Prev to null! " &
                              "Bad!!");
                           raise Constraint_Error;
                        end if;

                        Prev := Prev_Prev;
                     end;

                  elsif Different_Nodes (Prev_Next, Node) then
                     Last_Link := False;
                     Release (Prev);
                     Prev := Prev_Next;

                  else
                     --  Node is Prev_Next.
                     Release (Prev_Next);

                     if
                       Compare_And_Swap (Link      => "+" (Prev).Next'Access,
                                         Old_Value => Unmark (Node),
                                         New_Value => Unmark (Next))
                     then
                        exit Find;
                     end if;

                     --  Back-off
                     delay 0.0;

                  end if;

               end;

            end if;

         end loop Find;

         Release (Prev);
         Release (Next);
      end;

   end Help_Delete;

   ----------------------------------------------------------------------------
   function Same_Node (Left  : Private_Reference;
                       Right : Private_Reference) return Boolean is
      use MR_Ops;
   begin
      return "+" (Left) = "+" (Right);
   end Same_Node;

   ----------------------------------------------------------------------------
   function Same_Node (Left  : Private_Reference;
                       Right : Deque_Node_Reference) return Boolean is
      use MR_Ops;
   begin
      return Unmark (Left) = Right;
   end Same_Node;

   ----------------------------------------------------------------------------
   function Different_Nodes (Left, Right : Private_Reference) return Boolean is
      use MR_Ops;
   begin
      return "+" (Left) /= "+" (Right);
   end Different_Nodes;

end NBAda.Lock_Free_Deques;
