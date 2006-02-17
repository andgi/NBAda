-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_deques.adb
--  Description     : An Ada implementation of the lock-free deque algorithm
--                    by H. Sundell and P. Tsigas.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 15 18:59:45 2006
--  $Id: nbada-lock_free_deques.adb,v 1.1 2006/02/17 15:43:09 anders Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with Ada.Exceptions;

package body Lock_Free_Deques is

   ----------------------------------------------------------------------------
   --  Storage pool for the nodes.
   ----------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => Deque_Node'Max_Size_In_Storage_Elements);

   type New_Deque_Node_Access is access Deque_Node;
   for New_Deque_Node_Access'Storage_Pool use Node_Pool;

   function Create_Deque_Node is new LFRC_Ops.Create (New_Deque_Node_Access);

   ----------------------------------------------------------------------------
   -- Internal operations.
   ----------------------------------------------------------------------------

   function Read_All (Link : access Deque_Node_Reference)
                     return Deque_Node_Access
     renames LFRC_Ops.Dereference;
   function Read (Link : access Deque_Node_Reference)
                 return Deque_Node_Access;
   --  Behaves like LFRC_Ops.Dereference except that it returns
   --  Null_Reference if the link is marked as logically deleted.
   --  Note: It looks very unlikely that this is useful here!
   pragma Inline_Always (Read);

   procedure Push_Common (Node, Next : Deque_Node_Access);

   function Help_Insert (Prev, Node : in Deque_Node_Access)
                        return Deque_Node_Access;

   procedure Delete_Next (Node : in     Deque_Node_Access);
   --  Marks Node as logically deleted and unliks Node from the active
   --  structure.
   --  Note: The reference to Node is not released by Delete_Next.

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------
   procedure Init    (Deque : in out Deque_Type) is
      use LFRC_Ops;
      Head : constant Deque_Node_Access := Create_Deque_Node;
      Tail : constant Deque_Node_Access := Create_Deque_Node;
   begin
      Store ("+"(Head).Next'Access, Tail);
      Store ("+"(Tail).Previous'Access, Head);
      Store (Deque.Head'Access, Head);
      Store (Deque.Tail'Access, Tail);
      Release (Head);
      Release (Tail);
   end Init;

   ----------------------------------------------------------------------
   function  Pop_Right  (Deque : access Deque_Type) return Value_Type is
      use LFRC_Ops;
      Next, Node : Deque_Node_Access;
   begin
      Next := Dereference (Deque.Tail'Access);
      Node := Dereference ("+"(Next).Previous'Access);

      loop
         if "+"(Node).Next /= Unmark (Next) then
            Node := Help_Insert (Node, Next);

         elsif Node = Deque.Head then
            Release (Node);
            Release (Next);
            raise Deque_Empty;

         elsif Compare_And_Swap (Link      => "+"(Node).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Mark (Next))
         then
            Delete_Next (Node);
            declare
               Prev : Deque_Node_Access;
            begin
               Prev := Dereference ("+"(Node).Previous'Access);
               Prev := Help_Insert (Prev, Next);
               Release (Prev);
               Release (Next);
            end;

            declare
               Value : constant Value_Type := "+"(Node).Value;
            begin
               Delete (Node);
               return Value;
            end;
         end if;
         --  Back-off.
      end loop;

   end Pop_Right;

   ----------------------------------------------------------------------
   procedure Push_Right (Deque : in out Deque_Type;
                         Value : in     Value_Type) is
      use LFRC_Ops;
      Next, Prev : Deque_Node_Access;
      Node       : constant Deque_Node_Access := Create_Deque_Node;
   begin
      "+"(Node).Value := Value;
      Next := Dereference (Deque.Tail'Access);
      Prev := Dereference ("+"(Next).Previous'Access);

      loop
         if "+"(Prev).Next /= Unmark (Next) then

            Prev := Help_Insert (Prev, Next);

         else
            Store ("+"(Node).Previous'Access, Unmark (Prev));
            Store ("+"(Node).Next'Access, Unmark (Next));

            if Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Unmark (Node))
            then
               Release (Prev);
               exit;
            end if;
         end if;

         --  Back-off.
      end loop;

      Push_Common (Node, Next);

   end Push_Right;

   ----------------------------------------------------------------------
   function  Pop_Left  (Deque : access Deque_Type) return Value_Type is
      use LFRC_Ops;
      Next, Prev, Node, Old : Deque_Node_Access;
      Value                 : Value_Type;
   begin
      Prev := Dereference (Deque.Head'Access);

      loop
         Node := Dereference ("+"(Prev).Next'Access);
         if Node = Deque.Tail then

            Release (Node);
            Release (Prev);

            raise Deque_Empty;

         end if;

         Old := Dereference ("+"(Node).Next'Access);

         if Is_Marked (Old) then
            Delete_Next (Node);
            Release (Node);

         elsif Compare_And_Swap (Link      => "+"(Node).Next'Access,
                                 Old_Value => Old,
                                 New_Value => Mark (Old))
         then
            Delete_Next (Node);

            Next := Dereference ("+"(Node).Next'Access);
            Prev := Help_Insert (Prev, Next);

            Release (Prev);
            Release (Next);

            Value := "+"(Node).Value;
            Delete (Node);
            return Value;

         else
            Release (Node);
            --  Back-off.
         end if;
      end loop;

   end Pop_Left;

   ----------------------------------------------------------------------
   procedure Push_Left (Deque : in out Deque_Type;
                        Value : in     Value_Type) is
      use LFRC_Ops;
      Next, Prev : Deque_Node_Access;
      Node       : constant Deque_Node_Access := Create_Deque_Node;
   begin
      "+"(Node).Value := Value;
      Prev := Dereference (Deque.Head'Access);
      Next := Dereference ("+"(Prev).Next'Access);

      loop
         if "+"(Prev).Next /= Unmark (Next) then
            Release (Next);
            Next := Dereference ("+"(Prev).Next'Access);

         else
            Store ("+"(Node).Previous'Access, Unmark (Prev));
            Store ("+"(Node).Next'Access, Unmark (Next));

            if Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                 Old_Value => Unmark (Next),
                                 New_Value => Unmark (Node))
            then
               Release (Prev);
               exit;
            end if;
         end if;
         --  Back-off.
      end loop;

      Push_Common (Node, Next);
   end Push_Left;


   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------
   procedure Dispose (Node       : access Deque_Node;
                      Concurrent : in     Boolean) is
      use LFRC_Ops;
   begin
      if not Concurrent then
         Store (Node.Previous'Access, Null_Reference);
         Store (Node.Next'Access, Null_Reference);
      else
         declare
            Tmp : Deque_Node_Access;
         begin
            loop
               Tmp := Dereference (Node.Previous'Access);
               exit when Compare_And_Swap (Link      => Node.Previous'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
               Release (Tmp);
            end loop;
            Release (Tmp);

            loop
               Tmp := Dereference (Node.Next'Access);
               exit when Compare_And_Swap (Link      => Node.Next'Access,
                                           Old_Value => Tmp,
                                           New_Value => Null_Reference);
               Release (Tmp);
            end loop;
            Release (Tmp);
         end;
      end if;
   end Dispose;

   ----------------------------------------------------------------------
   procedure Clean_Up (Node : access Deque_Node) is
      use LFRC_Ops;
      Done : Boolean := False;
   begin
      while not Done loop
         Done := True;

         declare
            Prev, Prev2 : Deque_Node_Access;
         begin
            Prev := Dereference (Node.Previous'Access);

            if Prev /= Null_Reference and then Is_Marked (Node.Next) then
               --  Prev is logically deleted.
               Prev2 := Dereference ("+"(Prev).Previous'Access);
               Compare_And_Swap (Link      => Node.Previous'Access,
                                 Old_Value => Prev,
                                 New_Value => Mark (Prev2));
               Release (Prev2);

               Done := False;
            end if;

            Release (Prev);
         end;

         declare
            Next, Next2 : Deque_Node_Access;
         begin
            Next := Dereference (Node.Next'Access);
            if Next /= Null_Reference and then Is_Marked (Node.Next) then
               --  Next is logically deleted.
               Next2 := Dereference ("+"(Next).Next'Access);
               Compare_And_Swap (Link      => Node.Next'Access,
                                 Old_Value => Next,
                                 New_Value => Mark (Next2));
               Release (Next2);
               Done := False;
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
        Ada.Unchecked_Conversion (LFRC_Ops.Node_Access,
                                  New_Deque_Node_Access);

      X : New_Deque_Node_Access :=
        To_New_Deque_Node_Access (LFRC_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  Deque_Node type from the New_Deque_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------
   function Read (Link : access Deque_Node_Reference)
                 return Deque_Node_Access is
      use LFRC_Ops;
      Ref : Deque_Node_Access;
   begin
      Ref := Dereference (Link);
      if Is_Marked (Ref) then
         Release (Ref);
         return Null_Reference;
      else
         return Ref;
      end if;
   end Read;


   ----------------------------------------------------------------------
   procedure Push_Common (Node, Next : Deque_Node_Access) is
      use LFRC_Ops;
      Next_Prev : Deque_Node_Access;
   begin
      loop
         Next_Prev := Dereference ("+"(Next).Previous'Access);

         exit when Is_Marked (Next_Prev) or "+"(Node).Next /= Unmark (Next);

         if Compare_And_Swap (Link      => "+"(Next).Previous'Access,
                              Old_Value => Next_Prev,
                              New_Value => Unmark (Node))
         then
            Release (Next_Prev);

            if Is_Marked ("+"(Node).Previous) then
               declare
                  Prev : Deque_Node_Access;
               begin
                  Prev := Help_Insert (Node, Next);
               end;
            end if;

            exit;
         end if;

         --  Back-off.
      end loop;

      Release (Next);
      Release (Node);
   end Push_Common;

   ----------------------------------------------------------------------
   function Help_Insert (Prev, Node : in Deque_Node_Access)
                        return Deque_Node_Access is
      use LFRC_Ops;
      Prev1     : Deque_Node_Access := Prev;
      Prev_Next : Deque_Node_Access;
      Last_Link : Boolean := True;
   begin
      loop
         --  Should this be a Dereference that ignores deleted nodes?
         Prev_Next := Dereference ("+"(Prev1).Next'Access);

         if Prev_Next = Null_Reference then
            if not Last_Link then
               Delete_Next (Prev1);
               Last_Link := True;
            end if;

            declare
               Prev2 : Deque_Node_Access;
            begin
               Prev2 := Dereference ("+"(Prev1).Previous'Access);
               Release (Prev1);
               Prev1 := Prev2;
            end;
         else
            declare
               Old_Link : constant Deque_Node_Access :=
                 Dereference ("+"(Node).Previous'Access);
            begin
               if Is_Marked (Old_Link) then
                  Release (Prev_Next);
                  Release (Old_Link);
                  exit;
               end if;

               if Prev_Next /= Node then
                  Last_Link := False;
                  Release (Prev1);
                  Prev1 := Prev_Next;
               else
                  Release (Prev_Next);
                  if Compare_And_Swap (Link      => "+"(Node).Previous'Access,
                                       Old_Value => Old_Link,
                                       New_Value => Unmark (Prev1))
                  then
                     if not Is_Marked ("+"(Prev1).Previous) then
                        Release (Old_Link);
                        exit;
                     end if;
                  end if;
               end if;
               Release (Old_Link);
            end;
         end if;
         --  Back-off.
      end loop;

      return Prev1;
   end Help_Insert;

   ----------------------------------------------------------------------
   procedure Delete_Next (Node : in     Deque_Node_Access) is
      use LFRC_Ops;
   begin
      declare
         Old_Link : Deque_Node_Access;
      begin
         --  Set logically deleted mark on Node.Previous.
         loop
            Old_Link := Dereference ("+"(Node).Previous'Access);

            if Is_Marked (Old_Link) or else
              Compare_And_Swap (Link      => "+"(Node).Previous'Access,
                                Old_Value => Old_Link,
                                New_Value => Mark (Old_Link))
            then
               Release (Old_Link);
               exit;
            end if;

            Release (Old_Link);
         end loop;
      end;

      declare
         Prev, Next : Deque_Node_Access;
         Last_Link  : Boolean := True;
      begin
         --  Unlink Node from the active next chain.
         Prev := Dereference ("+"(Node).Previous'Access);
         Next := Dereference ("+"(Node).Next'Access);

         loop
            exit when Prev = Next;

            if Next = Null_Reference then
               Ada.Exceptions.Raise_Exception
                 (Constraint_Error'Identity,
                  "lock_free_deques.adb:469  Delete_Next: " &
                  "Node.Next is null! This should not happen!");
            end if;

            if Is_Marked ("+"(Next).Next) then
               declare
                  Next2 : Deque_Node_Access;
               begin
                  Next2 := Dereference ("+"(Next).Next'Access);
                  Release (Next);
                  Next := Next2;
               end;
            else
               declare
                  Prev_Next : Deque_Node_Access;
               begin
                  Prev_Next := Dereference ("+"(Prev).Next'Access);

                  if Prev_Next = Null_Reference then
                     if not Last_Link then
                        Delete_Next (Prev);
                        Last_Link := True;
                     end if;

                     declare
                        Prev2 : Deque_Node_Access;
                     begin
                        Prev2 := Dereference ("+"(Prev).Previous'Access);
                        Release (Prev);
                        Prev := Prev2;
                     end;
                  else
                     if Prev_Next /= Node then
                        Last_Link := False;
                        Release (Prev);
                        Prev := Prev_Next;
                     else
                        Release (Prev_Next);

                        exit when Compare_And_Swap
                          (Link      => "+"(Prev).Next'Access,
                           Old_Value => Unmark (Node),
                           New_Value => Unmark (Next));
                     end if;
                  end if;
               end;
            end if;
         end loop;

         Release (Prev);
         Release (Next);
      end;
   end Delete_Next;

end Lock_Free_Deques;
