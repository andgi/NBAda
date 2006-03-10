pragma Style_Checks (Off);
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_sets.adb
--  Description     : Lock-free list-based sets based on Maged Michael,
--                    "High Performance Dynamic Lock-Free Hash Tables and
--                    List-Based Sets", The 14th Annual ACM Symposium on
--                    Parallel Algorithms and Architectures (SPAA'02),
--                    pages 73-82, August 2002.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 12:23:47 2006
--  $Id: nbada-lock_free_sets.adb,v 1.1 2006/03/10 18:43:50 anders Exp $
-------------------------------------------------------------------------------
pragma Style_Checks (All_Checks);

pragma License (Modified_GPL);

with Lock_Free_Growing_Storage_Pools;

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Lock_Free_Sets is

   -------------------------------------------------------------------------
   --  Storage pool for the nodes.
   -------------------------------------------------------------------------

   Node_Pool : Lock_Free_Growing_Storage_Pools.Lock_Free_Storage_Pool
     (Block_Size => List_Node'Max_Size_In_Storage_Elements);

   type New_List_Node_Access is access List_Node;
   for New_List_Node_Access'Storage_Pool use Node_Pool;

   function Create_List_Node is new MRS_Ops.Create (New_List_Node_Access);

   procedure Find (Set             : in     Set_Type;
                   Key             : in     Key_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out List_Node_Access);
   --  Note: Prev, Cur, Next are Null_Reference or valid references that
   --        must be released. It is safe to Release a Null_Reference.

   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Init    (Set : in out Set_Type) is
      use MRS_Ops;
      Node : constant List_Node_Access := Create_List_Node;
   begin
      Store (Set.Head'Access, Node);
      Release (Node);
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Set_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is
      use MRS_Ops;
      Node : constant List_Node_Access := Create_List_Node;
   begin
      "+"(Node).Key   := Key;
      "+"(Node).Value := Value;
      loop
         declare
            Prev, Cur, Next : List_Node_Access;
            Found           : Boolean;
         begin
            Find (Into, Key,
                  Found,
                  Prev, Cur, Next);
            if Found then
               Release (Prev);
               Release (Cur);
               Release (Next);
               Delete  (Node);
               raise Already_Present;
            end if;
            Store ("+"(Node).Next'Access, Unmark (Cur));
            if
              Boolean_Compare_And_Swap (Link      => "+"(Prev).Next'Access,
                                        Old_Value => Unmark (Cur),
                                        New_Value => Node)
            then
               Release (Prev);
               Release (Cur);
               Release (Next);
               Release (Node);
               return;
            else
               Release (Prev);
               Release (Cur);
               Release (Next);
            end if;
         end;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From : in out Set_Type;
                      Key  : in     Key_Type) is
      use MRS_Ops;
   begin
      loop
         declare
            Prev, Cur, Next : List_Node_Access;
            Found           : Boolean;
         begin
            Find (From, Key,
                  Found,
                  Prev, Cur, Next);
            if not Found then
               Release (Prev);
               Release (Next);
               return;
            end if;

            if
              Boolean_Compare_And_Swap
              (Link      => "+"(Cur).Next'Access,
               Old_Value => Unmark (Next),
               New_Value => Mark (Next))
            then
               if
                 Boolean_Compare_And_Swap
                 (Link      => "+"(Prev).Next'Access,
                  Old_Value => Unmark (Cur),
                  New_Value => Unmark (Next))
               then
                  Delete (Cur);
               else
                  Release (Prev);
                  Release (Cur);
                  Release (Next);
                  Find (From, Key,
                        Found,
                        Prev, Cur, Next);
                  Release (Cur);
               end if;

               Release (Prev);
               Release (Next);
               return;
            end if;
            Release (Prev);
            Release (Cur);
            Release (Next);
         end;
      end loop;
   end Delete;

   ----------------------------------------------------------------------------
   function  Find    (Set : in Set_Type;
                      Key : in Key_Type) return Value_Type is
      use MRS_Ops;
      Prev, Cur, Next : List_Node_Access;
      Found           : Boolean;
   begin
      Find (Set, Key,
            Found,
            Prev, Cur, Next);
      if Found then
         declare
            Value : constant Value_Type := "+"(Cur).Value;
         begin
            Release (Prev);
            Release (Cur);
            Release (Next);
            return Value;
         end;
      else
         Release (Prev);
         Release (Next);
         raise Not_Found;
      end if;
   end Find;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Free     (Node : access List_Node) is

      procedure Reclaim is new
        Ada.Unchecked_Deallocation (List_Node,
                                    New_List_Node_Access);
      function To_New_List_Node_Access is new
        Ada.Unchecked_Conversion (MRS_Ops.Node_Access,
                                  New_List_Node_Access);

      X : New_List_Node_Access :=
        To_New_List_Node_Access (MRS_Ops.Node_Access (Node));
      --  This is dangerous in the general case but here we know
      --  for sure that we have allocated all the nodes of the
      --  List_Node type from the New_List_Node_Access pool.
   begin
      Reclaim (X);
   end Free;

   ----------------------------------------------------------------------------
   procedure Find (Set             : in     Set_Type;
                   Key             : in     Key_Type;
                   Found           :    out Boolean;
                   Prev, Cur, Next :    out List_Node_Access) is
      use MRS_Ops;

      type Mutable_Access is access all List_Node_Reference;
      type Immutable_Access is access constant List_Node_Reference;
      function To_Mutable is new Ada.Unchecked_Conversion
        (Immutable_Access, Mutable_Access);

      Mutable_Set_Head : constant Mutable_Access :=
        To_Mutable (Set.Head'Access);
   begin
      loop
         Prev := Dereference (Mutable_Set_Head);
         --  Prev is a dummy node always present at the head of the list.
         if "+"(Prev) = null then
            --  This should not happen!
            raise Constraint_Error;
         end if;
         Cur  := Dereference ("+"(Prev).Next'Access);

         Traverse :
         loop
            if "+"(Cur) = null then
               Found := False;
               Next  := Null_Reference;
               --  Prev is a valid reference. Cur and Next are null.
               return;
            end if;

            Next := Dereference ("+"(Cur).Next'Access);
            if "+"(Prev).Next /= Unmark (Cur) then
               --  Retry from the list head.
               exit Traverse;
            else
               if not Is_Marked (Next) then
                  if not ("+"(Cur).Key < Key) then
                     Found :=  "+"(Cur).Key = Key;
                     --  Prev is a valid reference. Cur and Next are valid
                     --  references or null.
                     return;
                  else
                     Release (Prev);
                     Prev := Cur;
                     Cur  := Null_Reference;
                  end if;
               else
                  --  Cur.Next is marked, i.e. Cur is logically deleted.
                  if
                    Boolean_Compare_And_Swap
                    (Link      => "+"(Prev).Next'Access,
                     Old_Value => Unmark (Cur),
                     New_Value => Unmark (Next))
                  then
                     Delete (Cur);
                     Cur := Null_Reference;
                  else
                     --  Retry from the list head.
                     exit Traverse;
                  end if;
               end if;
            end if;
            Release (Cur);
            Cur  := Next;
            Next := Null_Reference;
         end loop Traverse;
         Release (Prev);
         Release (Cur);
         Release (Next);
      end loop;
   end Find;

end Lock_Free_Sets;

