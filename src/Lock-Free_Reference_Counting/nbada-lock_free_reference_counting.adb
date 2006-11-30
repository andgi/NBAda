-------------------------------------------------------------------------------
--  Lock-Free Reference Counting - Lock-Free Reference Counting based on the
--  algorithm by Herlihy et al.
--  Copyright (C) 2006  Anders Gidenstam
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
--  As a special exception, if other files instantiate generics from this
--  unit, or you link this unit with other files to produce an executable,
--  this unit does not by itself cause the resulting executable to be
--  covered by the GNU General Public License. This exception does not
--  however invalidate any other reasons why the executable file might be
--  covered by the GNU Public License.
--
-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : lock_free_reference_counting.adb
--  Description     : Ada implementation of lock-free reference counting.
--                    Based on M. Herlihy, V. Luchango, P. Martin, M. Moir,
--                    "Nonblocking Memory Management Support for Dynamic-Sized
--                     Data Structures",  ACM Transactions on Computer Systems,
--                    23(2), 147--196, May 2005.
--  Author          : Anders Gidenstam
--  Created On      : Wed Nov 29 16:55:18 2006
--  $Id: nbada-lock_free_reference_counting.adb,v 1.2 2006/11/30 23:58:56 andersg Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Pass_The_Buck;

with Ada.Unchecked_Conversion;

with Ada.Exceptions;
with Ada.Text_IO;

package body Lock_Free_Reference_Counting is

   ----------------------------------------------------------------------------
   --  Types.
   ----------------------------------------------------------------------------

   function Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Reference_Count);

   procedure Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                            Increment : in     Primitives.Unsigned_32)
     renames Primitives.Fetch_And_Add;

   function Fetch_And_Add (Target    : access Primitives.Unsigned_32;
                           Increment : in     Primitives.Unsigned_32)
                          return Primitives.Unsigned_32
     renames Primitives.Fetch_And_Add;

   procedure Clean_And_Liberate (Node : in Managed_Node_Access);

   package PTB is new Pass_The_Buck
     (Max_Number_Of_Guards => Max_Number_Of_Guards,
      Value_Type           => Managed_Node_Access,
      Null_Value           => null);

   ----------------------------------------------------------------------------
   --  Internal data structures.
   ----------------------------------------------------------------------------

   --  Statistics counters.
   No_Nodes_Created   : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Created);
   No_Nodes_Reclaimed : aliased Primitives.Unsigned_32 := 0;
   pragma Atomic (No_Nodes_Reclaimed);

   ----------------------------------------------------------------------------
   --  Operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   package body Operations is

      ----------------------------------------------------------------------
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Shared_Reference,
                                       Private_Reference);
      function To_Private_Reference is
         new Ada.Unchecked_Conversion (Node_Access,
                                       Private_Reference);

      function To_Node_Access (X : Private_Reference)
                              return Node_Access;
      pragma Inline (To_Node_Access);

      type Shared_Reference_Base_Access is access all Shared_Reference_Base;
      type Shared_Reference_Access is access all Shared_Reference;
      function To_Shared_Reference_Base_Access is
         new Ada.Unchecked_Conversion (Shared_Reference_Access,
                                       Shared_Reference_Base_Access);

      function Compare_And_Swap_32 is
         new Primitives.Boolean_Compare_And_Swap_32 (Shared_Reference_Base);

      Mark_Mask  : constant Private_Reference := 2 ** Mark_Bits - 1;
      Ref_Mask   : constant Private_Reference := -(2 ** Mark_Bits);

      ----------------------------------------------------------------------
      function  Dereference (Link : access Shared_Reference)
                            return Private_Reference is

         Guard    : constant PTB.Guard_Type := PTB.Hire_Guard;
         Node_Ref : Private_Reference;
      begin
         Dereference :
         loop
            Node_Ref := To_Private_Reference (Link.all);

            exit Dereference when To_Node_Access (Node_Ref) = null;

            PTB.Post_Guard (Guard,
                            Managed_Node_Access (To_Node_Access (Node_Ref)));

            if To_Private_Reference (Link.all) = Node_Ref then
               declare
                  use type Reference_Count;
                  Node_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (Node_Ref));
                  --  Base type view of the node.
                  Old_RC    : Reference_Count := Node_Base.MM_RC;
               begin
                  while Old_RC > 0 loop
                     exit Dereference when Compare_And_Swap
                       (Target    => Node_Base.MM_RC'Access,
                        Old_Value => Old_RC,
                        New_Value => Old_RC + 1);
                     Old_RC := Node_Base.MM_RC;
                  end loop;
               end;
            end if;
         end loop Dereference;

         PTB.Post_Guard (Guard, null);
         PTB.Fire_Guard (Guard);
         return Node_Ref;
      end Dereference;

      ----------------------------------------------------------------------
      procedure Release (Node : in Private_Reference) is
         use type Reference_Count;
      begin
         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
               --  Base type view of the node.
            begin
               if Fetch_And_Add (Target    => Node_Base.MM_RC'Access,
                                 Increment => -1) = 1
               then
                  Clean_And_Liberate (Node_Base);
--                    Dispose (Node_Base);
--                    declare
--                       Old : constant PTB.Value_Set (1 .. 1)
--                         := (Node_Base, others => null);
--                       VS  : constant PTB.Value_Set
--                         := PTB.Liberate (Old);
--                    begin
--                       for I in VS'Range loop
--                          Free    (VS (I));
--                       end loop;
--                       if Debug then
--                          Fetch_And_Add (No_Nodes_Reclaimed'Access, VS'Length);
--                       end if;
--                    end;
               end if;
            end;
         end if;
      end Release;

      ----------------------------------------------------------------------
      function  "+"     (Node : in Private_Reference)
                        return Node_Access renames To_Node_Access;

      ----------------------------------------------------------------------
      function Deref (Node : in Private_Reference)
                     return Node_Access renames To_Node_Access;


      ----------------------------------------------------------------------
      function  Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in Private_Reference;
                                  New_Value : in Private_Reference)
                                 return Boolean is
         use type Reference_Count;
      begin
         --  Since we have dereferenced both Old_Value and New_Value they
         --  are guaranteed to have positive reference counts, regardless
         --  how the CAS goes.
         if
           Compare_And_Swap_32
           (Target    =>
              To_Shared_Reference_Base_Access (Link.all'Unchecked_Access),
            Old_Value => (Ref => Shared_Reference_Base_Impl (Old_Value)),
            New_Value => (Ref => Shared_Reference_Base_Impl (New_Value)))
         then
            if To_Node_Access (New_Value) /= null then
               declare
                  New_Value_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (New_Value));
                  --  Base type view of the node.
               begin
                  Fetch_And_Add (New_Value_Base.MM_RC'Access, 1);
               end;
            end if;

            if To_Node_Access (Old_Value) /= null then
               declare
                  Old_Value_Base : constant Managed_Node_Access :=
                    Managed_Node_Access (To_Node_Access (Old_Value));
                  --  Base type view of the node.
               begin
                  if Fetch_And_Add (Target    => Old_Value_Base.MM_RC'Access,
                                    Increment => -1) = 1
                  then
                     Clean_And_Liberate (Old_Value_Base);
--                       Dispose (Old_Value_Base);
--                       declare
--                          Old : constant PTB.Value_Set (1 .. 1)
--                            := (Old_Value_Base, others => null);
--                          VS  : constant PTB.Value_Set
--                            := PTB.Liberate (Old);
--                       begin
--                          for I in VS'Range loop
--                             Free    (VS (I));
--                          end loop;
--                          if Debug then
--                             Fetch_And_Add (No_Nodes_Reclaimed'Access,
--                                            VS'Length);
--                          end if;
--                       end;
                  end if;
               end;
            end if;

            return True;
         end if;

         return False;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Compare_And_Swap (Link      : access Shared_Reference;
                                  Old_Value : in     Private_Reference;
                                  New_Value : in     Private_Reference) is
         use type Reference_Count;
      begin
         if
           Compare_And_Swap (Link,
                             Old_Value,
                             New_Value)
         then
            null;
         end if;
      end Compare_And_Swap;

      ----------------------------------------------------------------------
      procedure Store   (Link : access Shared_Reference;
                         Node : in Private_Reference) is
         use type Reference_Count;
         Old : constant Node_Access :=
           To_Node_Access (To_Private_Reference (Link.all));
      begin
         To_Shared_Reference_Base_Access (Link.all'Unchecked_Access).all :=
           (Ref => Shared_Reference_Base_Impl (Node));

         if To_Node_Access (Node) /= null then
            declare
               Node_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (To_Node_Access (Node));
               --  Base type view of the node.
            begin
               Fetch_And_Add (Node_Base.MM_RC'Access, 1);
            end;
         end if;

         if Old /= null then
            declare
               Old_Base : constant Managed_Node_Access :=
                 Managed_Node_Access (Old);
               --  Base type view of the node.
            begin
               if Fetch_And_Add (Target    => Old_Base.MM_RC'Access,
                                 Increment => -1) = 1
               then
                  --  NOTE: We cannot allow recusrion here as it will easily
                  --        overflow the stack.

                  Clean_And_Liberate (Old_Base);
--                    Dispose (Old_Base);
--                    declare
--                       Old : constant PTB.Value_Set (1 .. 1)
--                         := (Old_Base, others => null);
--                       VS  : constant PTB.Value_Set
--                         := PTB.Liberate (Old);
--                    begin
--                       for I in VS'Range loop
--                          Free    (VS (I));
--                       end loop;
--                       if Debug then
--                          Fetch_And_Add (No_Nodes_Reclaimed'Access, VS'Length);
--                       end if;
--                    end;
               end if;
            end;
         end if;

      exception
         when E : Storage_Error =>
            Ada.Text_IO.Put_Line ("raised " &
                                  Ada.Exceptions.Exception_Name (E) &
                                  " : " &
                                  Ada.Exceptions.Exception_Message (E) &
                                  "   " &
                                  " in Lock_Free_Reference_Counting.Store");
      end Store;

      ----------------------------------------------------------------------
      function Create return Private_Reference is
         UNode : constant User_Node_Access := new Managed_Node;
         Node  : constant Node_Access      := UNode.all'Unchecked_Access;
      begin
         Node.all.MM_RC := 1;

         if Debug then
            Fetch_And_Add (No_Nodes_Created'Access, 1);
         end if;

         return To_Private_Reference (Node);
      end Create;

      ----------------------------------------------------------------------
      procedure Mark      (Node : in out Private_Reference) is
      begin
         Node := Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      function  Mark      (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node or 1;
      end Mark;

      ----------------------------------------------------------------------
      procedure Unmark    (Node : in out Private_Reference) is
      begin
         Node := Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Unmark    (Node : in     Private_Reference)
                          return Private_Reference is
      begin
         return Node and Ref_Mask;
      end Unmark;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Private_Reference)
                          return Boolean is
      begin
         return (Node and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function  Is_Marked (Node : in     Shared_Reference)
                          return Boolean is
      begin
         return (To_Private_Reference (Node) and Mark_Mask) = 1;
      end Is_Marked;

      ----------------------------------------------------------------------
      function "=" (Link : in     Shared_Reference;
                    Ref  : in     Private_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";

      ----------------------------------------------------------------------
      function "=" (Ref  : in     Private_Reference;
                    Link : in     Shared_Reference) return Boolean is
      begin
         return To_Private_Reference (Link) = Ref;
      end "=";


      ----------------------------------------------------------------------
      function To_Node_Access (X : Private_Reference)
                                     return Node_Access is

         function To_Node_Access is
            new Ada.Unchecked_Conversion (Private_Reference,
                                          Node_Access);

      begin
         return To_Node_Access (X and Ref_Mask);
      end To_Node_Access;

      ----------------------------------------------------------------------
   end Operations;

   ----------------------------------------------------------------------------
   procedure Print_Statistics is
   begin
      Ada.Text_IO.Put_Line ("Lock_Free_Reference_Counting.Print_Statistics:");
      Ada.Text_IO.Put_Line ("  #Created = " &
                            Primitives.Unsigned_32'Image (No_Nodes_Created));
      Ada.Text_IO.Put_Line ("  #Reclaimed = " &
                            Primitives.Unsigned_32'Image (No_Nodes_Reclaimed));

   end Print_Statistics;

   ----------------------------------------------------------------------------
   procedure Clean_And_Liberate (Node : in Managed_Node_Access) is

      function To_Managed_Node_Access (X : Shared_Reference_Base)
                                      return Managed_Node_Access;
      function To_Managed_Node_Access (X : Shared_Reference_Base)
                                      return Managed_Node_Access is

         function To_Managed_Node_Access is new
           Ada.Unchecked_Conversion (Shared_Reference_Base_Impl,
                                     Managed_Node_Access);

      begin
         return To_Managed_Node_Access (X.Ref and Ref_Mask);
      end To_Managed_Node_Access;

      use type Reference_Count;
      Refs : constant Reference_Set := All_References (Node);
   begin
      loop
         declare
            All_Null : Boolean := True;
         begin
            for I in Refs'Range loop
               declare
                  Next : constant Managed_Node_Access :=
                    To_Managed_Node_Access (Refs (I).all);
               begin
                  if Next /= null then
                     --  Lower the Next's RC.
                     if Fetch_And_Add (Target    => Next.MM_RC'Access,
                                       Increment => -1) = 1
                     then
                        declare
                           Next_Refs : constant Reference_Set :=
                             All_References (Next);
                        begin
                           All_Null              := False;
                           Refs (I).all          := Next_Refs (I).all;
                           Next_Refs (I).all.Ref := 0;
                           Clean_And_Liberate (Next);
                        end;
                     else
                        Refs (I).all.Ref := 0;
                     end if;
                  end if;
               end;
            end loop;

            exit when All_Null;
         end;
      end loop;

      declare
         Old : constant PTB.Value_Set (1 .. 1)
           := (1 => Node);
         VS  : constant PTB.Value_Set
           := PTB.Liberate (Old);
      begin
         for I in VS'Range loop
            Free    (VS (I));
         end loop;
         if Debug then
            Fetch_And_Add (No_Nodes_Reclaimed'Access, VS'Length);
         end if;
      end;
   end Clean_And_Liberate;

end Lock_Free_Reference_Counting;
