-------------------------------------------------------------------------------
--  Lock-free Flat-sets - An implementation of A. Gidenstam et al.'s
--                        lock-free flat-set algorithm.
--  Copyright (C) 2008  Anders Gidenstam
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
--  Filename        : nbada-lock_free_flat_sets.adb
--  Description     : A lock-free flat-set based on A. Gidenstam,
--                    M. Papatriantafilou and P. Tsigas,
--                    "Allocating memory in a lock-free manner",
--                    The 13th Annual European Symposium on Algorithms
--                    (ESA 2005), LNCS 3669, pages 329 - 242, 2005.
--  Author          : Anders Gidenstam
--  Created On      : Tue Jan 15 19:05:03 2008
--  $Id: nbada-lock_free_flat_sets.adb,v 1.4 2008/07/29 14:35:07 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Text_IO;
with Ada.Unchecked_Conversion;

package body NBAda.Lock_Free_Flat_Sets is

   procedure Compare_And_Swap is
      new Primitives.Void_Compare_And_Swap_32 (Set_State);
   function  Compare_And_Swap is
      new Primitives.Boolean_Compare_And_Swap_32 (Set_State);


   ----------------------------------------------------------------------------
   --  Public operations.
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function New_Element (Element : Element_Type) return Element_Reference is
   begin
      return Element_Reference (AM.Create (Element));
   end New_Element;

   ----------------------------------------------------------------------------
   function Dereference (Ref : Element_Reference) return Element_Access is
   begin
      return Element_Access (AM."+" (AM.Private_Reference (Ref)));
   end Dereference;

   ----------------------------------------------------------------------------
   procedure Get_Any (From    : in out Flat_Set_Type;
                      Element :    out Element_Reference) is
      use type Primitives.Standard_Unsigned;
      function To_Unsigned is
         new Ada.Unchecked_Conversion (AM.Shared_Location,
                                       Primitives.Standard_Unsigned);
      Last_Checksum : Primitives.Standard_Unsigned := 0;
      Checks        : constant := 2;
      Check         : Natural := 1;
      Checksum      : Primitives.Standard_Unsigned;
   begin
      loop
         declare
            Status : constant Set_State := From.State;
            I      : Flat_Set_Size := Flat_Set_Size (Status.Last);
         begin
            Checksum := 0;
            for J in 0 .. From.Size loop
               declare
                  use AM;
                  Node  : constant Private_Reference :=
                    AM.Dereference (From.Set (I)'Access);
               begin
                  Checksum := Checksum + To_Unsigned (From.Set (I));
                  if Node /= Null_Reference then
                     Compare_And_Swap
                       (Target    => From.State'Access,
                        Old_Value => Status,
                        New_Value =>
                          (False, Set_Index (I), Status.Version + 1));
                     Element := Element_Reference (Node);
                     return;
                  end if;
               end;
               if I = 0 then
                  I := From.Size;
               else
                  I := I - 1;
               end if;
            end loop;
         end;
         --  The set might be empty.
         if Checksum = Last_Checksum and Check >= Checks then
            raise Flat_Set_Empty;
         elsif Checksum = Last_Checksum then
            Check := Check + 1;
         else
            Last_Checksum := Checksum;
            Check := 1;
         end if;
      end loop;
   end Get_Any;

   ----------------------------------------------------------------------------
   procedure Insert  (Into    : in out Flat_Set_Type;
                      Element : in out Element_Reference) is
      use type Primitives.Standard_Unsigned;
      function To_Unsigned is
         new Ada.Unchecked_Conversion (AM.Shared_Location,
                                       Primitives.Standard_Unsigned);
      Last_Checksum : Primitives.Standard_Unsigned := 0;
      Checks        : constant := 2;
      Check         : Natural := 1;
      Checksum      : Primitives.Standard_Unsigned;
   begin
      loop
         declare
            Status : Set_State     := Into.State;
            I      : Flat_Set_Size := Flat_Set_Size (Status.Last);
         begin
            Primitives.Membar;
            Checksum := 0;
            Scan : for J in 0 .. Into.Size loop
               declare
                  use AM;
                  Dest   : AM.Private_Reference;
                  Result : AM.Move_Status;
               begin
                  Checksum := Checksum + To_Unsigned (Into.Set (I));
                  Move_Into : loop
                     Dest :=
                       AM.Dereference (Into.Set (I)'Access);
                     exit Move_Into when Dest /= Null_Reference;

                     Move (Element => Element,
                           To      => Into.Set (I)'Access,
                           Result  => Result);
                     case Result is
                        when Moved_Ok =>
                           Compare_And_Swap
                             (Target    => Into.State'Access,
                              Old_Value => Status,
                              New_Value =>
                                (False, Set_Index (I), Status.Version + 1));
                           return;

                        when No_Op =>
                           if not Fresh (Element) then
                              return;
                           end if;

                        when others =>
                           raise Program_Error;
                     end case;
                  end loop Move_Into;
               end;
               Status := Into.State;
               Primitives.Membar;
               if I = Into.Size then
                  I := 0;
               else
                  I := I + 1;
               end if;
            end loop Scan;
         end;
         --  The set might be full.
         if Checksum = Last_Checksum and Check >= Checks then
            raise Flat_Set_Full;
         elsif Checksum = Last_Checksum then
            Check := Check + 1;
         else
            Last_Checksum := Checksum;
            Check := 1;
         end if;
      end loop;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Dump (Set : in Flat_Set_Type) is
      use AM;
   begin
      Ada.Text_IO.Put_Line ("[");
      for I in Set.Set'Range loop
         Ada.Text_IO.Put_Line ("  " & Image (Set.Set (I)));
      end loop;
      Ada.Text_IO.Put_Line ("]");
   end Dump;

   function Number_Of_Elements (Set : access Flat_Set_Type) return Natural is
      use AM;
      Count : Natural := 0;
   begin
      for I in Set.Set'Range loop
         if
           not (AM.Dereference (Set.Set (I)'Access) = Null_Reference)
         then
            Count := Count + 1;
         end if;
      end loop;
      return Count;
   end Number_Of_Elements;

end NBAda.Lock_Free_Flat_Sets;
