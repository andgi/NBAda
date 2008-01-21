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
--  $Id: nbada-lock_free_flat_sets.adb,v 1.2 2008/01/21 18:21:31 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

with Ada.Text_IO;

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
   begin
      loop
         declare
            Status : constant Set_State := From.State;
            I      : Flat_Set_Size := Flat_Set_Size (Status.Last);
         begin
            if Status.Empty then
               raise Flat_Set_Empty;
            end if;
            for J in 0 .. From.Size loop
               declare
                  use AM;
                  Node  : constant Private_Reference :=
                    AM.Dereference (From.Set (I)'Access);
               begin
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
            if
              Compare_And_Swap (Target    => From.State'Access,
                                Old_Value => Status,
                                New_Value =>
                                  (True, Status.Last, Status.Version + 1))
            then
               raise Flat_Set_Empty;
            end if;
         end;
      end loop;
   end Get_Any;

   ----------------------------------------------------------------------------
   procedure Insert  (Into    : in out Flat_Set_Type;
                      Element : in out Element_Reference) is
   begin
      loop
         declare
            Status : Set_State     := Into.State;
            I      : Flat_Set_Size := Flat_Set_Size (Status.Last);
         begin
            Scan : for J in 0 .. Into.Size loop
               declare
                  use AM;
                  Dest   : AM.Private_Reference;
                  Result : AM.Move_Status;
               begin
                  Move_Into : loop
                     Dest :=
                       AM.Dereference (Into.Set (I)'Access);
                     exit Move_Into when Dest /= Null_Reference;

                     --  Preemptive reset of empty flag.
                     --  This could be undone by other operations.
                     --  Is it even needed?
                     if Status.Empty then
--                        Compare_And_Swap
--                          (Target    => Into.State'Access,
--                           Old_Value => Status,
--                           New_Value =>
--                             (False, Set_Index (I), Status.Version + 1));
                        Into.State :=
                          (False, Set_Index (I), Status.Version + 1);
                     end if;
                     Move (Element => Element,
                           To      => Into.Set (I)'Access,
                           Result  => Result);
                     case Result is
                        when Moved_Ok =>
--                           Compare_And_Swap
--                             (Target    => Into.State'Access,
--                              Old_Value => Status,
--                              New_Value =>
--                                (False, Set_Index (I), Status.Version + 1));
                           Into.State :=
                             (False, Set_Index (I), Status.Version + 1);
                           return;

                        when Moved_Away =>
                           return;

                        when others =>
                           null;
                     end case;
                  end loop Move_Into;
               end;
               Status := Into.State;
               if I = Into.Size then
                  I := 0;
               else
                  I := I + 1;
               end if;
            end loop Scan;
         end;
         --  The set might be full.
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

end NBAda.Lock_Free_Flat_Sets;
