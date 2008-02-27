-------------------------------------------------------------------------------
--  Lock-Free Dicitionary Test - Test benchmark for lock-free dictionaries.
--
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
--  Filename        : test_dictionaries.adb
--  Description     : Test application for the lock-free dictionaries.
--  Author          : Anders Gidenstam
--  Created On      : Wed Feb 27 15:15:15 2008
--  $Id: test_dictionaries.adb,v 1.1 2008/02/27 17:27:27 andersg Exp $
-------------------------------------------------------------------------------

pragma License (GPL);

package body Test_Dictionaries is

   ----------------------------------------------------------------------------
   procedure Init    (Dictionary : in out Dictionary_Type) is
      use Trees;
   begin
      Dictionary.Mutex.Acquire;
      Init (Dictionary.Dictionary);
      Dictionary.Mutex.Release;
   end Init;

   ----------------------------------------------------------------------------
   procedure Insert  (Into  : in out Dictionary_Type;
                      Key   : in     Key_Type;
                      Value : in     Value_Type) is
      use Trees;
   begin
      Into.Mutex.Acquire;
      Insert (Into.Dictionary, Key, Value);
      Into.Mutex.Release;
   exception
      when Trees.Already_Present =>
         Into.Mutex.Release;
         raise Already_Present;
      when others =>
         Into.Mutex.Release;
         raise;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Delete  (From : in out Dictionary_Type;
                      Key  : in     Key_Type) is
      use Trees;
      Tmp : Value_Type;
   begin
      From.Mutex.Acquire;
      Tmp := Delete_Min (From.Dictionary);
      From.Mutex.Release;
   exception
      when Trees.Not_Found =>
         From.Mutable.Self.Mutex.Release;
         raise Not_Found;
      when others =>
         From.Mutable.Self.Mutex.Release;
         raise;
   end Delete;

   ----------------------------------------------------------------------------
   function  Lookup  (From : in Dictionary_Type;
                      Key  : in Key_Type)
                     return Value_Type is
      use Trees;
      Result : Value_Type;
   begin
      From.Mutable.Self.Mutex.Acquire;
      Result := Lookup (From.Dictionary, Key);
      From.Mutable.Self.Mutex.Release;
      return Result;
   exception
      when Trees.Not_Found =>
         From.Mutable.Self.Mutex.Release;
         raise Not_Found;
      when others =>
         From.Mutable.Self.Mutex.Release;
         raise;
   end Lookup;

   procedure Verify (Dictionary : in out Dictionary_Type;
                     Print      : in     Boolean := False) is
      use Trees;
   begin
      Verify (Dictionary.Dictionary, Print);
   end Verify;

   ----------------------------------------------------------------------------
   --  Private operations.
   ----------------------------------------------------------------------------

   protected body Mutex_Type is

      entry Acquire when not Locked is
      begin
         Locked := True;
      end Acquire;

      procedure Release is
      begin
         Locked := False;
      end Release;

   end Mutex_Type;

end Test_Dictionaries;
