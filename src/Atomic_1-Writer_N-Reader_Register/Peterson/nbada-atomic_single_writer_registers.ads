--                              -*- Mode: Ada -*-
-- Filename        : wait_free.ads
-- Description     :
-- Author          : Anders Gidenstam
-- Created On      : Fri Oct 19 23:57:47 2001
-- $Id: nbada-atomic_single_writer_registers.ads,v 1.1 2001/10/20 17:45:55 d96andgi Exp $

generic
   type Element_Type is private;

package Wait_Free is

   ----------------------------------------------------------------------------
   --
   -- 1-writer, N-reader atomic register
   --
   -- The algorithm is from 'Concurrent Reading While Writing',
   -- Gary L. Peterson, 1983.
   ----------------------------------------------------------------------------
   type Atomic_1_M_Register (No_Of_Readers : Positive) is limited private;

   procedure Write (Register : in out Atomic_1_M_Register;
                    Value    : in     Element_Type);
   procedure Read  (Register  : in out Atomic_1_M_Register;
                    Reader_No : in     Positive;
                    Value     :    out Element_Type);

private

   type Atomic_Boolean is new Boolean;
--   pragma Atomic (Atomic_Boolean);

   type Boolean_Array is array (Positive range <>) of Atomic_Boolean;
   pragma Atomic_Components (Boolean_Array);
   type Element_Array is array (Positive range <>) of Element_Type;
   pragma Volatile_Components (Element_Array);

   type Atomic_1_M_Register (No_Of_Readers : Positive) is
      record
         Reading  : Boolean_Array (1 .. No_Of_Readers) := (others => False);
         Writing  : Boolean_Array (1 .. No_Of_Readers) := (others => False);
         Wflag    : Atomic_Boolean := False;
         Switch   : Atomic_Boolean := False;
         Buff1    : Element_Type;
         pragma Volatile (Buff1);
         Buff2    : Element_Type;
         pragma Volatile (Buff2);
         Copybuff : Element_Array (1 .. No_Of_Readers);
         pragma Volatile (Copybuff);
      end record;

end Wait_Free;

