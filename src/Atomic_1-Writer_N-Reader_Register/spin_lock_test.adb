-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : spin_lock_test.adb
-- Description     : Small tests of spin-locks in SPARC assembler.
-- Author          : Anders Gidenstam
-- Created On      : Thu Jan 24 13:09:04 2002
-- $Id: spin_lock_test.adb,v 1.1 2002/01/24 16:11:26 anders Exp $
-------------------------------------------------------------------------------

with System.Machine_Code;
with Interfaces.C;
with Ada.Text_IO;

procedure Spin_Lock_Test is

   package Text_IO renames Ada.Text_IO;
   package MC      renames System.Machine_Code;
   package C       renames Interfaces.C;
   use type C.Int;

   type Unsigned_Byte is new C.Unsigned_Char;
--   pragma Atomic (Unsigned_Byte);
   type Access_Unsigned_Byte is access all Unsigned_Byte;

   procedure Spin_Lock (Lock : access Unsigned_Byte) is
   begin
      MC.Asm (Template =>
                "retry:         ; " &
                "ldstub %0, %%l0; " &
                "tst    %%l0    ; " &
                "be     out     ; " &
                "nop            ; " &
                "loop:          ; " &
                "ldub   %0, %%l0; " &
                "tst    %%l0    ; " &
                "bne    loop    ; " &
                "nop            ; " &
                "ba,a   retry   ; " &
                "out:           ; " &
                -- V8: PSO
                "nop",
                -- V9: PSO, RMO requires -Wa,-xarch=v8plus
--                "membar #LoadLoad | #LoadStore",
              Inputs => Unsigned_Byte'Asm_Input
                ("m", Lock.all),
              Clobber  => "%l0",
              Volatile => True);
   end Spin_Lock;

   procedure Spin_Unlock (Lock : access Unsigned_Byte) is
   begin
      MC.Asm (Template =>
                -- V8: PSO
                "stbar              ; " &
                -- V9: PSO, RMO requires -Wa,-xarch=v8plus
--                "membar #StoreStore ; " &
--                "membar #LoadStore  ; " &
                "stub   %%g0,%0     ; ",
              Inputs => Unsigned_Byte'Asm_Input
                ("m", Lock.all),
              Volatile => True);
   end Spin_Unlock;

   CAS_Lock : aliased Unsigned_Byte := 0;
   function Compare_And_Swap (Address   : access C.Int;
                              Old_Value : in     C.Int;
                              New_Value : in     C.Int)
                             return Boolean is
      pragma Suppress (All_Checks);
   begin
      Spin_Lock (CAS_Lock'Access);
      if Address.all = Old_Value then
         Address.all := New_Value;
         Spin_Unlock (CAS_Lock'Access);
         return True;
      else
         Spin_Unlock (CAS_Lock'Access);
         return False;
      end if;
   end Compare_And_Swap;

   function Compare_And_Compare_And_Swap (Address   : access C.Int;
                                          Old_Value : in     C.Int;
                                          New_Value : in     C.Int)
                                         return Boolean is
      pragma Suppress (All_Checks);
   begin
      -- Needs to have the spin-lock code integrated.
      MC.Asm (Template =>
                "retry:         ; " &
                "ldstub %0, %%l0; " &
                "tst    %%l0    ; " &
                "be     out     ; " &
                "nop            ; " &
                "loop:          ; " &
                "ldub   %0, %%l0; " &
                "tst    %%l0    ; " &
                "bne    loop    ; " &
                "nop            ; " &
                "ba,a   retry   ; " &
                "out:           ; " &
                -- V8: PSO
                "nop",
                -- V9: PSO, RMO requires -Wa,-xarch=v8plus
--                "membar #LoadLoad | #LoadStore",
              Inputs => Unsigned_Byte'Asm_Input
                ("m", Lock.all),
              Clobber  => "%l0",
              Volatile => True);
      return False;
   end Compare_And_Compare_And_Swap;

   type My_String is new String (1..40);
   type My_String_Array is array (1..2) of My_String;

   task type Writer (No : Positive);
   task type Reader (No : Positive);

   W1  : Writer (1);
   W2  : Writer (2);
   R1  : Reader (1);
   R2  : Reader (2);
   R3  : Reader (3);
   R4  : Reader (4);
   R5  : Reader (5);
   R6  : Reader (6);
   R7  : Reader (7);
   R8  : Reader (8);
   R9  : Reader (9);
   R10 : Reader (10);

   Str : constant My_String_Array :=
     ("Hej din gamla hoppräka!                 ",
      "Sju sjösjuka sjömän från Shanghai..     ");

   SLock : aliased Unsigned_Byte;
   Data : aliased My_String := Str(1);
   pragma Volatile (Data);

   task body Writer is
   begin
      loop
         -- Get lock
         Spin_Lock (SLock'Access);
--         Text_IO.Put ("w" & Integer'Image (No));

         Data := Str (No);

         -- Release lock
         Spin_Unlock (SLock'Access);

         delay 0.0;
      end loop;
   end Writer;

   task body Reader is
   begin
      delay 1.0;
      loop
         -- Get lock
         Spin_Lock (SLock'Access);
--         Text_IO.Put ("r" & Integer'Image (No));

         if Data /= Str (1) and Data /= Str (2) then
            Text_IO.Put_Line ("R" &
                              Integer'Image (No) & ": " &
                              String (Data));
         end if;

         -- Release lock
         Spin_Unlock (SLock'Access);

         delay 0.0;
      end loop;
   end Reader;

begin
   Text_IO.Put_Line ("Start");
   MC.Asm ("nop");
   MC.Asm ("nop");
--   MC.Asm ("save");
   MC.Asm ("nop");
--   MC.Asm ("restore");
   MC.Asm ("nop");
   MC.Asm ("nop");
end Spin_Lock_Test;
