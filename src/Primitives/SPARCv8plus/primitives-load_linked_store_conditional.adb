-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : primitives-load_linked_store_conditional.adb
-- Description     : Simulated LL and SC for SPARC v9.
-- Author          : Anders Gidenstam
-- Created On      : Wed Jul 10 11:41:48 2002
-- $Id: primitives-load_linked_store_conditional.adb,v 1.1 2002/07/12 15:02:45 andersg Exp $
-------------------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;
with System;

package body Primitives.Load_Linked_Store_Conditional is
  
  use type Primitives.Unsigned_32;
  
  type Element_Entry is
     record
	Linked : Boolean := False;
	Value  : Element;
     end record;
  type Element_Set is array (Unsigned_32 range <>) of Element_Entry;
  
  procedure Free is new Ada.Unchecked_Deallocation (Element_Set,
						    Active_Set);
  
  ----------------------------------------------------------------------------
  procedure Initialize (Set         : in out Active_Set;
			Buffer_Size : in     Natural) is
  begin
     Free (Set);
     Set := new Element_Set (0 .. Unsigned_32 (Buffer_Size) - 1);
  end Initialize;

  
  -------------------------------------------------------------------------
  function Load_Linked (Target    : access Element;
			Linked    : in     Active_Set)
		       return Element is
     type Element_Access is access all Element;
     function To_Unsigned_32 is new Ada.Unchecked_Conversion (Element_Access,
							      Unsigned_32);
     Value : Element     := Target.all;
     Pos   : Unsigned_32 :=
       To_Unsigned_32 (Element_Access (Target)) mod (Linked'Last + 1);
  begin
     Linked (Pos) := (True, Value);
     return Value;
  end Load_Linked;
  
  -------------------------------------------------------------------------
  function Store_Conditional (Target    : access Element;
			      New_Value : in     Element;
			      Linked    : in     Active_Set)
			     return Boolean is
     function CAS is new Primitives.Boolean_Compare_And_Swap_32 (Element);
     type Element_Access is access all Element;
     function To_Unsigned_32 is new Ada.Unchecked_Conversion (Element_Access,
							      Unsigned_32);
     Pos   : Unsigned_32 :=
       To_Unsigned_32 (Element_Access (Target)) mod (Linked'Last + 1);
  begin
     if Linked (Pos).Linked then
	Linked (Pos).Linked := False;
	return CAS (Target, Linked (Pos).Value, New_Value);
     else
	return False;
     end if;
  end Store_Conditional;

end Primitives.Load_Linked_Store_Conditional;
