-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : process_identification.adb
-- Description     : Process IDs.
-- Author          : Anders Gidenstam
-- Created On      : Fri Nov 19 16:06:16 2004
-------------------------------------------------------------------------------

with Ada.Task_Attributes;
with Primitives;

package body Process_Identification is

   subtype Process_ID_Base is Primitives.Unsigned_32;

   package Process_IDs is
      new Ada.Task_Attributes (Attribute     => Process_ID_Base,
                               Initial_Value => 0);

   --  Shared process id counter.
   Process_Count : aliased Process_ID_Base := 0;
   pragma Atomic (Process_Count);

   ----------------------------------------------------------------------------
   --  Register a process ID for this task.
   procedure Register is
      use type Process_ID_Base;
   begin
      if Process_IDs.Value = 0 then
         Process_IDs.Set_Value
           (Primitives.Fetch_And_Add (Target    => Process_Count'Access,
                                      Increment => 1) + 1);
      end if;
   end Register;

   ----------------------------------------------------------------------------
   --  Returns the process ID of the calling task.
   function Process_ID return Process_ID_Type is
   begin
      return Process_ID_Type (Process_IDs.Value);
   end Process_ID;

end Process_Identification;
