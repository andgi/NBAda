-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
-- Filename        : process_identification.ads
-- Description     : Process IDs.
-- Author          : Anders Gidenstam
-- Created On      : Fri Nov 19 15:56:51 2004
-------------------------------------------------------------------------------

generic
   Max_Number_Of_Processes : Natural;
package Process_Identification is

   type Process_ID_Type is new Natural range 1 .. Max_Number_Of_Processes;

   --  Register a process ID for this task.
   procedure Register;

   --  Returns the process ID of the calling task.
   function Process_ID return Process_ID_Type;

end Process_Identification;
