-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : my_set.ads
--  Description     : Test of the lock-free set.
--  Author          : Anders Gidenstam
--  Created On      : Fri Mar 10 17:48:33 2006
--  $Id: my_set.ads,v 1.1 2006/03/10 18:43:50 anders Exp $
-------------------------------------------------------------------------------

pragma License (Modified_GPL);

with Process_Identification;
with Lock_Free_Sets;

package My_Set is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   subtype Key_Type is Natural;
   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   package Sets is new Lock_Free_Sets (Key_Type    => Key_Type,
                                       Value_Type  => Value_Type,
                                       Process_Ids => PID);

end My_Set;
