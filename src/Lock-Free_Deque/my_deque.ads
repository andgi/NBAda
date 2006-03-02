-------------------------------------------------------------------------------
--                              -*- Mode: Ada -*-
--  Filename        : my_deque.ads
--  Description     : Instantiation of the lock-free deque.
--  Author          : Anders Gidenstam
--  Created On      : Thu Mar  2 16:41:48 2006
--  $Id: my_deque.ads,v 1.1 2006/03/02 15:45:18 anders Exp $
-------------------------------------------------------------------------------

with Process_Identification;
with Lock_Free_Deques;

package My_Deque is

   package PID is
      new Process_Identification (Max_Number_Of_Processes => 32);

   type Value_Type is
      record
         Creator : PID.Process_ID_Type;
         Index   : Integer;
      end record;

   package Deques is new Lock_Free_Deques (Value_Type  => Value_Type,
                                           Process_Ids => PID);

end My_Deque;
