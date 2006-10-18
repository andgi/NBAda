with Lock_Based_Stack;
with Process_Identification;

generic
   type Element_Type is private;
   --  Element type.
   with package Process_Ids is
     new Process_Identification (<>);
   --  Process identification.
package Test_Stack is

   package Stack is new
     Lock_Based_Stack (Element_Type, Process_Ids);

   ----------------------------------------------------------------------------
   --  Stack.
   ----------------------------------------------------------------------------
   subtype Stack_Type is Stack.Stack_Type;

   Stack_Empty : exception renames Stack.Stack_Empty;

   procedure Push (On      : in out Stack_Type;
                   Element : in     Element_Type)
     renames Stack.Push;
   procedure Pop  (From    : in out Stack_Type;
                   Element :    out Element_Type)
     renames Stack.Pop;
   function  Pop  (From    : access Stack_Type)
                  return Element_Type
     renames Stack.Pop;
   function  Top  (From    : access Stack_Type)
                  return Element_Type
     renames Stack.Top;


end Test_Stack;
