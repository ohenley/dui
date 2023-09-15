with Ada.Text_IO; use Ada.Text_IO;

package body Mouse_Observers is

   procedure On_Notify (o : access Observer) is
   begin
      Put_Line ("Happy");
   end;

end Mouse_Observers;
