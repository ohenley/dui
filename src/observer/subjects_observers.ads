with Ada.Finalization;

package Subjects_Observers is

   type Root_Subject_Type is
     abstract tagged private;

   procedure Notify (Subject : in out Root_Subject_Type);

   type Root_Observer_Type (Subject : access Root_Subject_Type'Class) is
     abstract tagged private;

   procedure Update (Observer : access Root_Observer_Type) is abstract;

private

   type Observer_Access is access all Root_Observer_Type'Class;
   pragma Suppress (Accessibility_Check, On => Observer_Access);
   pragma Suppress (Access_Check, On => Observer_Access);

   type Root_Subject_Type is
     abstract tagged record
        Observer_List : Observer_Access;
     end record;

   procedure Register
     (Subject  : access Root_Subject_Type;
      Observer : access Root_Observer_Type'Class);

   procedure Unregister
     (Subject  : access Root_Subject_Type;
      Observer : access Root_Observer_Type'Class);


   use Ada.Finalization;

   type Root_Observer_Type (Subject : access Root_Subject_Type'Class) is
     abstract new Controlled with record
        Next : Observer_Access;
     end record;


   procedure Initialize (Observer : in out Root_Observer_Type);

   procedure Finalize (Observer : in out Root_Observer_Type);


end Subjects_Observers;