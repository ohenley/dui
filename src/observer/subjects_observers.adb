package body Subjects_Observers is

   procedure Notify (Subject : in out Root_Subject_Type) is
      Observer : Observer_Access := Subject.Observer_List;
   begin
      while Observer /= null loop
         Update (Observer);
         Observer := Observer.Next;
      end loop;
   end Notify;

   procedure Register
     (Subject  : access Root_Subject_Type;
      Observer : access Root_Observer_Type'Class) is
   begin
      Observer.Next := Subject.Observer_List;
      Subject.Observer_List := Observer_Access (Observer);
   end;

   procedure Unregister
     (Subject  : access Root_Subject_Type;
      Observer : access Root_Observer_Type'Class) is

      OA : constant Observer_Access := Observer_Access (Observer);

      Prev  : Observer_Access;
      Index : Observer_Access;
   begin
      if Subject.Observer_List = OA then
         Subject.Observer_List := Subject.Observer_List.Next;
      else
         Prev := Subject.Observer_List;
         Index := Prev.Next;

         while Index /= OA loop
            Prev := Index;
            Index := Index.Next;
         end loop;

         Prev.Next := Index.Next;
      end if;
   end Unregister;


   procedure Initialize (Observer : in out Root_Observer_Type) is
   begin
      Register (Observer.Subject, Observer'Access);
   end;

   procedure Finalize (Observer : in out Root_Observer_Type) is
   begin
      Unregister (Observer.Subject, Observer'Access);
   end;


end Subjects_Observers;