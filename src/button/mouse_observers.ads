with Mouse; use Mouse;
with Subjects_Observers; use Subjects_Observers;

package Mouse_Observers is

  type Observer (Mouse : access Mouse_T'Class) is
   new Root_Observer_Type with private;

  procedure On_Notify (O : access Observer);

private

  type Observer (Mouse: access Mouse_T'Class) is
  new Root_Observer_Type (Mouse) with null record;

end Mouse_Observers;
