with Ada.Strings.Unbounded;

with graphic;

--with Mouse_Observers;
--with Mouse_Subject;

package Widget.Button is

   subtype Parent is Widget.Instance;

   type state_enum is (idle, hover, clicking);
   type state_colors is array (state_enum) of graphic.color;

   type Instance is new Parent with record
      state  : state_enum := idle;
      colors : state_colors := (idle     => graphic.grey,
                                hover    => graphic.light_grey,
                                clicking => graphic.white);
      button_text  : Any_Acc;
      --Mouse : Mouse_Observers.Observer (Mouse_Subject.Get_Mouse_Access);
   end record;
   
   subtype Class is Instance'Class;

   type Acc is access all Instance;
   type Any_Acc is access all Class;

   function Create (id            : string;
                    parent        : Widget.Any_Acc;
                    text          : string;
                    self_flex     : flex_t  := default_flex;
                    child_flex    : flex_t  := default_flex;
                    bgd           : graphic.color) return Widget.Any_Acc;

   overriding
   procedure Event (This : in out Instance; Evt : Event_Kind);

   overriding
   procedure Draw (This : in out Instance; img : in out graphic.image);

private
   
   subtype Dispatch is Instance'Class;
   
end Widget.Button;
