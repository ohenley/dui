--with Ada.Text_IO; use Ada.Text_IO;
with dui;

with namespaces; use namespaces;

package body Widget.Button is

    function Create (id            : string;
                     parent        : Widget.Any_Acc;
                     text          : string;
                     self_flex     : flex_t  := default_flex;
                     child_flex    : flex_t  := default_flex;
                     bgd           : graphic.color) return Widget.Any_Acc is
        this : Widget.Any_Acc;
    begin
        this := new Instance' (af.Controlled with
                              id            => +id,
                              self_flex     => self_flex,
                              child_flex    => child_flex,
                              bgd           => graphic.red_1,
                              others        => <>);
        dui.add_to_LOT (This, Parent);

        Any_Acc(this).button_text := wt.Create (id         => id & ".text",
                                                parent     => this,
                                                text       => text,
                                                self_flex  => (expand_w => (behavior => max),
                                                               expand_h => (behavior => max),
                                                               others   => <>),
                                                child_flex => (dir    => left_right,
                                                               others => <>),
                                                bgd        => graphic.red_1);
        
        return This;
    end;

    overriding 
    procedure Event (This : in out Instance; Evt : Event_Kind) is
    begin
        null;
    end Event;

    overriding 
    procedure Draw (This : in out Instance; img : in out g.image) is
    begin
        dui.draw_rect (img, this.x+1, this.y+1, this.w, this.h, this.colors (this.state));
    end Draw;

end Widget.Button;
