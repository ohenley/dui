with Ada.Text_IO; use Ada.Text_IO;
with dui; use dui;

with graphic; use graphic;
with importer;
with x11_window;
with Ada.Strings.Unbounded;

with Ada.Finalization; use Ada.Finalization;

with font;

with widget; use widget;
with widget.button;
with widget.text;
with widget.image;


procedure Main is

    package w renames widget;
    package wt renames widget.text;
    package wi renames widget.image;
    package wb renames widget.button;
    package g  renames graphic;
    

    color_counter : float := 1.0;
    mult : g.color_val := 0.01;

    function get_val (counter : float) return g.color_val is
    begin
        return 0.3 + g.color_val(counter * float(mult));
    end;

    function get_color return g.color is
        c : g.color;
    begin
        color_counter := color_counter + 1.0;
        c := (get_val (color_counter), get_val (color_counter), get_val (color_counter), 0.0);
        return c;
    end;

    header  : w.Any_Acc := wt.Create (id         => "header",
                                      parent     => dui.main_widget,
                                      text       => "Filtering Image",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 50),
                                                     others   => <>),
                                      child_flex => (dir    => left_right,
                                                     others => <>),
                                      bgd        => get_color);

   
    images : w.Any_Acc := w.Create(id         => "images",
                                   parent     => dui.main_widget,
                                   self_flex  => (expand_w => (behavior => max),
                                                  expand_h => (behavior => max),
                                                  others   => <>),
                                   child_flex => (dir      => left_right,
                                                  others   => <>),
                                   bgd        => get_color);
           
    process : w.Any_Acc := wt.Create (id         => "process",
                                      parent     => dui.main_widget,
                                      text       => "0%",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 50),
                                                     others => <>),
                                      child_flex => (dir      => left_right,
                                                     others   => <>),
                                      bgd        => get_color);
                                      

    orig_img : w.Any_Acc := wi.Create (id           => "orig_img",
                                       parent       => images,
                                       abs_filename => "data/Ada_Lovelace_photo.qoi",
                                       self_flex    => (--expand_w => (pixel, 654),
                                                        expand_h => (behavior => max),
                                                        others   => <>),
                                       bgd          => get_color);

    filt_img : w.Any_Acc := wt.Create (id            => "filt_img",
                                       parent        => images,
                                       text          => "Filtered",
                                       magnification => 1,
                                       self_flex     => (expand_h => (behavior => max),
                                                         others   => <>),
                                       bgd           => get_color);

    img_sep  : w.Any_Acc := w.Create (id         => "img_sep",
                                      parent     => images,
                                      self_flex  => (expand_w => (pixel, 10),
                                                     expand_h => (behavior => max),
                                                     others   => <>),
                                      bgd        => get_color);

    tools   : w.Any_Acc := wt.Create (id         => "tools",
                                      parent     => images,
                                      text       => "Tools",
                                      self_flex  => (expand_w => (pixel, 200),
                                                     expand_h => (behavior => max),
                                                     others   => <>),
                                      bgd        => get_color);

    compute : w.Any_Acc := wb.Create (id         => "compute_button",
                                      parent     => tools,
                                      text       => "Compute",
                                      self_flex  => (expand_w => (behavior => max),
                                                     expand_h => (pixel, 100),
                                                     others   => <>),
                                      bgd        => get_color);
    
    render_ui : x11_window.Update_Image_Buffer := dui.render'Access;
    
begin
    x11_window.open_window (1920, 1080, 60, render_ui);
end;