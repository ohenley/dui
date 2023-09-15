with ada.text_io; use ada.text_io;

with Ada.Real_Time; use Ada.Real_Time;


with Ada.Finalization; use Ada.Finalization;

with importer;
with font;

with widget;

with namespaces; use namespaces;

package body dui is

    procedure add_to_lOt (Widget : Any_Acc; Parent : Any_Acc) is
    begin
        dui.LOT.Append_Child (dui.Layout_Object_Tree.Find (dui.LOT, Parent), Widget);
    end;

    procedure draw_image(target : in out g.image;
                         img : in out g.image_access;
                         x, y  : natural;
                         w, h  : natural) is
        it, jt : integer;
    begin
        for j in img'range(2) loop
            for i in img'range(1) loop
                it := x + i;
                jt := y + j;
                if it < x + w and jt < y + h then
                    target (it, jt) := img (I,J);
                end if;
            end loop;
        end loop;
    exception
        when others =>
            Put_Line ("ij: " & it'image & " jt: " & jt'image);
    end;

    procedure draw_rect (target : in out g.image;
                         x, y  : natural;
                         w, h  : natural;
                         c     : g.color) is
      Xb : constant Integer := x;
      Xe : constant Integer := x + w - 1;
      Yb : constant Integer := y;
      Ye : constant Integer := y + h - 1;
      ic, jc : integer;
    begin
        for I in Xb .. Xe loop
            for J in Yb .. Ye loop
                ic := i;
                jc := j;
                target (I, J) := c;
            end loop;
        end loop;
    exception
        when others =>
            Put_Line ("i: " & ic'image & " J: " & jc'image);
    end;

    procedure draw_character (c             : character;
                              magnification : natural;
                              target        : in out g.image;
                              x, y          : natural;
                              color         : g.color) is
        x_font : constant integer := font.get_font_char_start (c);
        y_font : constant integer := 1;
        use g;
    begin
        for fj in font.bitmap_height_t'first .. font.bitmap_base loop
            for fi in font.bitmap_width_t'first .. font.bitmap_base loop
                --Put_Line (font.font_1_img (x_font + fi - 1, y_font + fj - 1)'image);
                --Put_Line (g.color_val'last'image);
                if font.font_1_img (x_font + fi - 1, y_font + fj - 1) = g.white then
                    for mj in 1 .. magnification loop
                        for mi in 1 .. magnification loop
                            target (x+(fi*magnification)+mi-1, y+(fj*magnification)+mj-1) := color;
                        end loop;
                    end loop;
                end if;
            end loop;
        end loop;
    end;

    procedure draw_text (target        : in out g.image;
                         text          : string;
                         magnification : natural;
                         x, y          : natural;
                         color         : g.color) is
        use g;
    begin
        for c in text'range loop
            declare
                i : natural := x + (c-1) * (magnification * (font.bitmap_base + 1));
                j : natural := y;
            begin
               draw_character (text (c), magnification, target, i, j, color);
            end;
        end loop;
    exception
        when others =>
            put_line("draw text problem!");
    end;

    -- need a pass from leaf to root to compute intrinsic, inner content width and height

    procedure render (target        : in out graphic.image;
                      window_width  : natural;
                      window_height : natural) is


        procedure render_node (c : Layout_Object_Tree.cursor) is
        begin
            Layout_Object_Tree.Element (c).Draw (target);
        end;

        procedure test (c : Layout_Object_Tree.cursor) is
        begin
            if Layout_Object_Tree.Element (c).all in Loadable'Class then
                Loadable'Class(Layout_Object_Tree.Element (c).all).Load;
            end if;
        end;

        procedure compute_node (c : Layout_Object_Tree.cursor) is
            cc : natural := natural (Layout_Object_Tree.Child_Count (c));
            e : w.Class := Layout_Object_Tree.Element (c).all;
            cw : natural := e.w;
            ch : natural := e.h;
            cx : natural := e.x;
            cy : natural := e.y;
            counter : natural := 0;
            child_row : boolean := (e.child_flex.dir = left_right or e.child_flex.dir = right_left);
            child_column : boolean := (e.child_flex.dir = top_bottom or e.child_flex.dir = bottom_top);
            child_depth : boolean := (e.child_flex.dir = front_back or e.child_flex.dir = back_front);
        begin
            if cc > 0 then
                declare
                    expand_w, expand_h : expand_t;
                    width_pixel_left : natural := e.w;
                    height_pixel_left : natural := e.h;
                    total_portion : natural := 0;
                    nbr_max       : natural := 0;
                begin
                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop
                        if child_row then
                            expand_w := LOT(i).self_flex.expand_w;
                            case expand_w.behavior is
                                when portion =>
                                    total_portion    := total_portion + expand_w.portion;
                                when pixel =>
                                    width_pixel_left := width_pixel_left - expand_w.pixel;
                                when percent => 
                                    width_pixel_left := width_pixel_left - natural (percent_t(e.w) * expand_w.percent);
                                when content =>
                                    width_pixel_left := width_pixel_left - LOT(i).w;
                                when max =>
                                    nbr_max := nbr_max + 1;
                            end case;
                        elsif child_column then
                            expand_h := LOT(i).self_flex.expand_h;
                            case expand_h.behavior is
                                when portion =>
                                    total_portion    := total_portion + expand_h.portion;
                                when pixel =>
                                    height_pixel_left := height_pixel_left - expand_h.pixel;
                                when percent => 
                                    height_pixel_left := height_pixel_left - natural (percent_t(e.h) * expand_h.percent);
                                when content =>
                                    height_pixel_left := height_pixel_left - LOT(i).h;
                                when max =>
                                    nbr_max := nbr_max + 1;
                            end case;
                        elsif child_depth then
                            null;
                        end if;
                    end loop;

                    for i in Layout_Object_Tree.Iterate_Children (LOT, C) loop 
                        LOT(i).x := cx;
                        LOT(i).y := cy;
                        expand_w := LOT(i).self_flex.expand_w;
                        expand_h := LOT(i).self_flex.expand_h;
                        if child_row then
                            case expand_w.behavior is -- update w in row context
                                when portion =>
                                    LOT(i).w := (e.w / total_portion) * expand_w.portion;
                                when pixel =>
                                    LOT(i).w := expand_w.pixel;
                                when percent => 
                                    LOT(i).w := natural(percent_t(e.w) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT(i).w := (width_pixel_left / nbr_max);
                            end case;
                            case expand_h.behavior is  -- update h in row context
                                when portion =>
                                    null;
                                when pixel =>
                                    LOT(i).h := expand_h.pixel;
                                when percent => 
                                    LOT(i).h := natural(percent_t(e.h) * expand_h.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT(i).h := e.h;
                            end case;
                            cx := cx + LOT(i).w;
                        elsif child_column then
                            case expand_h.behavior is -- update h in column context
                                when portion =>
                                    LOT(i).h := (e.h / total_portion) * expand_h.portion;
                                when pixel =>
                                    LOT(i).h := expand_h.pixel;
                                when percent => 
                                    LOT(i).h := natural(percent_t(e.h) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT(i).h := (height_pixel_left / nbr_max);
                            end case;
                            case expand_w.behavior is -- update w in column context
                                when portion =>
                                    null;
                                when pixel =>
                                    LOT(i).w := expand_w.pixel;
                                when percent => 
                                    LOT(i).w := natural(percent_t(e.w) * expand_w.percent);
                                when content =>
                                    null;
                                when max =>
                                    LOT(i).w := e.w;
                            end case;
                            cy := cy + LOT(i).h;
                        elsif child_depth then
                            null;
                        end if;
                    counter := counter + 1;
                    end loop;
                end;
            end if;
        end;

        Start_Time            : Time;
        Elapsed_Time          : Time_Span;

    begin
        Start_Time := Clock;
        LOT(Layout_Object_Tree.First_Child(LOT.Root)).w := window_width;
        LOT(Layout_Object_Tree.First_Child(LOT.Root)).h := window_height;
        Layout_Object_Tree.Iterate (LOT, compute_node'access);
        Layout_Object_Tree.Iterate (LOT, render_node'access);
        --Layout_Object_Tree.Iterate (LOT, test'access);
        Elapsed_Time := Clock - Start_Time;
        -- Put_Line ("Elapsed time (whole dui): "
        --    & Duration'Image (To_Duration (Elapsed_Time))
        --    & " seconds");
    end;
begin
    main_widget := new w.Instance' (Controlled with
                                    id          => +"main",
                                    child_flex  => (dir        => top_bottom,
                                                    others     => <>),
                                    others => <>);
    LOT.Append_Child (Parent => LOT_Root, New_Item => main_widget);
    LOT_Root := Layout_Object_Tree.First_Child (LOT_Root);
    font.font_1_img := g.Load_QOI ("data/font_1.qoi");
end dui;