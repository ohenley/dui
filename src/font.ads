with graphic; 
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Characters;

package font is

    pragma Elaborate_Body;

    package g renames graphic;

    bitmap_base : Integer := 5; 

    subtype bitmap_width_t is Integer range 1 .. 39*bitmap_base;
    subtype bitmap_height_t is Integer range 1 .. bitmap_base;
    
    font_1_img : g.image (bitmap_width_t, bitmap_height_t);

    function get_font_char_start (c : Character) return bitmap_width_t;

end font;