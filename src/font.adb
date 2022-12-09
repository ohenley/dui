with Ada.Characters.Handling;

package body font is
    function get_font_char_start (c : Character) return bitmap_width_t is
        u_c : character := Ada.Characters.Handling.to_upper (c);
    begin
        case u_c is
            when 'A' => return 1;
            when 'B' => return 6;
            when 'C' => return 11;
            when 'D' => return 16;
            when 'E' => return 21;
            when 'F' => return 26;
            when 'G' => return 31;
            when 'H' => return 36;
            when 'I' => return 41;
            when 'J' => return 46;
            when 'K' => return 51;
            when 'L' => return 56;
            when 'M' => return 61;
            when 'N' => return 66;
            when 'O' => return 71;
            when 'P' => return 76;
            when 'Q' => return 81;
            when 'R' => return 86;
            when 'S' => return 91;
            when 'T' => return 96;
            when 'U' => return 101;
            when 'V' => return 106;
            when 'W' => return 111;
            when 'X' => return 116;
            when 'Y' => return 121;
            when 'Z' => return 126;
            when '_' => return 131;
            when ' ' => return 136;
            when '0' => return 141;
            when '1' => return 146;
            when '2' => return 151;
            when '3' => return 156;
            when '4' => return 161;
            when '5' => return 166;
            when '6' => return 171;
            when '7' => return 176;
            when '8' => return 181;
            when '9' => return 186;
            when '%' => return 191;
            when others => return 1;
        end case;
    end;
begin
    null;
end font;