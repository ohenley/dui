with graphic;
with x11;

package x11_window is

    type Update_Image_Buffer is access procedure (image         : in out graphic.image;
                                                  window_width  : natural;
                                                  window_height : natural);
    procedure open_window (window_width  : natural;
                           window_height : natural;
                           fps           : Integer;
                           render_ui     : Update_Image_Buffer);
end x11_window; 