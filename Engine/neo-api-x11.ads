with Interfaces.C.Strings;
with Interfaces;

with system;

package x11 is
    package ics renames Interfaces.C.Strings;
    package ic  renames interfaces.C;

    ExposureMask : constant ic.unsigned_long := 15;
    KeyPressMask : constant ic.unsigned_long := 1;
    ButtonPressMask	: constant ic.unsigned_long := 2;
    StructureNotifyMask	: constant ic.unsigned_long := 17;
    ResizeRedirectMask : constant ic.unsigned_long := 18;


    ButtonPress	: constant ic.unsigned_long := 4;
-- #define ButtonRelease		5
-- #define MotionNotify		6
-- #define EnterNotify		7
-- #define LeaveNotify		8
-- #define FocusIn			9
-- #define FocusOut		10
-- #define KeymapNotify		11
-- #define Expose			12

    type bool is new ic.int;
    type char_access is access all ic.char;


    --type char_access is new system.address;
    type xpointer is new char_access;

    type xevent is array (1..24) of ic.long;
    type xevent_access is access all xevent;

    type display_access is new system.address; 
    type window is new ic.unsigned_long; 

    type visual_access is new system.address;
    type screen_access is new system.address;
    type xgcvalues_access is new system.address;

    type colormap is new ic.unsigned_long;
    type drawable is new ic.unsigned_long;

    type graphic_context is new system.address;

    type xwindow_attributes is record
        x : ic.int;
        y : ic.int;
        width : ic.int;
        height : ic.int;
        border_width : ic.int;
        depth : ic.int;
        visual : visual_access;
        root : window;
        class : ic.int;
        bit_gravity : ic.int;
        win_gravity : ic.int;
        backing_store : ic.int;
        backing_planes : ic.unsigned_long;
        backing_pixel : ic.unsigned_long;
        save_under : bool;
        color_map : colormap;
        map_installed : bool;
        map_state : ic.int;
        all_event_masks : ic.long;
        your_event_mask : ic.long;
        do_not_propagate_mask : ic.long;
        override_redirect : bool;
        screen : screen_access;
    end record;
    type xwindow_attributes_access is access all xwindow_attributes;

    type ximage_access;

    type af_create_image is access function  return Ximage_access;
    type af_destroy_image is access function (Image: Ximage_access) return ic.int;       
    type af_get_pixel is access function (Image : Ximage_access; X : ic.int; Y : ic.int) return ic.unsigned_long;
    type af_put_pixel is access function (Image : Ximage_access; X : ic.int; Y : ic.int; Pixel: ic.unsigned_long) return ic.int;
    type af_sub_image is access function (Image : Ximage_access; X : ic.int; Y : ic.int; width : ic.unsigned; height : ic.unsigned) return Ximage_access; 
    type af_add_pixel is access function (Image : Ximage_access; value : ic.long) return ic.int;

    type funcs is record
        create_image : af_create_image;                 -- Xlib.h:349
        destroy_image: af_destroy_image;            -- Xlib.h:351
        get_pixel    : af_get_pixel;                -- Xlib.h:352
        put_pixel    : af_put_pixel;                -- Xlib.h:353
        sub_image    : af_sub_image;                -- Xlib.h:354
        add_pixel    : af_add_pixel;                -- Xlib.h:355
    end record;

    type ximage is record
        width            : ic.int;
        height           : ic.int;
        xoffset          : ic.int; -- number of pixels offset in X direction
        format           : ic.int;
        data             : char_access;
        byte_order       : ic.int;
        bitmap_unit      : ic.int;
        bitmap_bit_order : ic.int;
	    bitmap_pad       : ic.int;			
	    depth            : ic.int;
	    bytes_per_line   : ic.int;		
	    bits_per_pixel   : ic.int;		   
	    red_mask         : ic.unsigned_long;
        green_mask       : ic.unsigned_long;
        blue_mask        : ic.unsigned_long; 
	    obdata           : xpointer;
        f                : funcs;
    end record;
    type ximage_access is access all ximage;

    function x_default_screen (display : display_access) return ic.int with
        Import        => True,
        Convention    => c,
        External_Name => "XDefaultScreen";

    function x_default_root_window (display : display_access) return window with
        Import        => True,
        Convention    => c,
        External_Name => "XDefaultRootWindow";

    function x_get_window_attributes (display : display_access; w : window; window_attributes_return : xwindow_attributes_access) return ic.int with
        Import        => True,
        Convention    => c,
        External_Name => "XGetWindowAttributes";

    function x_sync (display : display_access; discard : bool) return ic.int with
        Import        => True,
        Convention    => c,
        External_Name => "XSync";

    function x_open_display (display_name : ics.chars_ptr) return display_access with
        Import        => True,
        Convention    => c,
        External_Name => "XOpenDisplay";

    function x_create_simple_window (display      : display_access;
                                     parent       : window;
                                     x            : ic.int;
                                     y            : ic.int;
                                     width        : ic.unsigned;
                                     height       : ic.unsigned;
                                     border_width : ic.unsigned;
                                     border       : ic.unsigned_long;
                                     background   : ic.unsigned_long) return window with
        Import        => True, 
        Convention    => c,
        External_Name => "XCreateSimpleWindow";

    function x_create_graphic_context (display : display_access; w : window; value_mask : ic.unsigned_long; values : xgcvalues_access) return graphic_context with
        Import        => True,
        Convention    => c,
        External_Name => "XCreateGC";

    procedure x_select_input (display : display_access; w : window; event_mask : ic.long) with
        Import        => True,
        Convention    => c,
        External_Name => "XSelectInput";

    procedure x_map_window (display : display_access; w : window) with
        Import        => True,
        Convention    => c,
        External_Name => "XMapWindow";

    procedure x_next_event (display : display_access;  event_return : xevent_access) with
        Import        => True,
        Convention    => c,
        External_Name => "XNextEvent";

    procedure x_send_event (display : display_access; w : window; propagate : bool; event_mask : ic.long; event_send : xevent_access) with
        Import        => True,
        Convention    => c,
        External_Name => "XSendEvent";

    function x_pending (display : display_access) return ic.int with
        Import        => True,
        Convention    => c,
        External_Name => "XPending";

    function alloc_xshm_image (display      : display_access;
                               visual       : visual_access;
                               img          : ximage_access;
                               width        : ic.int;
                               height       : ic.int;
                               depth        : ic.int) return ximage_access;

    function x_shm_put_image (display    : display_access;
                              d          : drawable;
                              gc         : graphic_context;
                              image      : ximage_access;
                              src_x      : ic.int;
                              src_y      : ic.int;
                              dst_x      : ic.int;
                              dst_y      : ic.int;
                              src_w      : ic.unsigned;
                              src_h      : ic.unsigned;
                              send_event : bool) return bool with
        Import        => True,
        Convention    => c,
        External_Name => "XShmPutImage";

    procedure wrap_x_shm_put_image (display    : display_access;
                                    d          : drawable;
                                    gc         : graphic_context;
                                    image      : ximage_access;
                                    src_w      : ic.unsigned;
                                    src_h      : ic.unsigned) with
        Import        => True,
        Convention    => c,
        External_Name => "wrap_x_shm_put_image";

    function wrap_alloc_xshm_image(display      : display_access;
                                   visual       : visual_access;
                                   width        : ic.int;
                                   height       : ic.int;
                                   depth        : ic.int) return ximage_access with
        Import        => True,
        Convention    => c,
        External_Name => "wrap_alloc_xshm_image";

    type x_shm_segment_info is record
            shmseg    : ic.unsigned_long;
            shmid     : ic.int;
            shmaddr   : char_access;
            read_only : bool;
        end record;
    type x_shm_segment_info_access is access all x_shm_segment_info;

    shminfo : aliased x_shm_segment_info;

    function x_shm_create_image (display      : display_access;
                                 visual       : visual_access;
                                 depth        : ic.unsigned;
                                 format       : ic.int;
                                 data         : char_access;
                                 shm_info     : x_shm_segment_info_access;
                                 width        : ic.unsigned;
                                 height       : ic.unsigned) return ximage_access with
        Import        => True, 
        Convention    => c,
        External_Name => "XShmCreateImage";

    
    

end x11;
