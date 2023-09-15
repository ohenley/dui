with Ada.Containers.Bounded_Multiway_Trees;
with Ada.Strings.Unbounded;

with graphic;

with widget; use widget;

package dui is

    pragma Elaborate_Body;

    package Layout_Object_Tree is new Ada.Containers.Bounded_Multiway_Trees (Any_Acc);    
    LOT : Layout_Object_Tree.tree (1000);
    LOT_Root : Layout_Object_Tree.cursor := LOT.Root;
    main_widget : Widget.Any_Acc;

    --img : graphic.image_access := new graphic.image(1 .. 1920, 1 .. 1080);

    procedure add_to_LOT (Widget : Any_Acc; Parent : Any_Acc);

    type Loadable is interface;
    procedure Load (L: Loadable) is abstract;

    procedure draw_image(target : in out graphic.image;
                         img : in out graphic.image_access;
                         x, y  : natural;
                         w, h  : natural);


    procedure draw_rect (target : in out graphic.image;
                         x, y  : natural;
                         w, h  : natural;
                         c     : graphic.color);

    procedure draw_text (target : in out graphic.image;
                         text  : string;
                         magnification : natural;
                         x, y  : natural;
                         color : graphic.color);

    procedure render (target : in out graphic.image;
                      window_width  : natural;
                      window_height : natural);

end;