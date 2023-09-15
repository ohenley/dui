--with Subjects_Observers; use Subjects_Observers;

with Ada.Containers.Vectors;

with Ada.Text_IO; use Ada.Text_IO;
with System;

with X.Xlib;

package body Mouse is

    type Mouse_t is record
        position : Position_T;
        click    : Click_T;
    end record;

    package Registered_Click_Callbacks is new
     Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Click_Callback_t);

    Click_Callbacks : Registered_Click_Callbacks.Vector;

    procedure Register_To_Click (cb : Click_Callback_t) is
    begin
        Click_Callbacks.Append (cb);
    end;

    procedure Unregister_To_Click (cb : Click_Callback_t) is
    begin
        Click_Callbacks.Remove (cb);
    end;


    protected Mouse_Proxy is
        procedure Set_Position (X : Natural; Y : Natural);
        function Get_Position return Position_T;
        procedure Set_Click (Left : Boolean; Middle : Boolean; Right : Boolean);
        function Get_Clicks return Click_T;
    private
        Mouse : Mouse_T;
    end Mouse_Proxy;

    protected body Mouse_Proxy is
        procedure Set_Position (X : Natural; Y : Natural) is
        begin
            Mouse.Position.X := X;
            Mouse.Position.Y := Y;
        end Set_Position;

        function Get_Position return Position_T is (Mouse.Position);

        procedure Set_Click (Left : Boolean; Middle : Boolean; Right : Boolean)
        is
        begin
            if Mouse.Click.Left /= Left or Mouse.Click.Middle /= Middle or
               Mouse.Click.Right /= Right
            then
                null;
            end if;

            Mouse.Click.Left   := Left;
            Mouse.Click.Middle := Middle;
            Mouse.Click.Right  := Right;
        end Set_Click;

        function Get_Clicks return Click_T is (Mouse.Click);
    end Mouse_Proxy;


    function Get_Mouse_Position return Position_T is
       (Mouse_Proxy.Get_Position);

    function Get_Mouse_Clicks return Click_T is 
        (Mouse_Proxy.Get_Clicks);

    
    task body T is
        function Init_Mouse return Integer;
        pragma Import (C, Init_Mouse, "init_mouse");

        procedure Get_Mouse_Data
           (fd : Integer; 
            Display : X.Xlib.XDisplay_access; 
            x : in out Natural;
            y : in out Natural; 
            left : in out Boolean; 
            middle : in out Boolean;
            right : in out Boolean);
        pragma Import (C, Get_Mouse_Data, "get_mouse_data");
        Display : X.Xlib.XDisplay_access := X.Xlib.XOpenDisplay (null);
        fd                  : Integer                := Init_Mouse;
        X, Y                : Integer                := 0;
        Left, Middle, Right : Boolean                := False;
    begin
        while True loop
            delay (1.0);
            Get_Mouse_Data (fd, Display, X, Y, Left, Middle, Right);
            Mouse_Proxy.Set_Position (X, Y);
        end loop;
    end T;
end Mouse;
