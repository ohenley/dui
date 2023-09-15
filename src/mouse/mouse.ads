with Subjects_Observers;  use Subjects_Observers;

package Mouse is

    type Position_T is record
        X : Natural;
        Y : Natural;
    end record;

    type Click_T is record
        Left : Boolean;
        Middle : Boolean;
        Right : Boolean;
    end record;
    
    function Get_Mouse_Position return Position_T;
    function Get_Mouse_Clicks return Click_T;

    type Click_Callback_t is access procedure (Click : Click_T);
    procedure Register_To_Click (cb : Click_Callback_t);

    task T;

end Mouse;