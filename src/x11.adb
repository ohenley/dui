with Ada.Text_IO; use Ada.Text_IO;

--with System.Address_To_Access_Conversions;

with Ada.Unchecked_Conversion;

package body x11 is

    

    function alloc_xshm_image (display      : display_access;
                               visual       : visual_access;
                               img          : ximage_access;
                               width        : ic.int;
                               height       : ic.int;
                               depth        : ic.int) return ximage_access is

        

        
            
        
        function x_shm_attach (display      : display_access;
                               shm_info     : x_shm_segment_info_access) return bool with
            Import        => True, 
            Convention    => c,
            External_Name => "XShmAttach";

        function shm_get (shm_id : ic.int; cmd : ic.unsigned_long; shm_flg : ic.int) return ic.int with
            Import        => True, 
            Convention    => c,
            External_Name => "shmget";

        function shm_attach (shm_id : ic.int; shm_addr : ic.int; shm_flg : ic.int) return char_access with
            Import        => True, 
            Convention    => c,
            External_Name => "shmat";

        
        type shm_id_ds_access is new system.Address;
        function shm_ctl (shm_id : ic.int; cmd : ic.int; buf : shm_id_ds_access) return ic.int with
            Import        => True, 
            Convention    => c,
            External_Name => "shmctl";

        shminfo : aliased x_shm_segment_info;
        -- img : ximage_access := x_shm_create_image (display, 
        --                                            visual, 
        --                                            depth, 
        --                                            2, 
        --                                            null, 
        --                                            shminfo'Unchecked_Access, 
        --                                            ic.unsigned (width), 
        --                                            ic.unsigned (height));
        res_attach : bool;
        res_sync  : ic.int;
        res_ctl   : ic.int;
        use ic;

    begin
        shminfo.shmid     := shm_get (0, ic.unsigned_long (img.bytes_per_line * img.height), 1023);
        shminfo.shmaddr   := shm_attach (shminfo.shmid, 0, 0);

        img.data          := shminfo.shmaddr;

        shminfo.read_only := 0;
        res_attach        := x_shm_attach (display, shminfo'Unchecked_Access);
        res_sync          := x_sync (display, 0);
        res_ctl           := shm_ctl (shminfo.shmid, 0, shm_id_ds_access (system.null_address));

        --Put_Line (test'image);
        --Put_Line (addr'image);


        Put_Line (shminfo'image);

        return img;
    end;
end x11;