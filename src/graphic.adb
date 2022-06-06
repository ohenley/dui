with qoi;
with namespaces; use namespaces;

with system;

with GNAT.OS_Lib;

package body graphic is

    function Load_QOI (abs_filename : String) return image is

        type Storage_Array_Access is access all sse.Storage_Array;
        type Input_Data is record
            Data : Storage_Array_Access;
            Desc : QOI.QOI_Desc;
        end record;

        use GNAT.OS_Lib;
        use sse;

        FD : File_Descriptor;
        Ret : Integer;

        Result : Input_Data;

        function to_image return image is
            w : integer := integer (result.Desc.Width);
            h : integer := integer (result.Desc.Height);
            img : image (1 .. w, 1 .. h);
            idx : sse.storage_count;
            offset : natural;
            test : float;
        begin
            for j in img'range(2) loop
                offset := (j-1)*w;
                for i in img'range(1) loop
                    idx := sse.Storage_Count (((offset+(i-1))*3)+1);

                    img (i, j) := (color_val(float(Result.Data(idx + 2))/ 256.024096386), 
                                   color_val(float(Result.Data(idx + 1))/ 256.024096386), 
                                   color_val(float(Result.Data(idx + 0))/ 256.024096386), 
                                   0.0);
                end loop;
            end loop;
            return img;
        end;

    begin

        aio.Put_Line ("IO, LOAD_QOI : " & abs_filename);

        FD := GNAT.OS_Lib.Open_Read (abs_filename, Binary);

        if FD = Invalid_FD then
            aio.Put_Line (aio.Standard_Error, GNAT.OS_Lib.Errno_Message);
            GNAT.OS_Lib.OS_Exit (1);
        end if;

        declare
            Len : constant sse.Storage_Count := sse.Storage_Count (File_Length (FD));
            In_Data : constant Storage_Array_Access := new sse.Storage_Array (1 .. Len);
        begin
            Ret := Read (FD, In_Data.all'Address, In_Data.all'Length);

            if Ret /= In_Data'Length then
                aio.Put_Line (GNAT.OS_Lib.Errno_Message);
                GNAT.OS_Lib.OS_Exit (1);
            end if;

            Close (FD);

            QOI.Get_Desc (In_Data.all, Result.Desc);

            declare
                use sse;
                Out_Len : constant sse.Storage_Count :=
                Result.Desc.Width * Result.Desc.Height * Result.Desc.Channels;
                Out_Data : constant Storage_Array_Access :=
                new sse.Storage_Array (1 .. Out_Len);
                Output_Size : sse.Storage_Count;
            begin
                QOI.Decode (Data        => In_Data.all,
                            Desc        => Result.Desc,
                            Output      => Out_Data.all,
                            Output_Size => Output_Size);

                Result.Data := Out_Data;

                return to_image;
            end;
        end;
    end Load_QOI;
    
end graphic;