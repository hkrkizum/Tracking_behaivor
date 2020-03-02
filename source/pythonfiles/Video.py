#-*- coding:utf-8 -*-
import pathlib
import CoreCode
import pandas as pd

if __name__ == "__main__":
    P_path = pathlib.Path("Video_1file.py").resolve()
    Base_dir = P_path.parents[0]

    print("Choose list file")
    ini_dir = Base_dir / "Setting" / "FileList"

    setting_file_list = CoreCode.def_select_file(Basedir=ini_dir,
                                                 name="Traking",
                                                 message="Choose setting file")
    in_f_name = pathlib.Path(setting_file_list).name
    print("Selected file" + in_f_name)
    print("******************************************")

    df_filelist = pd.read_csv(setting_file_list)
    print(df_filelist)

    print("******************************************")

    for i in range(len(df_filelist)):
        in_f = df_filelist.iloc[i,0]
        in_f_name = pathlib.Path(in_f).name
        setting_file = df_filelist.iloc[i,1]
        threshold = df_filelist.iloc[i, 2]
        # out_n = df_filelist.iloc[i, 2]
        #threshold = df_filelist.iloc[i, 3]
        resolution = CoreCode.def_get_Resolution(in_f)

        print("******************************************")
        print("Analyze:" + in_f_name)
        CoreCode.main(Basedir=Base_dir,
                      input=in_f,
                      filename=in_f_name,
                      setting=setting_file,
                      threshold=threshold,
                      resolution=resolution)
        print("Analyze Complete:" + in_f_name)

    print("Analyze Complete")