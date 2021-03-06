#-*- coding:utf-8 -*-
import pathlib, os
import CoreCode

if __name__ == "__main__":
    P_path = pathlib.Path(os.path.abspath(__file__)).resolve()
    Base_dir = P_path.parents[2]

    print("Choose video file")
    ini_dir = Base_dir / "Videofiles"

    in_f = CoreCode.def_select_file(Basedir=ini_dir,
                                    name="Traking",
                                    message="Choose object file")
    in_f_name = pathlib.Path(in_f).name
    print("Selected video file: " + in_f_name)
    print("******************************************")

    print("Choose mask polygon CSV file")
    ini_dir = Base_dir / "Setting" / "Polygon"

    setting_file = CoreCode.def_select_file(Basedir=ini_dir,
                                            name="Traking",
                                            message="Choose setting file")
    print("Selected mask polygon CSV file: " + setting_file)
    print("******************************************")

    print("Input V threshold Value")
    threshold = CoreCode.def_set_nvalue()
    if threshold == "empty":
        print("V threshold Value: default, 35")
        threshold = 35
    else:
        print("V threshold Value: " + str(threshold))
        threshold=int(threshold)
    print("******************************************")
    print("Analyze Start:" + in_f_name)

    resolution = CoreCode.def_get_Resolution(in_f)
    CoreCode.main(Basedir=Base_dir,
                  input=in_f,
                  filename=in_f_name,
                  setting=setting_file,
                  threshold=threshold,
                  resolution=resolution)
    print("Analyze Complete:" + in_f_name)