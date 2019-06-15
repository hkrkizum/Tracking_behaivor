#-*- coding:utf-8 -*-
import cv2
import numpy as np
import pandas as pd
import datetime
import pathlib, tkinter, tkinter.filedialog, tkinter.messagebox, tkinter.simpledialog


# ガンマ補正
gamma = 1.2
lookUpTable = np.zeros((256, 1), dtype='uint8')
for i in range(256):
    lookUpTable[i][0] = 255 * pow(float(i) / 255, 1.0 / gamma)

def def_set_nvalue():
    root = tkinter.Tk()
    root.withdraw()
    answer = tkinter.simpledialog.askstring("Value", "Set V threshold")

    if answer is "":
        res = "empty"
    else:
        res = answer
    print(res)
    return res

def def_select_file(Basedir, name='Traking Analysis', message='Select Polygon XY csv!'):
    root = tkinter.Tk()
    root.withdraw()
    fTyp = [("", "*")]
    # iDir = os.path.abspath(os.path.dirname(__file__))
    tkinter.messagebox.showinfo(name, message)
    file = tkinter.filedialog.askopenfilename(filetypes=fTyp, initialdir=Basedir)

    # 処理ファイル名の出力
    return file

def def_create_maskimg(setting_file, resolution):
    setting_polyXY = pd.read_csv(setting_file)
    setting_polyXY['X'] = setting_polyXY['X'].astype(np.int64)
    setting_polyXY['Y'] = setting_polyXY['Y'].astype(np.int64)
    setting_polyXY = setting_polyXY.iloc[:, 1:].values

    # np.zeroで[0,0,0]を1920*1080の配列作成
    size = resolution[0], resolution[1], 3
    mask_img = np.zeros(size, dtype=np.uint8)
    # mask_img[np.where((mask_img == [0,0,0]).all(axis=2))] = [0,255,255]
    # cv2.fillPoly(mask_img, pts=[background_poly], color=(255, 255, 0))
    # 設定ファイルで読み込んだポリゴンを白で描画
    cv2.fillPoly(mask_img, pts=[setting_polyXY], color=(255, 255, 255))
    # グレースケール化
    mask_img = cv2.cvtColor(mask_img, cv2.COLOR_BGR2GRAY)

    return mask_img

def def_get_Resolution(input):
    cap = cv2.VideoCapture(input)
    ret, frame_raw = cap.read()
    height, width, channels = frame_raw.shape[:3]
    res = [height, width]

    return res

def main(Basedir, input, filename, setting, resolution, threshold=None):
    mask_img = def_create_maskimg(setting, resolution)
    if not threshold:
        threshold = 35

    # 動画の読み込み
    cap = cv2.VideoCapture(input)

    speed = 0
    center = []
    # 動画終了まで繰り返し
    while(cap.isOpened()):

        # フレームを取得
        ret, frame_raw = cap.read()
        # フレーム読み込み失敗で抜ける
        if not ret:
            break

        # ガンマ補正
        frame_gamma = cv2.LUT(frame_raw, lookUpTable)

        # Convert HSV
        frame = cv2.cvtColor(frame_raw, cv2.COLOR_BGR2HSV)
        # frame = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)
        h_img, s_img, v_img = cv2.split(frame)
        # cv2.imshow('HSV_frame', frame)

        # Threthold
        # ret, h_img_thread = cv2.threshold(h_img, 0, 255, cv2.THRESH_BINARY_INV)
        ret, v_img_thread = cv2.threshold(v_img, threshold, 255, cv2.THRESH_BINARY_INV)
        # ret, s_img_thread = cv2.threshold(s_img, 0, 255, cv2.THRESH_BINARY_INV)

        # img_thread = np.maximum(v_img_thread, s_img_thread, h_img_thread)
        img_thread = v_img_thread

        # marge mask
        img_thread = cv2.bitwise_and(img_thread, img_thread, mask=mask_img)

        # オープニング処理でくずを除去
        kernel = np.ones((5, 5), np.uint8)
        img_thread = cv2.morphologyEx(img_thread, cv2.MORPH_CLOSE, kernel)

        # クロージング処理で穴を除去
        kernel = np.ones((10, 10), np.uint8)
        img_thread = cv2.morphologyEx(img_thread, cv2.MORPH_OPEN, kernel)

        # 平均化する画素の周囲の大きさを指定する。
        # (5, 5) の場合、個々の画素の地点の周囲5×5マスの平均をとる。
        # 数値が大きいほどぼやける。
        average_square = (25, 25)
        # #
        # # x軸方向の標準偏差
        sigma_x = 1
        # #
        # # Gaussianオペレータを使用して平滑化
        img_thread = cv2.GaussianBlur(img_thread, average_square, sigma_x)

        cv2.imshow('binary', img_thread)

        # ラベリング処理
        label = cv2.connectedComponentsWithStats(img_thread)

        # エリアの大きさでフィルタリング
        label3_filtered = label[3][np.where((label[2][:, 4] < 10000) & (label[2][:, 4] > 600))]
        label2_filtered = label[2][np.where((label[2][:, 4] < 10000) & (label[2][:, 4] > 600))]
        # 大きさでソート
        area = list(label2_filtered.argsort(axis=0)[::-1][:, 4])

        if len(area) < 1:
            # print([np.nan,np.nan])
            center.append([np.nan,np.nan])
        else:
            max_index = int(area[0])
            # print(list(label3_filtered[max_index]))
            center.append(list(label3_filtered[max_index]))
            cv2.rectangle(frame_gamma,
                          (label2_filtered[max_index,0], label2_filtered[max_index,1]),
                          (label2_filtered[max_index,0] + label2_filtered[max_index,2], label2_filtered[max_index,1] + label2_filtered[max_index,3]),
                          (255, 0, 0))

        cv2.imshow(filename, frame_gamma)

        if cv2.waitKey(1) & 0xFF == ord('q'):
            break

        speed += 1
        if speed % 150 == 0:
            print(".")
        elif speed % 30 == 0:
            print(".", end="")
        else:
            pass
    filename_time = datetime.datetime.today()
    filename_stem = filename + "_" + filename_time.strftime("%Y%m%d%H%M%S") + ".csv"
    output_path = Basedir / "TrackingData"/ filename_stem
    #output_path = "/home/biodocker/WD/TrackingData/" + filename + "_" + filename_time.strftime("%Y%m%d%H%M%S") + ".csv"
    result = pd.DataFrame(center)
    result.to_csv(output_path, header=False, index=False)

    comment_1 = "# Analized filename: " + input + "\n"
    comment_2 = "# Setting file: " + setting + "\n"
    comment_3 = "# V threshold: " + str(threshold) + "\n"

    with open(output_path) as f:
        l = f.readlines()

    l.insert(0, comment_1)
    l.insert(1, comment_2)
    l.insert(2, comment_3)

    with open(output_path, mode='w') as f:
        f.writelines(l)

    print(result)
    cap.release()
    cv2.destroyAllWindows()