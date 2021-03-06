# Tracking_behaivor

Python and R script for analyzing mouse moving in behavior experiment

![top](https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/top.gif)

## Getting Started

### Requirement

- Python3

  thise pacakege is requried.
  - opencv-python=4.1.0.25=pypi_0
  - numpy
  - pandas
  - Pathlib
  - thinker
  - datetime

- R

  thise pacakege is requried.
  - sp
  - tidyverse
  - jsonlite
  - stringr

### Installation

```bash
git clone https://github.com/hkrkizum/Tracking_behaivor.git
```

## Preparation  

```bash
Tracking_behaivor/
  ├─ .git/
  ├─ .gitignore
  ├─ README.md
  ├─ Videofiles/
  │   └─ test.mp4
  ├─ Setting/
  │   ├─ FileList/
  │   └─ Polygon/
  │       ├─ FileList/
  │       └─ Polygon/
  │           └─ polygon.csv
  ├─ TrackingData/
  └─ source/
     ├─ pythonfiles/
     │   ├─ Video_1file.py
     │   └─ CoreCode.py
     └─ Rfiles
```

Make Mask Poligon setting file

- 1st col: Number
- 2nd col: X
- 3rd col: Y

→　put in `Setting/`

![Base](https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/polygon.png)

## Usage

### A. 1 file tracking

1. Run tracking

```bash
python .\source\pythonfiles\Video_1file.py
```

- 1-1. pop up. when you select OK, you can choose video file on GUI.

<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step1.png" width="70%">
<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step1_1.png" width="70%">

- 1-2. when you select OK, you can choose CSV file on GUI.

<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step2.png" width="70%">
<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step2_1.png" width="70%">

- 1-3. Set threthold parameter. If you set blank, 35 will be used as default.

<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step3.png" width="70%">

- 1-4. Analized data will be saved in `TrackingData/` automatically.

<img src="https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/1video_step4.png" width="70%">