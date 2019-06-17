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

### Installation

```
$ git clone https://github.com/hkrkizum/Tracking_behaivor.git
```

## Preparation  

```
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

→　put in Setting/

![Base](https://github.com/hkrkizum/Tracking_behaivor/blob/image/images/polygon.png)

