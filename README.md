# Tracking_behaivor

Python and R script for analyzing mouse moving in behavior experiment

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

1. Make Mask Poligon setting file

![Base screeshot](https://imgur.com/8XNBipB "サンプル")


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