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

### Set format file

```
Docker_POMSandSTAI/
  ├─ .git/
  ├─ .gitignore
  ├─ README.md
  ├─ docker-compose.yml
  └─ data/
      ├─ Source/
      │    ├─ main_POMS.py
      │    ├─ main_STAI.py
      │    ├─ CoreCode.py
      │    ├─ Score/
      │    │   └─ Tscore...csv
      │    └─ Container/
      │        └─ Dockerfile       
      ├─  Result/
      └─  Rawdata/
            ├─  Rawdata_format_POMS.xlxs
            └─  Rawdata_format_STAI.xlxs
```