# Flash-Freeze Solution Dashboard (Public)

This repository contains a sanitized single-file Shiny app (`app.R`) with a fully English UI. No deployment credentials are included.

## Data
Included files:
- `flash_freeze_data.csv` — sample dataset (~5k rows) for immediate run
- `generate_data.py` — Python script to regenerate the dataset

Regenerate data with Python:
```bash
python generate_data.py
```

The app expects `flash_freeze_data.csv` to be in the same folder as `app.R`.

## Run locally
```r
shiny::runApp('app.R')
```

## Deploy
Use your own `rsconnect::setAccountInfo()` in a private script. Do not commit tokens.

## Credits
- Course: Big Data Analysis & R Language - Lab Work 3.5
- Group: 14
- Template inspiration: FAO Crops & Livestock Production Dashboard (v2.3, 2025)
- Maintainer: Jimmy

