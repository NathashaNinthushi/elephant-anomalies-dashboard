# Kaudulla Elephants — Anomaly Detection Dashboard

Interactive Shiny dashboard for anomaly detection in elephant GPS telemetry from Kaudulla National Park, Sri Lanka. Built as part of a wildlife data science collaboration with the Department of Wildlife Conservation, it runs four independent detection methods over ~2.5 years of hourly tracking data from 14 collared elephants.

**Live app:** https://01a0a4d1-f31a-ed15-9460-01e062f6f8bb.share.connect.posit.cloud/

## What it does

The dashboard reads one shared, cleaned GPS dataset and surfaces it through four tabs, each asking "is something wrong with this elephant?" a different way:

| Method | Reference | What it flags |
|---|---|---|
| **Movement rate** | Wall et al. (2014) | Days where an elephant's total daily movement drops to or below its own historical 1st percentile — a per-elephant baseline rather than a herd-wide threshold |
| **Immobility monitor** | Wall et al. (2014) | A sustained stay inside a ~13 m radius for ≥ 5 hours — the published real-time rule for catching death or serious injury early |
| **Revisit hotspots** | Bracis et al. (2018) | How often an elephant returns to the same place, separating core, repeated-use sites from one-off, exploratory locations |
| **Proximity anomalies** | Wall et al. (2014) | Pairwise distance between elephants — unusually long stretches together, separations after a bond, and weekly shifts in togetherness |

## Data

- `kaudulla_elephants_clean.csv` — cleaned GPS telemetry, retained after HDOP ≤ 10 filtering and geographic bounding-box validation
- Source: GPS collar tracking of elephants in Kaudulla National Park, in collaboration with the Department of Wildlife Conservation

## Project structure

```
├── app.R                    # combined UI/server, loads data once and shares it across tabs
├── mod_movement_rate.R      # movement rate anomaly module
├── mod_immobility.R         # immobility monitor module
├── mod_method4.R            # revisit hotspots module
├── mod_proximity.R          # proximity anomalies module
└── kaudulla_elephants_clean.csv
```

## Tech stack

R, Shiny, bslib, plotly, leaflet, DT, and the tidyverse (dplyr, tidyr, lubridate, purrr, readr). Deployed on Posit Connect Cloud.

## Running locally

```r
install.packages(c(
  "shiny", "bslib", "readr", "dplyr", "tidyr", "lubridate",
  "plotly", "leaflet", "DT", "scales", "purrr", "geosphere",
  "htmltools", "ggplot2", "bsicons"
))

shiny::runApp("app.R")
```

## Acknowledgements

Developed as part of a Statistical Consulting Service (SCS) project, Department of Statistics, Faculty of Applied Sciences, University of Sri Jayewardenepura, in collaboration with the Department of Wildlife Conservation, Sri Lanka.
