# Gadgetbridge Lab

Gadgetbridge Lab is an offline R Shiny dashboard for exploring data exported or synced through [Gadgetbridge](https://gadgetbridge.org/). It supports raw Gadgetbridge SQLite databases and ZIP exports, plus FIT workout files, and provides detailed views for activity, sleep, heart rate, stress, workouts, trends, and recovery.

The app is designed for local, personal data analysis. It runs entirely in R and does not upload the database or workout files to a service.

<!-- Screenshot placeholders will be added later. -->

## Features

- Daily overview with activity rings, key tiles, heart rate, and sleep summary
- Step totals, walking-bout detection, activity cadence, and sedentary-time estimates
- Heart-rate timelines, daily distributions, resting-heart-rate monitoring, and stress
- Sleep stages, nightly duration and efficiency, sleep regularity, social jetlag, and stage-specific heart rate
- Workout calendar, session details, pace, heart-rate zones, sport volume, and training load
- Long-term trends, monthly reviews, weekday patterns, and metric correlations
- HRV, skin temperature, sleep statistics, apnea-event estimates, and ECG metadata
- Huami/Amazfit/Zepp/Mi Band and Huawei/Honor device adapters
- Optional FIT-file parsing without requiring an additional FIT package
- Dark, neon, and light themes
- Optional synthetic demo values for testing advanced-metric layouts

## Supported inputs

### Gadgetbridge database

The app accepts:

- A local `Gadgetbridge.db` or another SQLite database file
- A Gadgetbridge ZIP export containing `database/Gadgetbridge`

Supported current adapters cover:

- **Huawei/Honor:** `HUAWEI_*` tables
- **Huami:** Amazfit, Zepp OS, and compatible Mi Band tables

If another brand is encountered, the app reports that it is unsupported and lists the non-empty database tables to help add an adapter.

### Workout files

You can provide FIT workout files in either of these ways:

- Select one or more `.fit` files in the sidebar
- Enter a folder path and search it recursively
- Include FIT files in a Gadgetbridge ZIP export

The built-in FIT parser reads record and session messages for time, heart rate, speed, elevation, cadence, distance, energy, sport, and training effect where those fields exist.

## Installation

Gadgetbridge Lab requires R. It also uses the following R packages:

- `shiny`
- `bslib`
- `DBI`
- `RSQLite`
- `dplyr`
- `plotly`

Install them with:

```r
install.packages(c("shiny", "bslib", "DBI", "RSQLite", "dplyr", "plotly"))
