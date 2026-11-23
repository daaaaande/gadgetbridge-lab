Gadgetbridge Lab
===============

Gadgetbridge Lab is an offline R Shiny dashboard for exploring data exported or synced through Gadgetbridge (https://gadgetbridge.org/). It supports Gadgetbridge SQLite databases, ZIP exports, and FIT workout files, with detailed views for activity, sleep, heart rate, stress, workouts, trends, and recovery.

The app is designed for local, personal data analysis. It runs entirely in R and does not upload databases or workout files to a service.

<!-- Screenshots can be added later. -->


Features
--------

- Daily overview with activity rings, key tiles, heart rate, and sleep
- Steps, walking-bout detection, cadence, and sedentary-time estimates
- Heart-rate timelines, distributions, resting heart rate, and stress
- Sleep stages, duration, efficiency, regularity, sleep debt, and stage-specific heart rate
- Workout calendar, session details, pace, heart-rate zones, sport volume, and training load
- Long-term trends, monthly reviews, weekday patterns, and metric correlations
- HRV, skin temperature, sleep statistics, RDI, SpO2, and ECG metadata
- Huawei/Honor and Huami/Amazfit/Zepp/Mi Band database adapters
- Built-in, dependency-free FIT-file parsing
- Dark, neon, and light themes
- Synthetic demo values for testing advanced-metric layouts


Supported data sources
----------------------

Gadgetbridge database
~~~~~~~~~~~~~~~~~~~~~~

The app accepts:

- A local .db or .sqlite file
- A Gadgetbridge ZIP export containing database/Gadgetbridge

Current adapters cover:

- Huawei/Honor: HUAWEI_* tables
- Huami: Amazfit, Zepp OS, and compatible Mi Band tables

Data from unsupported brands is reported by the app, including a list of non-empty database tables. That information can be used to add a new device adapter.

FIT workouts
~~~~~~~~~~~~

Workout files can be loaded by:

- Selecting one or more .fit files
- Providing a folder that is searched recursively
- Including FIT files in a Gadgetbridge ZIP export

The built-in FIT parser reads available record and session fields, including time, heart rate, speed, elevation, cadence, distance, energy, sport, and training effect.


Requirements
------------

Gadgetbridge Lab requires R and the following packages:

- shiny
- bslib
- DBI
- RSQLite
- dplyr
- plotly

Install the dependencies in R:

install.packages(c(
  "shiny",
  "bslib",
  "DBI",
  "RSQLite",
  "dplyr",
  "plotly"
))


Running the app
---------------

Place app_complete.R in the repository root and run:

shiny::runApp("app_complete.R")

You can also open app_complete.R in RStudio and select Run App.

Alternatively, run it from a terminal:

R -e 'shiny::runApp("app_complete.R")'


Getting started
---------------

1. Export or copy a Gadgetbridge database or export ZIP.
2. Start Gadgetbridge Lab.
3. Upload the .db, .sqlite, or .zip file in the sidebar.
4. Optionally select FIT files or enter a folder containing them.
5. Configure the time zone, date range, and analysis settings.

The app does not require a Gadgetbridge account or a cloud service.


Privacy
-------

- Data remains local in the R/Shiny process.
- Uploaded databases and workout files are temporary application inputs.
- The app contains no automatic telemetry or upload code.
- Demo mode uses generated values rather than personal data.

Gadgetbridge databases may contain biometric measurements, sleep patterns, workout history, device identifiers, and location-derived information. Review exported databases and screenshots before sharing them.


Configuration
-------------

The sidebar includes controls for:

- Age and sex
- Maximum-heart-rate estimation
- Resting-heart-rate override
- Step and active-minute goals
- Walking-bout cadence and duration
- Likely step-artefact margin
- Sleep-stage code mapping
- Sleep-session gap and minimum-night duration
- Waking window
- Free days and sleep need
- Theme
- Weight, height, and stride for rough estimates

Sleep-stage codes vary by device and firmware. The app allows users to remap light, deep, REM, and awake codes.


App sections
------------

- Today:
  Daily rings, tiles, timeline, and selected-day heart rate and sleep

- Metrics:
  Distances, calories, stress, SpO2, and fitness estimates

- Monthly review:
  Month-to-month changes and workout summaries

- Trends:
  Robust trend tests and smoothed metric plots

- Correlations:
  Pairwise relationships, lag analysis, and pair scanning

- Sleep:
  Stages, efficiency, regularity, sleep debt, and social jetlag

- Heart:
  Timeline, distributions, resting heart rate, stress, and SpO2

- Steps:
  Cadence, quality, hourly patterns, and sedentary time

- Workouts & training:
  Sessions, zones, pace, load, intensity, and sport trends

- Recovery & advanced:
  HRV, temperature, sleep scores, RDI, SpO2, and ECG metadata

- Data & methods:
  Detected source, table coverage, references, assumptions, and derived parameters


Interpretation and limitations
------------------------------

Gadgetbridge Lab is intended for personal trend analysis and education, not medical diagnosis.

Important limitations include:

- Wrist sleep stages and HRV estimates are less reliable than clinical measurements.
- Heart-rate-derived zones and fitness estimates are approximate.
- Distances and calories depend on sensor quality, manufacturer estimates, stride, weight, and height.
- Training-load calculations describe training data and should not be used to predict injury or prescribe exercise.
- Correlations do not establish cause and effect.
- Workouts can contain missing, duplicated, or unverified sensor data.
- ECG metadata and advanced health metrics vary by device and firmware.

The Data & methods tab documents formulas, thresholds, references, and assumptions.


Repository structure
--------------------

app_complete.R   Main Shiny UI and server implementation
README.md        Project documentation


Troubleshooting
---------------

Missing package error
~~~~~~~~~~~~~~~~~~~~~

Install the missing packages:

install.packages(c(
  "shiny",
  "bslib",
  "DBI",
  "RSQLite",
  "dplyr",
  "plotly"
))

Database not detected
~~~~~~~~~~~~~~~~~~~~~

Check that:

- The file is a SQLite database or Gadgetbridge ZIP export
- The ZIP contains database/Gadgetbridge
- The watch has synchronized data into Gadgetbridge
- Supported activity tables contain records

No sleep sessions shown
~~~~~~~~~~~~~~~~~~~~~~~

Check:

- The selected date range
- The selected time zone
- Whether staged or begin/end sleep records exist
- Sleep-stage code mappings
- The minimum-night duration and sleep-gap settings

No FIT workouts found
~~~~~~~~~~~~~~~~~~~~~

Check that:

- The folder exists and is accessible to R
- FIT files use the .fit extension
- The FIT file contains valid definition and record messages

Unsupported device
~~~~~~~~~~~~~~~~~~

Open Data & methods and inspect the non-empty table list. A device adapter can be added by converting the device-specific tables into the app's common activity format.


Screenshots
-----------

Screenshots will be added later:

- Dashboard / Today overview
- Sleep analysis
- Workouts and training
- Trends and correlations
- Recovery and advanced metrics


Contributing
------------

Contributions are welcome. Useful additions include:

- Additional Gadgetbridge device adapters
- Improved FIT parsing
- Unit tests for adapters and calculations
- Device and table-schema documentation
- Bug reports with anonymized reproduction data

Please do not commit databases, FIT files, or screenshots containing personal health data.


License
-------

A software license has not yet been chosen for this repository.


Disclaimer
----------

Gadgetbridge Lab is an independent analysis project and is not affiliated with or endorsed by Gadgetbridge, Huawei, HONOR, Amazfit, Zepp, Xiaomi, or other wearable manufacturers.

All health and fitness metrics are estimates and should not replace advice from a qualified health professional.
