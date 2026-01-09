# Houston Astros — Player Development Analyst Technical Assessment (Shiny)
# Author: Kylie Chung

This repository contains a coach-friendly Shiny dashboard and a player-facing PDF report generated from three CSV files:
- `pd_analyst_goals.csv` (stated goals)
- `pd_analyst_events.csv` (event-level outcomes)
- `pd_analyst_pitches.csv` (pitch-level outcomes)

The app is designed for fast, low-click insight into **Recent (last 14 days)** vs **Full-season** performance with KPIs: **BB%**, **K%**, **SLG**, and **FPS%**.

---

## Repository structure
`pd-analyst-shiny/`
`app.R`
`report.Rmd`
`README.md`
`data/
pd_analyst_goals.csv
pd_analyst_events.csv
pd_analyst_pitches.csv`

---

## How to run (from a clean R session)

1) Open RStudio  
2) Set working directory to the folder containing `app.R` (the project root)

```r
setwd("~/Desktop/pd-analyst-shiny")  # update to your local path
```
3) Install required packages only (first time)

```r
install.packages(c(
  "shiny","bslib","readr","dplyr","tidyr","lubridate","ggplot2","gt","rmarkdown","knitr"
))
```

4) Run the app

```r
shiny::runApp(".")
```


## note: PDF report setup

The app generates a player-facing PDF using report.Rmd (via a Shiny download button).

If PDF rendering fails (missing LaTeX)

Install TinyTeX once:

```r
install.packages("tinytex")
tinytex::install_tinytex()
```

---


## App views (required)
1) Player Overview

- Select a Player ID

- View stated performance goals

- View KPI summary cards comparing Recent (last 14 days) vs Full-season:

- BB%, K%, SLG, FPS%

- Goal status table compares Recent vs Season vs any goal target (if present in goals data)

2) Trends View

- Daily time series for selected KPI (minimal interaction: choose metric + view)

- “Most recent outing” summary table for quick context

3) Goals View

- Displays the player’s stated goals (primary/secondary/tertiary)


---


## Coach workflows (examples)
Workflow A: Quick check-in before a bullpen/game

- Select player

- Review goals

- Compare Recent vs Season KPI cards to see direction and magnitude

- Go to Trends and quickly check volatility over time

- Download the PDF to share with the player

Workflow B: Identify process vs results mismatch

- Select player

- Look at BB% and K% (plate discipline trend)

- Check SLG trend (power / quality of contact proxy)

- Use “Most recent outing” to anchor what just happened

Workflow C: Goals-first coaching conversation

- Select player

- Start with stated goals

- Use Goal Status table to show current standing (Recent vs Season)

- Download PDF so the athlete leaves with a clear summary


---


## KPI definitions (implementation)
Offense (from pd_analyst_events.csv)

- PA: plate appearances (from pa column if present; otherwise computed from available outcomes in code)

- BB% = BB / PA

- K% = SO / PA

- SLG = Total Bases / AB

- Total Bases derived from 1B/2B/3B/HR outcome columns

Pitching (from pd_analyst_pitches.csv)

- FPS% (First Pitch Strike %) = proportion of plate appearances where the first pitch is a strike

- First pitch is identified as 0–0 count (e.g., balls_before == 0 and strikes_before == 0, or equivalent grouping logic in code)

- Strike pitch results counted as: called_strike, swinging_strike, foul


---


## Data assumptions / column mapping

The app reads CSVs from ./data/.

Dates are read from sched_date and parsed as mm/dd/yyyy.

Player identity is unified across sources by mapping each dataset’s ID column to a canonical player_id used throughout the app:

- Goals: player_id

- Events: mapped to player_id (from the event file’s ID column)

- Pitches: mapped to player_id (from the pitch file’s ID column)

Any renaming/mapping is handled at load time in app.R to keep downstream calculations consistent.


---


## Performance considerations

CSVs are read once at startup (no re-reading on player changes).

Aggregations compute on filtered subsets for the selected player.

Minimal reactivity:

- Two summary tables (Recent vs Season) are computed and re-used across cards/tables

- Trends are computed once per selected player


---


## Tradeoffs / known limitations

This assessment focuses on clear, fast coach workflows over building a complex model.

KPI calculations rely on available columns in the provided CSVs; if a column is missing, the code uses documented fallbacks (or returns NA rather than failing).

Trends are grouped by sched_date for simplicity and speed.


---

## Notes for reviewers

All logic is contained in app.R and the PDF template report.Rmd.

The app is intended to run locally from a clean session using only the packages listed above.

