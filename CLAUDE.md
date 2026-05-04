# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nothing But Stats is an R Shiny dashboard for a private simulation basketball league (NBN — Nothing But Net). 30 team owners enter real NBA player stats into individual Google Sheets; this app aggregates and displays them. Hosted at `https://stats.nbn.today` on a Linux server running Shiny Server.

## Running the App

```r
# From the app/ directory
setwd("~/nothing-but-stats/app")
shiny::runApp()
```

No automated test suite, linter, or CI/CD pipeline exists.

## Data Refresh Pipeline

The pipeline pulls all 30 team Google Sheets, rebuilds pre-computed RDS files, and restarts the live service:

```bash
# Full automated refresh (from repo root)
bash refresh/refresh.sh --season 25-26 --playoff-date 2025-04-16 --drop-date 2025-04-25

# R preprocessing only
Rscript refresh/job.R "25-26" "2025-04-16" "2025-04-25"
```

The shell script: git pulls → R preprocessing → copies CSVs to `/var/www/stats.nbn.today/files/` → commits/pushes data → pulls into `/srv/shiny/nothing-but-stats` → restarts `shiny-release.service`.

## Architecture

**Data flow:** Google Sheets → `refresh/job.R` → `app/data/*.rds` → `app/global.R` (loads at startup) → reactive Shiny session

**Key directories:**
- `app/R/` — shared helpers sourced by `global.R` at startup
- `app/server_modules/` — one file per UI tab, sourced with `local=TRUE` in `server.R`
- `app/data/` — pre-built RDS/CSV files (not in version control for large files)
- `refresh/` — data pipeline scripts

**Module pattern:** `server.R` sources all `server_modules/*.R` files with `local=TRUE`. Shared reactives (`myPlayerData`, `mySeasonDF`, `gamelist`) live in `server_modules/_init.R`.

**Performance strategy:** All expensive calculations (standings, rankings, team stats) are pre-computed during refresh and saved as named-list RDS files keyed by season string (e.g., `standings_precomputed[["24-25"]]`). `global.R` loads these once at startup. This is intentional given the 4 GB server constraint.

## Key Conventions

**Season strings:** `"24-25"` for regular season, `"24-25 Playoffs"` for playoffs. Derived from CSV filenames like `allstats-24-25.csv`.

**Stats column names:** Single-letter abbreviations — `P` (points), `R` (rebounds), `A` (assists), `S` (steals), `B` (blocks), `M` (minutes), `TO` (turnovers), `PF` (fouls). Per-game averages use `PG` suffix (e.g. `PPG`). Three-point columns require backtick quoting: `` `3PM` ``, `` `3PA` ``.

**Player names:** `LAST, FIRST` ALL-CAPS convention. Alternate spellings are canonicalized in the `case_when` block inside `clean_allstats()` in `refresh/refresh-utils.R`.

**OPP column:** Away games have an `@` prefix (e.g. `@BOS`). `OPP_RAW` strips the `@`. Standings deduplication uses `!str_detect(OPP, "@")` to avoid double-counting.

**GMSC formula:** `P + (0.4*FGM) - (0.7*FGA) - (0.4*(FTA-FTM)) + (0.7*OR) + (0.3*DR) + S + (0.7*A) + (0.7*B) - (0.4*PF) - TO`

**Minutes validation:** Valid game totals are 240 (regulation), 265 (1OT), 290 (2OT), 315 (3OT). `check_allstats()` flags deviations.

**Award metadata:** All historical awards (All-Stars, MVP, DPOY, HOF, champions, playoff seeds, owners) are hard-coded as `tribble()` calls in `app/R/metadata.R`. These must be manually extended each season.

**RStudio project settings:** 2-space indentation, UTF-8 encoding (see `nothing-but-stats.Rproj`).
