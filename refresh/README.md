# Refresh Pipeline

Pulls stats from all 30 team Google Sheets, rebuilds pre-computed RDS/CSV files in `~/nbs-data/`, and restarts the live Shiny service.

## Scripts

### `refresh.sh` — full data refresh

```bash
bash refresh/refresh.sh --season 25-26 --playoff-date 2025-04-16 --drop-date 2025-04-25
```

| Flag | Required | Description |
|---|---|---|
| `--season` | No | Season string, e.g. `25-26`. Defaults to current season based on system date (Sep 30 cutoff). |
| `--playoff-date` | No | First date of playoff games. Stats on/after this date are written to the playoffs CSV. Omit if no playoffs yet. |
| `--drop-date` | No | Drop all game rows after this date. Defaults to today. |

**Steps:**
1. `git pull` the repo
2. Run `job.R` (downloads Sheets, validates, builds all RDS/CSV outputs)
3. Copy all `*.csv` files from `~/nbs-data/` to `/var/www/stats.nbn.today/files/`
4. `git add/commit/push` any changed files
5. `git pull` in `/srv/shiny/nothing-but-stats`
6. `sudo systemctl restart shiny-release.service`

Logs to `/var/log/refresh.log`. Exits immediately on any error (`set -e`).

---

### `deploy.sh` — code-only deploy

```bash
bash refresh/deploy.sh
```

Use this after UI or code-only changes when data hasn't changed. Skips the R pipeline entirely.

**Steps:**
1. Commit and push any uncommitted local changes
2. `git pull` in `/srv/shiny/nothing-but-stats`
3. `sudo systemctl restart shiny-release.service`

---

### `job.R` — R preprocessing

```bash
Rscript refresh/job.R "25-26" "2025-04-16" "2025-04-25"
```

Called by `refresh.sh` but can also be run standalone. Reads `NBS_DATA_DIR` env var (defaults to `~/nbs-data`) for all output paths.

**Pipeline:**
1. Download all 30 team Sheets via `get_allstats()` in `refresh-utils.R`
2. Validate with `check_allstats()` — aborts on minute-total errors, stat sanity failures, or missing data for any game on or before `drop-date`
3. Filter rows to `<= drop-date`
4. Split into regular season (before `playoff-date`) and playoffs (on/after)
5. Write per-season CSVs (`allstats-{season}.csv`, `allstats-playoffs-{YY}.csv`)
6. Load all historical CSVs from disk, inject current season from memory, run `clean_allstats()`
7. Pre-compute and write all RDS files (see below)
8. Write `standings.csv` and `allstats.csv`

---

## Utility Files

- **`refresh-utils.R`** — data acquisition and transformation: `get_allstats()`, `import_team_sheet()`, `check_allstats()`, `build_allstats()`, `add_playoff_info()`, `clean_allstats()`, `load_allstats()`, conference/division lookup helpers
- **`preprocess-utils.R`** — aggregations used only at build time: `get_win_streaks()`, `get_newsfeed()`, `calculate_team_offense_defense()`, `get_achievements_game()`, `get_achievements_season()`

---

## Output Files (`~/nbs-data/`)

The `NBS_DATA_DIR` environment variable controls this path (default: `/home/skim/nbs-data`).

### CSVs — also copied to `/var/www/stats.nbn.today/files/` for public download

| File | Description |
|---|---|
| `allstats-{YY-YY}.csv` | Raw regular season game rows for one season (e.g. `allstats-24-25.csv`). One row per player per game. Includes `gametype = "REG"`. |
| `allstats-playoffs-{YY}.csv` | Raw playoff game rows for one season (e.g. `allstats-playoffs-25.csv`). Includes `ROUND`, `GAME`, `gametype = "PLAYOFF"`. |
| `allstats.csv` | Full combined dataset — all seasons, regular + playoffs — after `clean_allstats()`. |
| `standings.csv` | Current regular season standings: `SEED`, `TEAM`, `GB`, `W`, `L`, `PCT`, `PPG`, `OPPG`, `DIFF`. Tie-breaking follows NBA rules (H2H → division winner → division PCT → conference PCT → point diff). |
| `player-bio-database.csv` | Player bio data (DOB, experience, college, country). Used by `build_allstats()` to attach age at game date. Not written by `job.R` — maintained separately. |

### RDS — loaded by `global.R` at Shiny startup

| File | Description |
|---|---|
| `dfs.rds` | All regular season game-level rows, all seasons. The primary dataset. Includes `GMSC`, `TS`, `ROOKIE`, `WL`, `TEAM_PTS`, `OPP_TEAM_PTS`. |
| `dfs_playoffs.rds` | All playoff game-level rows, all seasons. Same schema as `dfs.rds` plus `ROUND` and `GAME`. |
| `standings.rds` | Named list keyed by season string (e.g. `"24-25"`). Each element is the output of `compute_standings()`. Pre-computed for all historical seasons. |
| `team_stats.rds` | Named list keyed by season string. Each element is the output of `compute_team_stats()`. Pre-computed for all historical seasons. |
| `my_ranks.rds` | Player rankings across all stat categories, output of `get_ranks(dfs)` from `app/R/utils.R`. |
| `team_ratings.rds` | Per-team offensive/defensive ratings (adjusted for opponent) per season, output of `calculate_team_offense_defense()`. |
| `news.rds` | Newsfeed entries: high GMSC games (≥ 35), triple-doubles, all-time NBN records, career highs, career milestone totals (every 1,000 in P/R/A/S/B/3PM). |
| `game_high_player.rds` | Game rows where at least one of P/R/A/S/B ≥ 5. Used for game-high leaderboards. |
| `season_high_player.rds` | Season totals per player per season (M, P, R, A, S, B, 3PM, TO, PF, TD). |
| `game_high_team.rds` | Per-game team totals (sum of all players), includes point differential. |
| `season_high_team.rds` | Season totals per team with W/L record, PPG, OPPG, margin of victory. |
| `wl_streaks.rds` | Win and loss streaks ≥ 10 games across all seasons, sorted by streak length. |
| `cum_diff.rds` | Cumulative point differential over time per franchise (all seasons). Used in the franchise profiles tab. |
| `playoff_top_performers.rds` | Top 3 players by GMSC per team per playoff game. Used in the head-to-head tab. |

### RDS — legacy / currently unused

| File | Description |
|---|---|
| `ach_game.rds` | Per-game achievements (Snell Award, Stinker, One Man Show, etc.). Generation is commented out in `job.R`. |
| `ach_season.rds` | Per-season achievements (Carry Job, 2K Club, 50/40/90, etc.). Generation is commented out in `job.R`. |
| `ach_reg.rds` | Unknown — not written by current `job.R`. Likely a prior naming of `ach_season.rds`. |
| `bios.rds` | Player bio data as RDS. Not written by current `job.R`. |
| `dfs_everything.rds` | Not written by current `job.R`. Likely a prior combined `dfs` + `dfs_playoffs`. |
