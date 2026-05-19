# Refresh Pipeline

Pulls stats from all 30 team Google Sheets, rebuilds pre-computed RDS/CSV files in `$NBS_DATA_DIR`, and restarts the live Shiny service.

## Entry Point

All pipeline operations go through `nbs.sh`:

```bash
bash refresh/nbs.sh <subcommand> [flags]
```

---

## Subcommands

### `refresh` — full pipeline

```bash
bash refresh/nbs.sh refresh
bash refresh/nbs.sh refresh --season 25-26
bash refresh/nbs.sh refresh --season 25-26 --playoffs-from 2026-04-15
bash refresh/nbs.sh refresh --season 25-26 --through 2026-04-25
```

Runs pull + build + deploy in a single R session. The playoff start date is auto-looked up from `seasons.conf` when not provided explicitly.

---

### `pull` — download Sheets, validate, write per-season CSVs

```bash
bash refresh/nbs.sh pull
bash refresh/nbs.sh pull --through 2026-04-25
```

Downloads all 30 team Google Sheets, validates data (minute totals, stat sanity, missing values), filters rows to `--through` date, splits into regular season and playoff CSVs, and writes them to `$NBS_DATA_DIR`. Aborts on any data error for games on or before `--through`.

**Steps:**
1. `git pull` the repo
2. Download Sheets via `get_allstats()`, validate with `check_allstats()`
3. Split on `--playoffs-from` date (from `seasons.conf` if not passed)
4. Write `allstats-{YY-YY}.csv` and `allstats-playoffs-{YY}.csv`

---

### `build` — compute all RDS/CSV outputs from existing CSVs

```bash
bash refresh/nbs.sh build
bash refresh/nbs.sh build --season 25-26
```

Loads the current season's CSVs from `$NBS_DATA_DIR` (written by a prior `pull`), combines with all historical CSVs, and rebuilds every RDS file. Use this when you want to rebuild outputs without re-downloading Sheets (e.g. after metadata changes in `metadata.R`).

**Steps:**
1. Load current season from `allstats-{YY-YY}.csv` (and playoff CSV if present)
2. Load historical CSVs via `load_allstats()`
3. Run `clean_allstats()`, compute all RDS outputs
4. Write `standings.csv`, `owner_stats.csv`, `allstats.csv`
5. Copy all `*.csv` to `/var/www/stats.nbn.today/files/`

---

### `deploy` — commit, push, and restart service

```bash
bash refresh/nbs.sh deploy
```

Commits and pushes any local changes, pulls into `/srv/shiny/nothing-but-stats`, and restarts `shiny-release.service`. Use this after UI or code-only changes when data hasn't changed.

---

## Flags

| Flag | Applies to | Description |
|---|---|---|
| `--season` | all | Season string, e.g. `25-26`. Defaults to current season (Sep 30 cutoff). |
| `--playoffs-from` | `pull`, `refresh` | First date of playoff games. Stats on/after this date go to the playoffs CSV. Auto-looked up from `seasons.conf` if omitted. |
| `--through` | `pull`, `refresh` | Drop all game rows after this date. Defaults to today. |

---

## Season Config (`seasons.conf`)

Maps season strings to their playoff start dates. Consulted when `--playoffs-from` is not passed explicitly.

```
25-26=2026-04-15
24-25=2025-04-16
```

**To add a new season:** append a line before playoffs start (leave blank), then fill in the date when you know it:

```
26-27=
25-26=2026-04-15
```

---

## Backward Compatibility

The old scripts still work — they delegate to `nbs.sh`:

```bash
bash refresh/refresh.sh --season 25-26 --playoff-date 2025-04-16 --drop-date 2025-04-25
bash refresh/deploy.sh
```

`--playoff-date` and `--drop-date` are translated to `--playoffs-from` and `--through` automatically.

---

## Logging

All output is appended to `/var/log/refresh.log`. Scripts exit immediately on any error (`set -e`).

---

## Utility Files

- **`refresh-utils.R`** — data acquisition and transformation: `get_allstats()`, `import_team_sheet()`, `check_allstats()`, `build_allstats()`, `add_playoff_info()`, `clean_allstats()`, `load_allstats()`, conference/division lookup helpers
- **`preprocess-utils.R`** — aggregations used only at build time: `get_win_streaks()`, `get_newsfeed()`, `calculate_team_offense_defense()`

---

## Output Files (`$NBS_DATA_DIR`, default `/home/skim/nbs-data`)

### CSVs — also copied to `/var/www/stats.nbn.today/files/`

| File | Description |
|---|---|
| `allstats-{YY-YY}.csv` | Raw regular season game rows for one season. |
| `allstats-playoffs-{YY}.csv` | Raw playoff game rows for one season. |
| `allstats.csv` | Full combined dataset — all seasons, regular + playoffs. |
| `standings.csv` | Current standings with NBA tie-breaking rules. |
| `owner_stats.csv` | Owner aggregate stats (W/L, ratings, playoff depth). |

### RDS — loaded by `global.R` at Shiny startup

| File | Description |
|---|---|
| `dfs.rds` | All regular season game-level rows, all seasons. Primary dataset. |
| `dfs_playoffs.rds` | All playoff game-level rows, all seasons. |
| `standings.rds` | Named list keyed by season: pre-computed standings. |
| `team_stats.rds` | Named list keyed by season: pre-computed team stats. |
| `my_ranks.rds` | Player rankings across all stat categories. |
| `team_ratings.rds` | Per-team offensive/defensive ratings per season. |
| `news.rds` | Newsfeed entries (high GMSC, records, career milestones). |
| `game_high_player.rds` | Game rows where at least one of P/R/A/S/B ≥ 5. |
| `season_high_player.rds` | Season totals per player per season. |
| `game_high_team.rds` | Per-game team totals with point differential. |
| `season_high_team.rds` | Season totals per team with W/L and margin of victory. |
| `wl_streaks.rds` | Win/loss streaks ≥ 10 games, sorted by length. |
| `cum_diff.rds` | Cumulative point differential per franchise over time. |
| `playoff_top_performers.rds` | Top 3 players by GMSC per team per playoff game. |
