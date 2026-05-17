# args <- c("refresh", "", "", "")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  rlang::abort("Four arguments required: mode, season, playoffs_from, through")
}

mode          <- args[1]  # pull | build | refresh
season        <- args[2]  # e.g. "25-26" or ""
playoffs_from <- args[3]  # e.g. "2026-04-15" or ""
through       <- args[4]  # e.g. "2026-05-10" or ""

# Set-Up ----
setwd("~/projects/nothing-but-stats")
source("refresh/refresh-utils.R")
source("refresh/preprocess-utils.R")
source("app/R/metadata.R")

DATA_DIR <- Sys.getenv("NBS_DATA_DIR", "/home/skim/nbs-data")

today        <- Sys.Date()
current_year <- as.numeric(format(today, "%Y"))
cutoff_date  <- as.Date(paste0(current_year, "-09-30"))

# Default season
if (season == "") {
  season <- if (today <= cutoff_date) {
    paste0(substr(current_year - 1, 3, 4), "-", substr(current_year, 3, 4))
  } else {
    paste0(substr(current_year, 3, 4), "-", substr(current_year + 1, 3, 4))
  }
}
season_code   <- str_extract(season, "\\d{2}-\\d{2}")
season_suffix <- str_extract(season, "\\d{2}$")

# Default through date
if (through == "") {
  through <- today
  inform(glue("Through date defaulted to today ({today})."))
}

# Pull Phase ----
if (mode %in% c("pull", "refresh")) {
  inform("\nPulling data from sheets....")
  delete_before_date <- cutoff_date
  if (today < cutoff_date) {
    delete_before_date <- ymd(str_c(c(
      year(cutoff_date) - 1,
      month(cutoff_date),
      day(cutoff_date)
    ), collapse = "-"))
  }

  allstats <- get_allstats(delete_before = delete_before_date) %>%
    check_allstats()
  inform(" * DONE")

  if (nrow(allstats$errors$games %>% filter(DATE <= through)) > 0) {
    abort(c(
      "Found errors in the Sheets data.",
      str_c(
        allstats$errors$games$TEAM,
        " on ",
        allstats$errors$games$DATE,
        " bc of: ",
        allstats$errors$games$REASON
      )
    ))
  }

  allstats$data <- allstats$data %>% filter(DATE <= through)
  inform("No errors detected in Sheets data. Printing number of games detected in last 5 days.")
  errchk <- allstats$data %>%
    distinct(DATE, TEAM, OPP) %>%
    group_by(DATE) %>%
    mutate(n_sides = n()) %>%
    ungroup() %>%
    arrange(DATE)

  print(
    errchk %>% filter(DATE %in% tail(sort(unique(DATE)), 5)),
    n = 999
  )

  if (nrow(errchk %>% filter(n_sides %% 2 == 1)) > 0) {
    abort("Missing games detected (uneven number of sides on a day).")
  }

  inform("Building allstats....")
  built_allstats <- build_allstats(allstats$data)

  current_reg_raw <- if (playoffs_from == "") {
    inform("No playoffs_from provided, assuming no playoff data.")
    built_allstats
  } else {
    inform(glue("Splitting at {playoffs_from} for playoff data."))
    built_allstats %>% filter(DATE < as.Date(playoffs_from))
  }

  current_playoff_raw <- if (playoffs_from != "") {
    built_allstats %>% filter(DATE >= as.Date(playoffs_from)) %>% add_playoff_info()
  } else {
    NULL
  }

  current_reg_raw %>%
    mutate(gametype = "REG") %>%
    write_csv(file.path(DATA_DIR, glue("allstats-{season_code}.csv")))

  if (!is.null(current_playoff_raw) && nrow(current_playoff_raw) > 0) {
    inform("Some of these are playoff stats. Exporting.")
    current_playoff_raw %>%
      mutate(gametype = "PLAYOFF") %>%
      write_csv(file.path(DATA_DIR, glue("allstats-playoffs-{season_suffix}.csv")))
  }

  if (mode == "pull") {
    inform("Pull complete.")
    quit(status = 0)
  }
}

# Build Phase ----
if (mode == "build") {
  reg_csv <- file.path(DATA_DIR, glue("allstats-{season_code}.csv"))
  if (!file.exists(reg_csv)) {
    abort(glue("Regular season CSV not found: {reg_csv}. Run 'nbs pull' first."))
  }
  inform(glue("Loading current season from {reg_csv}"))
  current_reg_raw <- data.table::fread(reg_csv) %>%
    tibble() %>%
    mutate(DATE = as.Date(DATE))

  playoff_csv <- file.path(DATA_DIR, glue("allstats-playoffs-{season_suffix}.csv"))
  current_playoff_raw <- if (file.exists(playoff_csv)) {
    inform(glue("Loading playoff data from {playoff_csv}"))
    data.table::fread(playoff_csv) %>% tibble() %>% mutate(DATE = as.Date(DATE))
  } else {
    NULL
  }
}

# Post-processing: load historical seasons from disk, inject current season from memory ----
hist_reg <- load_allstats() %>%
  discard(~ any(.x$SEASON == season, na.rm = TRUE))

hist_playoffs <- load_allstats(playoffs = TRUE) %>%
  discard(~ any(.x$SEASON == str_c(season, " Playoffs"), na.rm = TRUE))

dfs <- c(
  hist_reg,
  list(current_reg_raw %>% mutate(SEASON = season))
) %>%
  clean_allstats() %>%
  mutate(gametype = 'REG', GAME = NA_integer_, ROUND = NA_integer_) %>%
  group_by(PLAYER) %>%
  mutate(ROOKIE = SEASON == min(SEASON)) %>%
  ungroup()

dfs_playoffs_items <- c(
  hist_playoffs,
  if (!is.null(current_playoff_raw) && nrow(current_playoff_raw) > 0)
    list(current_playoff_raw %>% mutate(SEASON = str_c(season, " Playoffs")))
  else
    list()
)
dfs_playoffs <- dfs_playoffs_items %>%
  clean_allstats() %>%
  mutate(gametype = 'PLAYOFF', ROOKIE = NA)

dfs_all <- bind_rows(dfs, dfs_playoffs)

write_rds(dfs, file.path(DATA_DIR, 'dfs.rds'), compress = "xz")
write_rds(get_newsfeed(dfs), file.path(DATA_DIR, 'news.rds'), compress = "xz")

write_rds(dfs_playoffs, file.path(DATA_DIR, 'dfs_playoffs.rds'), compress = "xz")

team_ratings <- calculate_team_offense_defense(dfs)
write_rds(team_ratings, file.path(DATA_DIR, 'team_ratings.rds'), compress = "xz")

inform("Calculating player ranks....")
source("app/R/utils.R")
my_ranks <- get_ranks(dfs)
write_rds(my_ranks, file.path(DATA_DIR, 'my_ranks.rds'), compress = "xz")
inform(" * DONE")

inform("Pre-computing standings and team stats for all seasons....")
seasons <- sort(unique(dfs$SEASON))
standings_list <- list()
team_stats_list <- list()

for (season in seasons) {
  inform(glue("  Computing for season {season}..."))
  season_df <- dfs %>% filter(SEASON == season)

  standings_list[[season]] <- compute_standings(season_df)
  team_stats_list[[season]] <- compute_team_stats(season_df)
}

write_rds(standings_list, file.path(DATA_DIR, 'standings.rds'), compress = "xz")
write_rds(team_stats_list, file.path(DATA_DIR, 'team_stats.rds'), compress = "xz")
inform(" * DONE")

inform("Building owner_stats.csv....")

owner_data <- read_csv(file.path(DATA_DIR, "owners.csv"), show_col_types = FALSE) %>%
  mutate(start_date = mdy(start_date), TEAM = toupper(team)) %>%
  select(-team) %>%
  arrange(TEAM, start_date) %>%
  group_by(TEAM) %>%
  mutate(
    end_date = if_else(
      row_number() < n(),
      lead(start_date) - days(1),
      as.Date(Sys.Date())
    )
  ) %>%
  ungroup()

game_data <- dfs_all %>%
  filter(!is.na(WL)) %>%
  distinct(TEAM, DATE, SEASON, WL, gametype) %>%
  mutate(DATE = as.Date(DATE))

team_game_counts <- dfs %>%
  mutate(OPP_RAW = str_replace(OPP, "@", "")) %>%
  distinct(SEASON, TEAM, OPP_RAW, DATE) %>%
  group_by(TEAM, SEASON) %>%
  summarize(n_games = n(), .groups = "drop")

owner_ratings <- team_ratings %>%
  left_join(team_game_counts, by = c("TEAM", "SEASON")) %>%
  mutate(
    year2 = as.integer(paste0("20", str_extract(SEASON, "\\d{2}$"))),
    midpoint_date = as.Date(paste0(year2, "-01-01"))
  ) %>%
  inner_join(
    owner_data %>% select(owner, TEAM, start_date, end_date),
    by = join_by(TEAM, midpoint_date >= start_date, midpoint_date <= end_date)
  ) %>%
  group_by(owner) %>%
  summarize(
    off_rtg = round(weighted.mean(OFF_RTG, n_games), 2),
    def_rtg = round(weighted.mean(DEF_RTG, n_games), 2),
    .groups = "drop"
  )

wl_stats <- owner_data %>%
  group_by(owner) %>%
  group_modify(~ {
    owner_periods <- .x
    owner_games <- game_data %>%
      inner_join(owner_periods %>% select(TEAM, start_date, end_date), by = "TEAM") %>%
      filter(DATE >= start_date & DATE <= end_date)
    po_games <- owner_games$SEASON[owner_games$gametype == "PLAYOFF"]
    tibble(
      teams               = str_c(sort(unique(owner_games$TEAM)), collapse = ", "),
      reg_w               = sum(owner_games$WL == "W" & owner_games$gametype == "REG",     na.rm = TRUE),
      reg_l               = sum(owner_games$WL == "L" & owner_games$gametype == "REG",     na.rm = TRUE),
      playoff_w           = sum(owner_games$WL == "W" & owner_games$gametype == "PLAYOFF", na.rm = TRUE),
      playoff_l           = sum(owner_games$WL == "L" & owner_games$gametype == "PLAYOFF", na.rm = TRUE),
      playoff_appearances = n_distinct(str_remove(po_games, " Playoffs"))
    )
  }) %>%
  ungroup()

reg_season_wl <- owner_data %>%
  group_by(owner) %>%
  group_modify(~ {
    periods <- .x
    game_data %>%
      filter(gametype == "REG") %>%
      inner_join(periods %>% select(TEAM, start_date, end_date), by = "TEAM") %>%
      filter(DATE >= start_date & DATE <= end_date) %>%
      mutate(
        yr     = as.integer(format(DATE, "%Y")),
        mo     = as.integer(format(DATE, "%m")),
        season = if_else(mo >= 6L,
          paste0(sprintf("%02d", yr %% 100L), "-", sprintf("%02d", (yr + 1L) %% 100L)),
          paste0(sprintf("%02d", (yr - 1L) %% 100L), "-", sprintf("%02d", yr %% 100L))
        )
      ) %>%
      group_by(season) %>%
      summarize(w = sum(WL == "W"), l = sum(WL == "L"), .groups = "drop") %>%
      filter(w + l > 0L)
  }) %>%
  ungroup() %>%
  mutate(pct = w / (w + l))

best_reg_season <- reg_season_wl %>%
  group_by(owner) %>%
  slice_max(pct, n = 1, with_ties = FALSE) %>%
  transmute(owner, best_reg_season = paste0(w, "-", l), best_reg_pct = pct) %>%
  ungroup()

worst_reg_season <- reg_season_wl %>%
  group_by(owner) %>%
  slice_min(pct, n = 1, with_ties = FALSE) %>%
  transmute(owner, worst_reg_season = paste0(w, "-", l), worst_reg_pct = pct) %>%
  ungroup()

season_meta <- get_owners() %>%
  left_join(
    get_playoff_seeds() %>%
      mutate(SEASON = str_remove(SEASON, " Playoffs")) %>%
      select(SEASON, TEAM) %>%
      distinct() %>%
      mutate(made_playoffs = TRUE),
    by = c("SEASON", "TEAM")
  ) %>%
  replace_na(list(made_playoffs = FALSE)) %>%
  group_by(OWNER) %>%
  summarize(
    seasons = n_distinct(SEASON),
    .groups = "drop"
  )

completed_seasons <- get_champion_list() %>%
  mutate(SEASON = str_remove(SEASON, " Playoffs")) %>%
  pull(SEASON)

champion_teams <- get_champion_list() %>%
  mutate(season = str_remove(SEASON, " Playoffs")) %>%
  select(season, TEAM)

team_playoff_wins <- game_data %>%
  filter(gametype == "PLAYOFF") %>%
  mutate(season = str_remove(SEASON, " Playoffs")) %>%
  filter(season %in% completed_seasons) %>%
  group_by(TEAM, season) %>%
  summarize(po_wins = sum(WL == "W"), .groups = "drop")

playoff_depth <- get_owners() %>%
  rename(season = SEASON) %>%
  filter(season %in% completed_seasons) %>%
  inner_join(
    get_playoff_seeds() %>%
      mutate(season = str_remove(SEASON, " Playoffs")) %>%
      select(season, TEAM),
    by = c("season", "TEAM")
  ) %>%
  left_join(team_playoff_wins, by = c("TEAM", "season")) %>%
  mutate(po_wins = replace_na(po_wins, 0L)) %>%
  left_join(champion_teams %>% mutate(is_champion = TRUE), by = c("TEAM", "season")) %>%
  mutate(is_champion = replace_na(is_champion, FALSE)) %>%
  group_by(OWNER) %>%
  summarize(
    po_r2          = sum(po_wins >= 4L),
    po_conf_finals = sum(po_wins >= 8L),
    po_finals      = sum(po_wins >= 12L),
    championships  = sum(is_champion),
    .groups = "drop"
  )

owner_stats <- wl_stats %>%
  rename(OWNER = owner) %>%
  left_join(season_meta,      by = "OWNER") %>%
  left_join(playoff_depth,    by = "OWNER") %>%
  left_join(best_reg_season,  by = c("OWNER" = "owner")) %>%
  left_join(worst_reg_season, by = c("OWNER" = "owner")) %>%
  left_join(owner_ratings,    by = c("OWNER" = "owner")) %>%
  rename(owner = OWNER) %>%
  mutate(
    total_w     = reg_w + playoff_w,
    total_l     = reg_l + playoff_l,
    reg_pct     = round(reg_w / (reg_w + reg_l), 3),
    playoff_pct = if_else(playoff_w + playoff_l > 0, round(playoff_w / (playoff_w + playoff_l), 3), NA_real_),
    total_pct   = round(total_w / (total_w + total_l), 3),
    across(c(po_r2, po_conf_finals, po_finals, championships), ~ replace_na(.x, 0L))
  ) %>%
  select(owner, teams, seasons, best_reg_season, best_reg_pct, worst_reg_season, worst_reg_pct,
         reg_w, reg_l, reg_pct, playoff_w, playoff_l, playoff_pct,
         total_w, total_l, total_pct, playoff_appearances,
         po_r2, po_conf_finals, po_finals, championships,
         off_rtg, def_rtg) %>%
  arrange(desc(total_pct), desc(total_w))

write_csv(owner_stats, file.path(DATA_DIR, "owner_stats.csv"))
inform(" * DONE")

start_time <- Sys.time()
inform("Calculating league stats....")

dfs_all %>%
  filter(pmax(P, R, A, S, B) >= 5) %>%
  select(PLAYER, SEASON, DATE, OPP, P, R, A, S, B, FGM, FGA, `3PM`, `3PA`, TO, PF) %>%
  write_rds(file.path(DATA_DIR, "game_high_player.rds"), compress = "xz")

dfs_all %>%
  group_by(PLAYER, SEASON) %>%
  summarize(across(
    c(M, P, R, A, S, B, `3PM`, TO, PF, TD),
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>%
  write_rds(file.path(DATA_DIR, "season_high_player.rds"), compress = "xz")

dfs_all %>%
  group_by(TEAM, SEASON, DATE, OPP) %>%
  mutate(DIFF = (TEAM_PTS - OPP_TEAM_PTS) / n()) %>%
  summarize(across(
    c(DIFF, P, R, A, S, B, `3PM`, TO, PF),
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>%
  write_rds(file.path(DATA_DIR, "game_high_team.rds"), compress = "xz")

x <- dfs_all %>%
  distinct(TEAM, SEASON, TEAM_PTS, OPP_TEAM_PTS, DATE) %>%
  mutate(
    W = if_else(TEAM_PTS > OPP_TEAM_PTS, 1, 0),
    L = if_else(TEAM_PTS < OPP_TEAM_PTS, 1, 0)
  ) %>%
  group_by(TEAM, SEASON) %>%
  summarize(
    W = sum(W),
    L = sum(L),
    TEAM_PTS = sum(TEAM_PTS),
    OPP_TEAM_PTS = sum(OPP_TEAM_PTS),
    .groups = "drop"
  ) %>%
  mutate(
    RECORD = str_c(W, "-", L),
    PCT = round(W / (W + L), 3)
  )
dfs_all %>%
  group_by(TEAM, SEASON) %>%
  summarize(across(
    c(P, R, A, S, B, `3PM`, TO, PF, TD),
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>%
  left_join(x, by = c("TEAM", "SEASON")) %>%
  mutate(DIFF = TEAM_PTS - OPP_TEAM_PTS,
         MOV = round(DIFF / (W + L), 2)) %>%
  select(-TEAM_PTS, -OPP_TEAM_PTS) %>%
  write_rds(file.path(DATA_DIR, "season_high_team.rds"), compress = "xz")

get_win_streaks(dfs_all) %>%
  select(-streak_group) %>%
  filter(streak >= 10) %>%
  arrange(desc(streak)) %>%
  write_rds(file.path(DATA_DIR, "wl_streaks.rds"), compress = "xz")

inform("Pre-computing franchise cumulative point differential....")
cum_diff_games <- dfs_all %>%
  mutate(OPP_RAW = str_replace(OPP, "@", "")) %>%
  group_by(SEASON, TEAM, OPP, OPP_RAW, DATE) %>%
  summarize(P = sum(P), .groups = "drop")

cum_diff_games %>%
  inner_join(
    cum_diff_games %>% select(OPP_RAW = TEAM, DATE, OPP_P = P),
    by = c("OPP_RAW", "DATE")
  ) %>%
  mutate(DIFF = P - OPP_P) %>%
  group_by(TEAM) %>%
  arrange(DATE, .by_group = TRUE) %>%
  mutate(CUM_DIFF = cumsum(DIFF), G = row_number()) %>%
  ungroup() %>%
  select(TEAM, SEASON, DATE, DIFF, CUM_DIFF, G) %>%
  write_rds(file.path(DATA_DIR, "cum_diff.rds"), compress = "xz")
inform(" * DONE")

inform("Pre-computing playoff top performers....")
dfs_playoffs %>%
  group_by(SEASON, ROUND, GAME, DATE, TEAM, PLAYER) %>%
  summarize(GMSC = sum(GMSC), P = sum(P), R = sum(R), A = sum(A), .groups = "drop") %>%
  group_by(SEASON, ROUND, GAME, DATE, TEAM) %>%
  arrange(desc(GMSC), .by_group = TRUE) %>%
  slice_head(n = 3) %>%
  ungroup() %>%
  write_rds(file.path(DATA_DIR, "playoff_top_performers.rds"), compress = "xz")
inform(" * DONE")

inform("Updating standings.csv....")

x <- dfs %>%
  filter(SEASON == max(SEASON)) %>%
  group_by(DATE, TEAM, OPP) %>%
  summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
  ungroup() %>%
  mutate(OPP = str_replace(OPP, "@", ""))

games <- x %>%
  left_join(
    x %>% select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
    by = c("DATE", "OPP")
  ) %>%
  mutate(
    WIN  = TEAM_PTS > OPP_PTS,
    LOSS = TEAM_PTS < OPP_PTS,
    CONF = get_conference(TEAM),
    DIV  = get_division(TEAM),
    OPP_CONF = get_conference(OPP),
    OPP_DIV  = get_division(OPP)
  )

standings <- games %>%
  group_by(TEAM) %>%
  summarize(
    W = sum(WIN),
    L = sum(LOSS),
    CONF = first(CONF),
    DIV  = first(DIV),
    CONF_W = sum(WIN & CONF == OPP_CONF),
    CONF_L = sum(LOSS & CONF == OPP_CONF),
    DIV_W  = sum(WIN & DIV == OPP_DIV),
    DIV_L  = sum(LOSS & DIV == OPP_DIV),
    PPG = mean(TEAM_PTS),
    OPPG = mean(OPP_PTS),
    .groups = "drop"
  ) %>%
  mutate(
    PCT = W / (W + L),
    CONF_PCT = CONF_W / (CONF_W + CONF_L),
    DIV_PCT  = DIV_W / (DIV_W + DIV_L),
    DIFF = PPG - OPPG
  )

h2h <- games %>%
  group_by(TEAM, OPP) %>%
  summarize(
    W = sum(WIN),
    L = sum(LOSS),
    PCT = W / (W + L),
    .groups = "drop"
  )

division_winners <- standings %>%
  group_by(DIV) %>%
  arrange(desc(PCT), desc(CONF_PCT), desc(DIFF)) %>%
  slice(1) %>%
  mutate(DIV_WINNER = TRUE) %>%
  select(TEAM, DIV_WINNER)

standings <- standings %>%
  left_join(division_winners, by = "TEAM") %>%
  mutate(DIV_WINNER = if_else(is.na(DIV_WINNER), FALSE, DIV_WINNER))

resolve_nba_ties <- function(df, h2h) {

  if (nrow(df) == 1) return(df)

  teams <- df$TEAM

  h2h_tied <- h2h %>%
    filter(TEAM %in% teams, OPP %in% teams) %>%
    group_by(TEAM) %>%
    summarize(H2H_PCT = mean(PCT, na.rm = TRUE), .groups = "drop")

  df2 <- df %>%
    left_join(h2h_tied, by = "TEAM") %>%
    mutate(H2H_PCT = replace_na(H2H_PCT, 0))

  ordering <- df2 %>%
    arrange(
      desc(H2H_PCT),
      desc(DIV_WINNER),
      desc(DIV_PCT),
      desc(CONF_PCT),
      desc(DIFF)
    )

  top <- ordering[1, ]

  tied <- ordering %>%
    filter(
      H2H_PCT == top$H2H_PCT,
      DIV_WINNER == top$DIV_WINNER,
      DIV_PCT == top$DIV_PCT,
      CONF_PCT == top$CONF_PCT,
      DIFF == top$DIFF
    )

  result <-
    if (nrow(tied) == nrow(ordering)) {
      ordering
    } else {
      bind_rows(
        top,
        resolve_nba_ties(
          ordering[-1, ] %>% select(-H2H_PCT),
          h2h
        )
      )
    }

  result %>% select(-H2H_PCT)
}

final_standings <- standings %>%
  group_by(CONF, W, L) %>%
  group_modify(~ resolve_nba_ties(.x, h2h)) %>%
  ungroup() %>%
  group_by(CONF) %>%
  arrange(CONF, desc(W), .by_group = TRUE) %>%
  mutate(
    GB = (max(W - L) - (W - L))/2,
    SEED = row_number()
  ) %>%
  ungroup()

final_standings %>%
  transmute(
    SEED = paste0(CONF, "-", SEED),
    TEAM, GB, W, L,
    PCT = round(PCT, 3),
    PPG = round(PPG, 1),
    OPPG = round(OPPG, 1),
    DIFF = round(DIFF, 1)
  ) %>%
  write_csv(file.path(DATA_DIR, "standings.csv"))

dfs_all %>%
  write_csv(file.path(DATA_DIR, "allstats.csv"))

inform("Writing league history CSV....")
write_league_history(dfs, team_ratings, DATA_DIR)
inform(" * DONE")

inform("Writing per-team profile CSVs....")
write_team_profiles(dfs, dfs_playoffs, standings_list, team_ratings, DATA_DIR)
inform(" * DONE")

inform("Writing roster and picks CSVs....")
write_roster_picks(season, sort(unique(dfs$TEAM)), DATA_DIR)
inform(" * DONE")

inform("Writing head-to-head matrix CSVs....")
write_h2h_matrix(dfs, dfs_playoffs, DATA_DIR)
write_owner_h2h_matrix(dfs, dfs_playoffs, owner_data, DATA_DIR)
inform(" * DONE")

inform("Writing player seasons CSV....")
bio_data <- read_csv(file.path(DATA_DIR, "player-bio-database.csv"),
                     skip = 1, show_col_types = FALSE, name_repair = "minimal") %>%
  select(
    NAME_KEY   = 1,
    PHOTO_URL  = `Img URL`,
    DOB        = DOB,
    COLLEGE    = COLLEGE,
    COUNTRY    = COUNTRY,
    NBN_DFT_YR = `NBN D YR`,
    NBN_DFT_R  = `NBN D R`,
    NBN_DFT_P  = `NBN D P`
  ) %>%
  mutate(NAME_KEY = toupper(NAME_KEY)) %>%
  filter(!is.na(NAME_KEY), NAME_KEY != "") %>%
  distinct(NAME_KEY, .keep_all = TRUE)

player_seasons <- dfs %>%
  group_by(PLAYER, SEASON, TEAM) %>%
  summarize(
    G         = n(),
    MIN       = sum(M,     na.rm = TRUE),
    PTS       = sum(P,     na.rm = TRUE),
    REB       = sum(R,     na.rm = TRUE),
    AST       = sum(A,     na.rm = TRUE),
    STL       = sum(S,     na.rm = TRUE),
    BLK       = sum(B,     na.rm = TRUE),
    TOV       = sum(TO,    na.rm = TRUE),
    PF        = sum(PF,    na.rm = TRUE),
    FGM       = sum(FGM,   na.rm = TRUE),
    FGA       = sum(FGA,   na.rm = TRUE),
    HIGH_P    = max(P,     na.rm = TRUE),
    HIGH_R    = max(R,     na.rm = TRUE),
    HIGH_A    = max(A,     na.rm = TRUE),
    HIGH_S    = max(S,     na.rm = TRUE),
    HIGH_B    = max(B,     na.rm = TRUE),
    HIGH_3PM  = max(`3PM`, na.rm = TRUE),
    HIGH_GMSC = max(GMSC,  na.rm = TRUE),
    `3PM`     = sum(`3PM`, na.rm = TRUE),
    `3PA`     = sum(`3PA`, na.rm = TRUE),
    FTM       = sum(FTM,   na.rm = TRUE),
    FTA       = sum(FTA,   na.rm = TRUE),
    GMSC      = sum(GMSC,  na.rm = TRUE),
    LAST_DATE = max(as.Date(DATE), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(bio_data, by = c("PLAYER" = "NAME_KEY")) %>%
  left_join(
    get_champions(dfs_playoffs) %>%
      group_by(PLAYER) %>%
      summarize(RINGS = n_distinct(SEASON), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  mutate(
    RINGS  = replace_na(as.integer(RINGS), 0L),
    PLAYER = tools::toTitleCase(tolower(PLAYER)),
    SLUG   = gsub("[^a-z0-9-]", "", gsub(" ", "-", gsub(", ", "-", tolower(PLAYER))))
  ) %>%
  arrange(PLAYER, SEASON, LAST_DATE)
write_csv(player_seasons, file.path(DATA_DIR, "player_seasons.csv"))
inform(" * DONE")

inform("Writing player seasons playoffs CSV....")
player_seasons_playoffs <- dfs_playoffs %>%
  group_by(PLAYER, SEASON, TEAM) %>%
  summarize(
    G         = n(),
    MIN       = sum(M,     na.rm = TRUE),
    PTS       = sum(P,     na.rm = TRUE),
    REB       = sum(R,     na.rm = TRUE),
    AST       = sum(A,     na.rm = TRUE),
    STL       = sum(S,     na.rm = TRUE),
    BLK       = sum(B,     na.rm = TRUE),
    TOV       = sum(TO,    na.rm = TRUE),
    PF        = sum(PF,    na.rm = TRUE),
    FGM       = sum(FGM,   na.rm = TRUE),
    FGA       = sum(FGA,   na.rm = TRUE),
    HIGH_P    = max(P,     na.rm = TRUE),
    HIGH_R    = max(R,     na.rm = TRUE),
    HIGH_A    = max(A,     na.rm = TRUE),
    HIGH_S    = max(S,     na.rm = TRUE),
    HIGH_B    = max(B,     na.rm = TRUE),
    HIGH_3PM  = max(`3PM`, na.rm = TRUE),
    HIGH_GMSC = max(GMSC,  na.rm = TRUE),
    `3PM`     = sum(`3PM`, na.rm = TRUE),
    `3PA`     = sum(`3PA`, na.rm = TRUE),
    FTM       = sum(FTM,   na.rm = TRUE),
    FTA       = sum(FTA,   na.rm = TRUE),
    GMSC      = sum(GMSC,  na.rm = TRUE),
    LAST_DATE = max(as.Date(DATE), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(bio_data, by = c("PLAYER" = "NAME_KEY")) %>%
  mutate(
    PLAYER = tools::toTitleCase(tolower(PLAYER)),
    SLUG   = gsub("[^a-z0-9-]", "", gsub(" ", "-", gsub(", ", "-", tolower(PLAYER))))
  ) %>%
  arrange(PLAYER, SEASON, LAST_DATE)
write_csv(player_seasons_playoffs, file.path(DATA_DIR, "player_seasons_playoffs.csv"))
inform(" * DONE")

inform("Writing player awards CSV....")
bind_rows(
  get_all_player_awards(),
  get_champions(dfs_playoffs) %>%
    distinct(PLAYER, SEASON) %>%
    mutate(AWARD = "Champion")
) %>%
  mutate(
    PLAYER = tools::toTitleCase(tolower(PLAYER)),
    SLUG   = gsub("[^a-z0-9-]", "", gsub(" ", "-", gsub(", ", "-", tolower(PLAYER))))
  ) %>%
  select(SLUG, PLAYER, SEASON, AWARD) %>%
  write_csv(file.path(DATA_DIR, "player_awards.csv"))
inform(" * DONE")

inform("Writing career stat totals CSVs....")
career_totals <- dfs %>%
  group_by(PLAYER) %>%
  summarize(
    P     = sum(P,     na.rm = TRUE),
    R     = sum(R,     na.rm = TRUE),
    A     = sum(A,     na.rm = TRUE),
    S     = sum(S,     na.rm = TRUE),
    B     = sum(B,     na.rm = TRUE),
    `3PM` = sum(`3PM`, na.rm = TRUE),
    .groups = "drop"
  )

list(
  "totals-p"   = "P",
  "totals-r"   = "R",
  "totals-a"   = "A",
  "totals-s"   = "S",
  "totals-b"   = "B",
  "totals-3pm" = "3PM"
) %>%
  iwalk(function(col, name) {
    career_totals %>%
      arrange(desc(.data[[col]])) %>%
      slice_head(n = 250) %>%
      mutate(RANK = row_number()) %>%
      select(RANK, PLAYER, all_of(col)) %>%
      write_csv(file.path(DATA_DIR, glue("{name}.csv")))
  })
inform(" * DONE")

inform("Writing game high CSVs....")
game_highs_base <- dfs_all %>%
  select(DATE, SEASON, PLAYER, TEAM, OPP, gametype, P, R, A, S, B, `3PM`)

list(
  "game-highs-p"   = "P",
  "game-highs-r"   = "R",
  "game-highs-a"   = "A",
  "game-highs-s"   = "S",
  "game-highs-b"   = "B",
  "game-highs-3pm" = "3PM"
) %>%
  iwalk(function(col, name) {
    game_highs_base %>%
      arrange(desc(.data[[col]]), DATE) %>%
      slice_head(n = 20) %>%
      mutate(RANK = row_number()) %>%
      select(RANK, DATE, SEASON, PLAYER, TEAM, OPP, gametype, P, R, A, S, B, `3PM`) %>%
      write_csv(file.path(DATA_DIR, glue("{name}.csv")))
  })
inform(" * DONE")

inform("Writing playoff classics CSV....")
dfs_playoffs %>%
  filter(WL == "W") %>%
  group_by(PLAYER, SEASON, DATE, TEAM, OPP, ROUND, GAME) %>%
  summarize(
    P     = sum(P,     na.rm = TRUE),
    R     = sum(R,     na.rm = TRUE),
    A     = sum(A,     na.rm = TRUE),
    S     = sum(S,     na.rm = TRUE),
    B     = sum(B,     na.rm = TRUE),
    `3PM` = sum(`3PM`, na.rm = TRUE),
    FGM   = sum(FGM,   na.rm = TRUE),
    FGA   = sum(FGA,   na.rm = TRUE),
    GMSC  = sum(GMSC,  na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(GMSC)) %>%
  slice_head(n = 10) %>%
  mutate(
    RANK   = row_number(),
    PLAYER = tools::toTitleCase(tolower(PLAYER)),
    OPP    = str_replace(OPP, "@", "")
  ) %>%
  select(RANK, SEASON, DATE, PLAYER, TEAM, OPP, ROUND, GAME, P, R, A, S, B, `3PM`, FGM, FGA, GMSC) %>%
  write_csv(file.path(DATA_DIR, "playoff-classics.csv"))
inform(" * DONE")

inform("Writing hof.csv....")
hof_csv <- dfs_all %>%
  mutate(
    G = 1,
    GMSC_WGT_WL = case_when(WL == "W" ~ 1.25, TRUE ~ 0.75),
    GMSC_WGT_GAMETYPE = case_when(
      ROUND == 1 ~ 2, ROUND == 2 ~ 4, ROUND == 3 ~ 8, ROUND == 4 ~ 16, TRUE ~ 1
    )
  ) %>%
  group_by(SEASON, TEAM, ROUND) %>%
  mutate(GMSC_WGT_ROUNDLEN = case_when(
    GMSC_WGT_GAMETYPE == 1 ~ 1, TRUE ~ 5.5 / n_distinct(DATE)
  )) %>%
  ungroup() %>%
  mutate(GMSC_WEIGHTED = GMSC * GMSC_WGT_WL * GMSC_WGT_GAMETYPE * GMSC_WGT_ROUNDLEN) %>%
  group_by(PLAYER) %>%
  summarize_at(vars(c("G", "M", "P", "R", "A", "S", "B", "GMSC_WEIGHTED")), sum) %>%

  left_join(
    get_champions(dfs_playoffs) %>%
      group_by(PLAYER) %>% summarize(RINGS = n_distinct(SEASON), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    dfs_playoffs %>%
      group_by(PLAYER) %>% summarize(PLAYOFF_APPS = n_distinct(SEASON), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    dfs %>%
      distinct(PLAYER, SEASON) %>%
      mutate(ACTIVE = as.integer(SEASON == max(SEASON))) %>%
      group_by(PLAYER) %>% summarize(ACTIVE = max(ACTIVE), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    dfs %>%
      distinct(PLAYER, TEAM) %>%
      group_by(PLAYER) %>%
      summarize(TEAMS = str_c(sort(TEAM), collapse = ","), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_allnbn1() %>% group_by(PLAYER) %>% summarize(ALL_NBN_1 = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_allnbn2() %>% group_by(PLAYER) %>% summarize(ALL_NBN_2 = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_allnbn3() %>% group_by(PLAYER) %>% summarize(ALL_NBN_3 = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_allstars() %>% group_by(PLAYER) %>% summarize(ALLSTARS = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_mvp() %>% group_by(PLAYER) %>% summarize(MVP = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_dpoy() %>% group_by(PLAYER) %>% summarize(DPOY = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_alldef() %>% group_by(PLAYER) %>% summarize(ALL_DEF = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_6moy() %>% group_by(PLAYER) %>% summarize(SIX_MOY = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_roy() %>% group_by(PLAYER) %>% summarize(ROY = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  left_join(
    get_mip() %>% group_by(PLAYER) %>% summarize(MIP = n(), .groups = "drop"),
    by = "PLAYER"
  ) %>%
  mutate(
    RINGS        = replace_na(RINGS, 0L),
    PLAYOFF_APPS = replace_na(PLAYOFF_APPS, 0L),
    ALLSTARS     = replace_na(ALLSTARS, 0L),
    ALL_NBN_1    = replace_na(ALL_NBN_1, 0L),
    ALL_NBN_2    = replace_na(ALL_NBN_2, 0L),
    ALL_NBN_3    = replace_na(ALL_NBN_3, 0L),
    MVP          = replace_na(MVP, 0L),
    DPOY         = replace_na(DPOY, 0L),
    ALL_DEF      = replace_na(ALL_DEF, 0L),
    SIX_MOY      = replace_na(SIX_MOY, 0L),
    ROY          = replace_na(ROY, 0L),
    MIP          = replace_na(MIP, 0L),
    HOF_POINTS   = round(
      GMSC_WEIGHTED / 100 +
        RINGS        * 10 +
        PLAYOFF_APPS *  1 +
        MVP          *  8 +
        DPOY         *  5 +
        ALLSTARS     *  3 +
        ALL_NBN_1    *  4 +
        ALL_NBN_2    *  3 +
        ALL_NBN_3    *  2 +
        ALL_DEF      *  2 +
        SIX_MOY      *  3 +
        ROY          *  3 +
        MIP          *  2,
      1
    )
  ) %>%
  arrange(desc(HOF_POINTS)) %>%
  slice_head(n = 250) %>%
  select(PLAYER, TEAMS, HOF_POINTS, RINGS, PLAYOFF_APPS, ALLSTARS,
         ALL_NBN_1, ALL_NBN_2, ALL_NBN_3, MVP, DPOY, ALL_DEF,
         SIX_MOY, ROY, MIP,
         G, M, P, R, A, S, B, ACTIVE)

write_csv(hof_csv, file.path(DATA_DIR, "hof.csv"))
inform(" * DONE")

inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))
