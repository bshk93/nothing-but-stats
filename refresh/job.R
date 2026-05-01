# args <- c("", "", "")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  rlang::abort("Four arguments to `refresh` are required.")
}

season <- ifelse(length(args) >= 1, args[1], "")
playoff_date <- ifelse(length(args) >= 2, args[2], "")
drop_date <- ifelse(length(args) >= 3, args[3], "")

# Set-Up ----
setwd("~/nothing-but-stats")
source("refresh/refresh-utils.R")
source("refresh/preprocess-utils.R")

DATA_DIR <- Sys.getenv("NBS_DATA_DIR", "/home/skim/nbs-data")

today <- Sys.Date()
current_year <- as.numeric(format(today, "%Y"))
cutoff_date <- as.Date(paste0(current_year, "-09-30"))

# default for season
if (season == "") {
  if (today <= cutoff_date) {
    season <- paste0(substr(current_year - 1, 3, 4), "-", substr(current_year, 3, 4))
  } else {
    season <- paste0(substr(current_year, 3, 4), "-", substr(current_year + 1, 3, 4))
  }
}

# default for drop date
if (drop_date == "") {
  drop_date <- today
  inform(glue("Drop after date defaulted to today ({today})."))
}

# Pull Data from Sheets ----
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

if (nrow(allstats$errors$games %>% filter(DATE <= drop_date)) > 0) {
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

allstats$data <- allstats$data %>% filter(DATE <= drop_date)
inform("No errors detected in Sheets data. Printing number of games detected in last 5 days.")
errchk <- allstats$data %>% 
  distinct(DATE, TEAM, OPP) %>% 
  group_by(DATE) %>% 
  mutate(n_sides = n()) %>% 
  ungroup() %>% 
  arrange(DATE)

print(
  errchk %>% 
    filter(DATE %in% tail(sort(unique(DATE)), 5)),
  n = 999
)

if (nrow(errchk %>% filter(n_sides %% 2 == 1)) > 0) {
  abort("Missing games detected (uneven number of sides on a day).")
}


inform("Building allstats....")
built_allstats <- build_allstats(allstats$data)
season_code <- str_extract(season, "\\d{2}-\\d{2}")

# Slice into regular season and playoff portions
current_reg_raw <- if (playoff_date == "") {
  inform("No playoff_date provided, assuming no playoff data.")
  built_allstats
} else {
  inform(glue("A playoff_date was provided, using {playoff_date} as cutoff to check for playoff stats."))
  built_allstats %>% filter(DATE < as.Date(playoff_date))
}

current_playoff_raw <- if (playoff_date != "") {
  built_allstats %>% filter(DATE >= as.Date(playoff_date)) %>% add_playoff_info()
} else {
  NULL
}

# Write CSVs for archiving and future historical loads
current_reg_raw %>%
  mutate(gametype = "REG") %>%
  write_csv(file.path(DATA_DIR, glue("allstats-{season_code}.csv")))

if (!is.null(current_playoff_raw) && nrow(current_playoff_raw) > 0) {
  inform("Some of these are playoff stats. Exporting.")
  season_suffix <- str_extract(season, "\\d{2}$")
  current_playoff_raw %>%
    mutate(gametype = "PLAYOFF") %>%
    write_csv(file.path(DATA_DIR, glue("allstats-playoffs-{season_suffix}.csv")))
}

# Post-processing: load historical seasons from disk, inject current season from memory
# (avoids re-reading the CSV we just wrote)
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

write_rds(calculate_team_offense_defense(dfs), file.path(DATA_DIR, 'team_ratings.rds'), compress = "xz")

# Pre-compute my_ranks for performance
inform("Calculating player ranks....")
source("app/R/utils.R")
my_ranks <- get_ranks(dfs)
write_rds(my_ranks, file.path(DATA_DIR, 'my_ranks.rds'), compress = "xz")
inform(" * DONE")

# Pre-compute standings and team stats for all seasons
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

# start_time <- Sys.time()
# inform("Parsing roster log....")
# 
# read_delim("app/data/roster-log.txt", delim = ";FARTS;", col_names = c("ts", "text")) %>% 
#   transmute(DATE = as_date(ts), TEXT = toupper(text))
# 
# inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))

start_time <- Sys.time()
inform("Calculating league stats....")
# game highs
dfs_all %>%
  filter(pmax(P, R, A, S, B) >= 5) %>%
  select(PLAYER, SEASON, DATE, OPP, P, R, A, S, B, FGM, FGA, `3PM`, `3PA`, TO, PF) %>%
  write_rds(file.path(DATA_DIR, "game_high_player.rds"), compress = "xz")

# season highs
dfs_all %>%
  group_by(PLAYER, SEASON) %>%
  summarize(across(
    c(M, P, R, A, S, B, `3PM`, TO, PF, TD),
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>%
  write_rds(file.path(DATA_DIR, "season_high_player.rds"), compress = "xz")

# team game highs
dfs_all %>%
  group_by(TEAM, SEASON, DATE, OPP) %>%
  mutate(DIFF = (TEAM_PTS - OPP_TEAM_PTS) / n()) %>%
  summarize(across(
    c(DIFF, P, R, A, S, B, `3PM`, TO, PF),
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>%
  write_rds(file.path(DATA_DIR, "game_high_team.rds"), compress = "xz")

# team season highs
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

# win/loss streaks
get_win_streaks(dfs_all) %>%
  select(-streak_group) %>%
  filter(streak >= 10) %>%
  arrange(desc(streak)) %>%
  write_rds(file.path(DATA_DIR, "wl_streaks.rds"), compress = "xz")

# Franchise cumulative point differential (for franchise_profiles tab)
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

# Top 3 performers per playoff game per team (for head_to_head tab)
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

# # worst seasons (at least 40 games)
# dfs %>%
#   select(PLAYER, SEASON, TEAM, M, P, R, OR, DR, A, S, B, TO, GMSC, FGM, FGA, `3PM`, `3PA`, FTM, FTA, PF, WL) %>%
#   group_by(PLAYER, SEASON) %>%
#   summarize(
#     G = n(),
#     TEAMS = str_c(unique(TEAM), collapse = ", "),
#     foul_outs = sum(PF == 6),
#     win_pct = round(sum(WL == "W")/n(), 3),
#     across(-c(TEAM, TEAMS, foul_outs, WL, win_pct), sum),
#     .groups = "drop"
#   ) %>%
#   filter(M > 1500) %>% 
#   mutate(
#     fg_missed = FGA - FGM,
#     ft_missed = FTA - FTM,
#     pts_missed = 2 * (fg_missed) + (ft_missed),
#     turnovers = TO,
#     possessions_wasted = FGA - FGM + TO - OR - S,
#     GMSC_per_min = GMSC / M
#   ) %>%
#   select(
#     PLAYER, SEASON, TEAMS, G, MP = M,
#     win_pct,
#     GMSC_per_min,
#     fg_missed,
#     ft_missed,
#     pts_missed,
#     turnovers,
#     possessions_wasted,
#     foul_outs
#   ) %>%
#   mutate(
#     pts_missed_per_min = pts_missed / MP,
#     possessions_wasted_per_min = possessions_wasted / MP,
#     foul_outs_per_min = foul_outs / MP
#   )



inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))

# if (toupper(skip_achievements) %in% c("TRUE", "T")) {
#   inform("Skipping achievements.")
# } else {
#   start_time <- Sys.time()
#   inform("Calculating achievements....")
#   
#   ach_metadata <- read_csv("app/data/metadata-achievements.csv", show_col_types = FALSE)
# 
#   ach_game <- dfs %>%
#     nest_by(PLAYER) %>%
#     mutate(ach = list(get_achievements_game(
#       data,
#       ach_metadata
#     ))) %>%
#     select(-data) %>%
#     unnest(ach)
# 
#   write_rds(ach_game, 'app/data/ach_game.rds')
# 
#   ach_season <- dfs %>%
#     nest_by(PLAYER) %>%
#     mutate(ach = list(get_achievements_season(
#       data,
#       dfs,
#       PLAYER,
#       ach_metadata
#     ))) %>%
#     select(-data) %>%
#     unnest(ach) %>%
#     ungroup()
# 
#   write_rds(ach_season, 'app/data/ach_season.rds')
#   
#   inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))
# }


# Update files in /var/www/stats.nbn.today/files/
inform("Updating /files/...")
  
x <- dfs %>% 
  filter(SEASON == max(SEASON)) %>% 
  group_by(DATE, TEAM, OPP) %>%
  summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
  ungroup() %>%
  mutate(OPP = str_replace(OPP, "@", ""))

games <- x %>%
  left_join(
    x %>%
      select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
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
  
  # recursive elimination
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
  
  # 🔑 CRITICAL: drop H2H_PCT before returning
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

# x %>%
#   left_join(
#     x %>%
#       select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
#     by = c('DATE', 'OPP')
#   ) %>%
#   group_by(TEAM) %>%
#   summarize(
#     W = sum(TEAM_PTS > OPP_PTS),
#     L = sum(TEAM_PTS < OPP_PTS),
#     PPG = mean(TEAM_PTS),
#     OPPG = mean(OPP_PTS),
#     .groups = 'drop'
#   ) %>%
#   mutate(PCT = round(W / (W+L), 3),
#          DIFF = round(PPG - OPPG, 1),
#          PPG = round(PPG, 1),
#          OPPG = round(OPPG, 1),
#          CONF = get_conference(TEAM)) %>%
#   group_by(CONF) %>%
#   mutate(GB = (max(W - L) - (W - L))/2) %>%
#   arrange(CONF, GB) %>%
#   mutate(SEED = str_c(CONF, "-", row_number())) %>%
#   ungroup() %>%
#   select(SEED, TEAM, GB, W, L, PCT, PPG, OPPG, DIFF) %>% 
#   write_csv("files/standings.csv")

dfs_all %>%
  write_csv(file.path(DATA_DIR, "allstats.csv"))



# con = dbConnect(
#   Postgres(),
#   host = "localhost",
#   port = 5432,
#   dbname = "nbn",
#   user = "postgres",
#   password = "mylittl3Farter"
# )

# For obs that exist, update values

# Build and pre-process 


# Update SQL database
