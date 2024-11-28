# args <- c("", "", "")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 4) {
  rlang::abort("Four arguments to `refresh` are required.")
}

season <- ifelse(length(args) >= 1, args[1], "")
playoff_date <- ifelse(length(args) >= 2, args[2], "")
drop_date <- ifelse(length(args) >= 3, args[3], "")
skip_achievements <- ifelse(length(args) >= 4, args[4], "")

# Set-Up ----
setwd("~/nothing-but-stats")
source("refresh/refresh-utils.R")
source("refresh/preprocess-utils.R")

today <- Sys.Date()
current_year <- as.numeric(format(today, "%Y"))
cutoff_date <- as.Date(paste0(current_year, "-08-31"))

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

if (nrow(allstats$errors$games) > 0) {
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

inform("Building allstats....")
built_allstats <- build_allstats(allstats$data %>% filter(DATE <= drop_date))

if (playoff_date == "") {
  inform("No playoff_date provided, assuming no playoff data.")
  # Write/update output file
  built_allstats %>% 
    mutate(gametype = "REG") %>% 
    write_csv(glue::glue("app/data/allstats-{str_extract(season, '\\\\d{2}-\\\\d{2}')}.csv"))
} else {
  inform(glue("A playoff_date was provided, using {playoff_date} as cutoff to check for playoff stats."))
  # Write/update output file
  built_allstats %>% 
    filter(DATE < playoff_date) %>% 
    mutate(gametype = "REG") %>% 
    write_csv(glue::glue("app/data/allstats-{str_extract(season, '\\\\d{2}-\\\\d{2}')}.csv"))
  
  # Playoffs, if applicable
  maybe <- built_allstats %>% 
    filter(DATE >= playoff_date) %>% 
    add_playoff_info() %>% 
    mutate(gametype = "PLAYOFF")
  
  if (nrow(maybe) > 0) {
    inform("Some of these are playoff stats. Exporting.")
    maybe %>% 
      write_csv(glue::glue("app/data/allstats-playoffs-{str_extract(season, '\\\\d{2}$')}.csv"))
  }
}

# Post-processing of data
dfs <- load_allstats() %>%
  clean_allstats() %>% 
  mutate(gametype = 'REG',
         GAME = NA_integer_,
         ROUND = NA_integer_) %>%
  group_by(PLAYER) %>% 
  mutate(ROOKIE = SEASON == min(SEASON)) %>% 
  ungroup()

dfs_playoffs <- load_allstats(playoffs = TRUE) %>%
  clean_allstats() %>% 
  mutate(gametype = 'PLAYOFF',
         ROOKIE = NA)

dfs_everything <- rbind(dfs, dfs_playoffs)

write_rds(dfs, 'app/data/dfs.rds')
write_rds(get_newsfeed(dfs), 'app/data/news.rds')

write_rds(dfs_playoffs, 'app/data/dfs_playoffs.rds')

start_time <- Sys.time()
inform("Calculating league stats....")
# game highs
dfs_everything %>% 
  filter(P + R + A + S + B >= 20) %>% 
  select(PLAYER, SEASON, DATE, OPP, P, R, A, S, B, `3PM`, TO, PF) %>% 
  write_rds("app/data/game_high_player.rds")

# season highs
dfs_everything %>% 
  group_by(PLAYER, SEASON) %>% 
  summarize(across(
    c(M, P, R, A, S, B, `3PM`, TO, PF, TD), 
    sum, 
    .names = "{.col}"
  ), .groups = "drop") %>% 
  write_rds("app/data/season_high_player.rds")

# team game highs
dfs_everything %>% 
  group_by(TEAM, SEASON, DATE, OPP) %>% 
  mutate(DIFF = (TEAM_PTS - OPP_TEAM_PTS) / n()) %>% 
  summarize(across(
    c(DIFF, P, R, A, S, B, `3PM`, TO, PF), 
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>% 
  write_rds("app/data/game_high_team.rds")

# team season highs
dfs_everything %>% 
  group_by(TEAM, SEASON) %>% 
  summarize(across(
    c(P, R, A, S, B, `3PM`, TO, PF, TD, TEAM_PTS, OPP_TEAM_PTS), 
    sum, 
    .names = "{.col}"
  ), .groups = "drop") %>% 
  mutate(DIFF = TEAM_PTS - OPP_TEAM_PTS) %>% 
  select(-TEAM_PTS, -OPP_TEAM_PTS) %>% 
  write_rds("app/data/season_high_team.rds")

inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))

if (toupper(skip_achievements) %in% c("TRUE", "T")) {
  inform("Skipping achievements.")
} else {
  start_time <- Sys.time()
  inform("Calculating achievements....")
  ach_metadata <- read_csv("app/data/metadata-achievements.csv", show_col_types = FALSE)
  
  ach_game <- dfs %>% 
    nest_by(PLAYER) %>% 
    mutate(ach = list(get_achievements_game(
      data,
      ach_metadata
    ))) %>% 
    select(-data) %>% 
    unnest(ach)
  
  write_rds(ach_game, 'app/data/ach_game.rds')
  
  ach_season <- dfs %>% 
    nest_by(PLAYER) %>% 
    mutate(ach = list(get_achievements_season(
      data,
      dfs,
      PLAYER,
      ach_metadata
    ))) %>% 
    select(-data) %>% 
    unnest(ach) %>% 
    ungroup()
  
  write_rds(ach_season, 'app/data/ach_season.rds')
  
  inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))
}



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
