setwd("~/nothing-but-stats/app")

# Source functions ----
source("update.R")
source("R/read.R")
source("R/utils.R")
source("R/news.R")

# Globals ----
myseason <- "2024-25"
myplayoffdate <- "2025-04-16" # Playoff start date
check_start_date <- "2024-11-19" # Date to start doing checks (newly entered data)
drop_after_date <- "2024-11-22" # Date after which to delete stats (e.g. unfinished days)

# Refresh stats ----

# Pull down and check allstats, extracting data from Google sheets
# Also runs some simple tests
allstats <- get_allstats(delete_before = "2024-09-01")

# Additional Testing
test_allstats(
  allstats,
  check_start_date
)

# Check games
allstats %>% 
  distinct(DATE, TEAM, OPP) %>% 
  filter(str_detect(OPP, '^@')) %>% 
  group_by(DATE) %>% 
  mutate(n_games = n()) %>% 
  arrange(desc(DATE)) %>% 
  View

# Cleaning and building
built_allstats <- build_allstats(allstats %>% filter(DATE <= drop_after_date))

# Write/update output file
built_allstats %>% 
  filter(DATE < myplayoffdate) %>% 
  mutate(gametype = "REG") %>% 
  write_csv(glue::glue("data/allstats-{str_extract(myseason, '\\\\d{2}-\\\\d{2}')}.csv"))

# Playoffs, if applicable
built_allstats %>% 
  filter(DATE >= myplayoffdate) %>% 
  add_playoff_info() %>% 
  mutate(gametype = "PLAYOFF") %>% 
  write_csv(glue::glue("data/allstats-playoffs-{str_extract(myseason, '\\\\d{2}$')}.csv"))

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

write_rds(dfs, 'data/dfs.rds')
write_rds(get_newsfeed(dfs), 'data/news.rds')

write_rds(dfs_playoffs, 'data/dfs_playoffs.rds')

start_time <- Sys.time()
inform("Calculating achievements....")
ach_metadata <- read_csv("data/metadata-achievements.csv", show_col_types = FALSE)

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

write_rds(ach_season, 'data/ach_season.rds')

inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))

start_time <- Sys.time()
inform("Calculate league stats....")
# game highs
dfs_everything %>% 
  filter(P + R + A + S + B >= 20) %>% 
  select(PLAYER, SEASON, DATE, OPP, P, R, A, S, B, `3PM`, TO, PF) %>% 
  write_rds("data/game_high_player.rds")

# season highs
dfs_everything %>% 
  group_by(PLAYER, SEASON) %>% 
  summarize(across(
    c(M, P, R, A, S, B, `3PM`, TO, PF, TD), 
    sum, 
    .names = "{.col}"
  ), .groups = "drop") %>% 
  write_rds("data/season_high_player.rds")

# team game highs
dfs_everything %>% 
  group_by(TEAM, SEASON, DATE, OPP) %>% 
  summarize(across(
    c(P, R, A, S, B, `3PM`, TO, PF), 
    sum,
    .names = "{.col}"
  ), .groups = "drop") %>% 
  write_rds("data/game_high_team.rds")

# team season highs
dfs_everything %>% 
  group_by(TEAM, SEASON) %>% 
  summarize(across(
    c(P, R, A, S, B, `3PM`, TO, PF, TD), 
    sum, 
    .names = "{.col}"
  ), .groups = "drop") %>% 
  write_rds("data/season_high_team.rds")

inform(glue(" * DONE [{round(Sys.time() - start_time, 1)}s]"))


# STOP HERE ----
# Afterwards, re-build the Shiny app




# Refresh player bios ----
googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1i3vuaoJOxKrynLZsjV8VGv3eslunMPbrGsrax-xCyVk/", 
  "data/player-bio-database.csv", "csv", overwrite = TRUE
)

read_csv("data/player-bio-database.csv", skip = 1, show_col_types = F) %>% 
  select(-Name...1) %>% 
  rename(Name = Name...2) %>% 
  mutate(Name = case_when(
    Name == "KANTER, ENES" ~ "FREEDOM, ENES",
    Name == "THOMAS, CAMERON" ~ "THOMAS, CAM",
    Name == "REDDISH, CAMERON" ~ "REDDISH, CAM",
    TRUE ~ Name
  )) %>% 
  mutate(DOB = mdy(DOB)) %>% 
  write_rds('data/bios.rds')


# Refresh rosters (UNUSED) ----
googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/14Pwrjk4S9cgB1f2Q16S3YgfoHre8gUxjjXde8k5Nn_0/", 
  sheet = 'SAS',
  range = 'A4:J46'
)
  




z2 %>% 
  group_by(TEAM, SEASON) %>% 
  summarize(
    OFF_RTG = mean(DIFF_OFF),
    DEF_RTG = mean(DIFF_DEF),
    TOT_RTG = OFF_RTG + DEF_RTG
  ) %>% mutate(id = str_c(SEASON, '\n', TEAM)) %>% 
  ggplot(aes(x = OFF_RTG, y = DEF_RTG)) +
  geom_point() +
  geom_label(aes(label = id), size = 3) + 
  xlim(-15, 15) + 
  ylim(-15, 15)



# Fix data (example) ----
df_to_fix <- read_csv('data/allstats-22-23.csv')

df_fixed <- df_to_fix %>% 
  mutate(PLAYER = case_when(
    PLAYER == 'LOPEZ, BROOK' & DATE == '2023-01-11' & M == 28 ~ 'PORTER, MICHAEL',
    PLAYER == 'BARTON, WILL' & DATE == '2023-01-06' & M == 35 ~ 'BARNES, HARRISON',
    PLAYER == 'BAZLEY, DARIUS' & DATE == '2023-01-12' & M == 16 ~ 'BRADLEY, TONY',
    TRUE ~ PLAYER
  ))

df_fixed %>% 
  filter(TEAM == 'SAS' & DATE == '2023-01-11')

# make copy of old csv

write_csv(df_fixed, 'data/allstats-22-23.csv')
