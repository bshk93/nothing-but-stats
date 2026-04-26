# Global ----
# Load libraries, data, and define global variables

# Libraries ----
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(rlang)
library(glue)
library(DT)
library(zoo)
library(data.table)
library(ggimage)
library(plotly)
library(reactable)
library(shinyWidgets)
library(bslib)
library(shinydashboard)
library(shinyjs)

# Source utility functions ----
walk(list.files("R/", full.names = T), source)

DATA_DIR <- Sys.getenv("NBS_DATA_DIR", "/home/skim/nbs-data")

# Load data files ----
dfs <- read_rds(file.path(DATA_DIR, 'dfs.rds'))
dfs_playoffs <- read_rds(file.path(DATA_DIR, 'dfs_playoffs.rds'))
news <- read_rds(file.path(DATA_DIR, 'news.rds'))
bios <- read_rds(file.path(DATA_DIR, 'bios.rds'))
team_ratings <- read_rds(file.path(DATA_DIR, 'team_ratings.rds'))

# Helper function to combine dfs and dfs_playoffs on demand (avoids duplication)
get_dfs_everything <- function() {
  bind_rows(dfs, dfs_playoffs)
}

# Derived data ----
champions <- get_champions(dfs_playoffs)
ach_metadata <- read_csv("data/metadata-achievements.csv", show_col_types = FALSE)

# Pre-computed data (with fallbacks for initial setup) ----
# Load pre-computed ranks, or compute if file doesn't exist
if (file.exists(file.path(DATA_DIR, 'my_ranks.rds'))) {
  my_ranks <- read_rds(file.path(DATA_DIR, 'my_ranks.rds'))
} else {
  # Fallback: compute if file doesn't exist (should only happen before first refresh)
  my_ranks <- get_ranks(dfs)
}

# Load pre-computed standings and team stats, or compute if files don't exist
if (file.exists(file.path(DATA_DIR, 'standings.rds')) && file.exists(file.path(DATA_DIR, 'team_stats.rds'))) {
  standings_precomputed <- read_rds(file.path(DATA_DIR, 'standings.rds'))
  team_stats_precomputed <- read_rds(file.path(DATA_DIR, 'team_stats.rds'))
} else {
  # Fallback: compute if files don't exist (should only happen before first refresh)
  standings_precomputed <- list()
  team_stats_precomputed <- list()
  seasons <- sort(unique(dfs$SEASON))
  for (season in seasons) {
    season_df <- dfs %>% filter(SEASON == season)
    standings_precomputed[[season]] <- compute_standings(season_df)
    team_stats_precomputed[[season]] <- compute_team_stats(season_df)
  }
}

# Load pre-computed high stats and streaks
game_high_player <- read_rds(file.path(DATA_DIR, "game_high_player.rds"))
season_high_player <- read_rds(file.path(DATA_DIR, "season_high_player.rds"))
game_high_team <- read_rds(file.path(DATA_DIR, "game_high_team.rds"))
season_high_team <- read_rds(file.path(DATA_DIR, "season_high_team.rds"))
wl_streaks <- read_rds(file.path(DATA_DIR, "wl_streaks.rds"))

# Player name mappings ----
player_teams <- get_dfs_everything() %>% 
  arrange(PLAYER, DATE) %>% 
  group_by(PLAYER) %>% 
  mutate(last_played = last(TEAM)) %>% 
  ungroup() %>% 
  mutate(NAME = str_c(PLAYER, ' (', last_played, ')')) %>% 
  distinct(PLAYER, NAME)

named_names <- player_teams$PLAYER %>% 
  set_names(player_teams$NAME)
