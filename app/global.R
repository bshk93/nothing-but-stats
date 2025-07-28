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

walk(list.files("R/", full.names = T), source)

dfs <- read_rds('data/dfs.rds')
dfs_playoffs <- read_rds('data/dfs_playoffs.rds')
dfs_everything <- rbind(dfs, dfs_playoffs)
news <- read_rds('data/news.rds')
bios <- read_rds('data/bios.rds')
team_ratings <- read_rds('data/team_ratings.rds')

champions <- get_champions(dfs_playoffs)

ach_metadata <- read_csv("data/metadata-achievements.csv", show_col_types = FALSE)

my_ranks <- get_ranks(dfs)

game_high_player <- read_rds("data/game_high_player.rds")
season_high_player <- read_rds("data/season_high_player.rds")
game_high_team <- read_rds("data/game_high_team.rds")
season_high_team <- read_rds("data/season_high_team.rds")

player_teams <- dfs_everything %>% 
  arrange(PLAYER, DATE) %>% 
  group_by(PLAYER) %>% 
  mutate(last_played = last(TEAM)) %>% 
  ungroup() %>% 
  mutate(NAME = str_c(PLAYER, ' (', last_played, ')')) %>% 
  distinct(PLAYER, NAME)

named_names <- player_teams$PLAYER %>% 
  set_names(player_teams$NAME)