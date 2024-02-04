if (stringr::str_detect(getwd(), 'bshk9')) {
  setwd("C:/Users/bshk9/OneDrive/home/projects/nbn/nothing-but-stats/app")
} else {
  setwd("C:/Users/Brian/OneDrive/home/projects/nbn/nothing-but-stats/app")
}

library(tidyverse)
library(lubridate)
# Source functions ----
source("update.R")

# Globals ----
myseason <- "2023-24"
myplayoffdate <- "2024-04-11" # Playoff start date

# Refresh stats ----
# Pull down and check allstats, extracting data from Google sheets
allstats <- get_allstats(delete_before = "2023-07-01")

mytestdate <- "2024-01-28"

allstats %>% filter(DATE == mytestdate) %>% 
  distinct(PLAYER) %>% 
  anti_join(allstats %>% filter(DATE < mytestdate) %>% distinct(PLAYER))

allstats %>% 
  distinct(DATE, TEAM, OPP) %>% 
  filter(str_detect(OPP, '^@')) %>% 
  group_by(DATE) %>% 
  mutate(n_games = n()) %>% 
  arrange(DATE) %>% 
  View

# Cleaning and building
built_allstats <- build_allstats(allstats %>% filter(DATE <= "2024-01-28"))

# Write/update output file
built_allstats %>% 
  filter(DATE < myplayoffdate) %>% 
  mutate(gametype = "REG") %>% 
  write_csv(glue::glue("data/allstats-{str_extract(myseason, '\\\\d{2}-\\\\d{2}')}.csv"))

# Playoffs
built_allstats %>% 
  filter(DATE >= myplayoffdate) %>% 
  add_playoff_info() %>% 
  mutate(gametype = "PLAYOFF") %>% 
  write_csv(glue::glue("data/allstats-playoffs-{str_extract(myseason, '\\\\d{2}$')}.csv"))

# Post-processing of data


# Afterwards, re-build the Shiny app




# Refresh player bios ----
googledrive::drive_download(
  "https://docs.google.com/spreadsheets/d/1i3vuaoJOxKrynLZsjV8VGv3eslunMPbrGsrax-xCyVk/", 
  "R/player-bio-database.csv", "csv", overwrite = TRUE
)

# Refresh rosters ----
googlesheets4::read_sheet(
  "https://docs.google.com/spreadsheets/d/14Pwrjk4S9cgB1f2Q16S3YgfoHre8gUxjjXde8k5Nn_0/", 
  sheet = 'SAS',
  range = 'A4:J46'
)
  