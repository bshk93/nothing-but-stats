args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  rlang::abort("Three arguments to `refresh` are required.")
}

#setwd("~/nothing-but-stats/app")
setwd("/srv/shiny/nothing-but-stats/app")
source("../refresh-utils.R")
source("../preprocess-utils.R")

# myseason <- "2024-25"
# myplayoffdate <- "2025-04-16" # Playoff start date
# check_start_date <- "2024-11-01" # Date to start doing checks (newly entered data)
# drop_after_date <- "2024-11-03" # Date after which to delete stats (e.g. unfinished days)

myseason <- args[1]
myplayoffdate <- as_date(args[2])
drop_after_date <- as_date(args[3])
inform(glue("The args are {myseason}, {myplayoffdate}, and {drop_after_date}."))

# Pull data from sheets
allstats <- get_allstats(delete_before = "2024-09-01") %>% 
  check_allstats()

if (nrow(allstats$errors$games) > 0) {
  warn(glue("Found {nrow(allstats$errors$games)} games with errors that will be dropped."))
  allstats$data <- allstats$data %>% 
    anti_join(allstats$errors$games, by = c("TEAM", "DATE"))
}

built_allstats <- build_allstats(allstats$data %>% filter(DATE <= drop_after_date))

# Write/update output file
built_allstats %>% 
  filter(DATE < myplayoffdate) %>% 
  mutate(gametype = "REG") %>% 
  write_csv(glue::glue("data/allstats-{str_extract(myseason, '\\\\d{2}-\\\\d{2}')}.csv"))

# Playoffs, if applicable
maybe <- built_allstats %>% 
  filter(DATE >= myplayoffdate) %>% 
  add_playoff_info() %>% 
  mutate(gametype = "PLAYOFF")

if (nrow(maybe) > 0) {
  inform("Some of these are playoff stats. Exporting.")
  maybe %>% 
    write_csv(glue::glue("data/allstats-playoffs-{str_extract(myseason, '\\\\d{2}$')}.csv"))
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

write_rds(dfs, 'data/dfs.rds')
write_rds(get_newsfeed(dfs), 'data/news.rds')

write_rds(dfs_playoffs, 'data/dfs_playoffs.rds')
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
