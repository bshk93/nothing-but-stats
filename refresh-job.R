# args <- c("", "", "")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 3) {
  rlang::abort("Three arguments to `refresh` are required.")
}

season <- ifelse(length(args) >= 1, args[1], "")
playoff_date <- ifelse(length(args) >= 2, args[2], "")
drop_date <- ifelse(length(args) >= 3, args[3], "")

# Set-Up ----
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
}


setwd("~/nothing-but-stats/app")
#setwd("/srv/shiny/nothing-but-stats/app")
# source("update.R")
# source("R/read.R")
# source("R/utils.R")
source("../refresh-utils.R")
source("R/news.R")
source("R/ach.R")

# Pull Data from Sheets ----
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

if (nrow(allstats$errors$games) > 0) {
  # warn(glue("Found {nrow(allstats$errors$games)} games with errors that will be dropped."))
  # allstats$data <- allstats$data %>% 
  #   anti_join(allstats$errors$games, by = c("TEAM", "DATE"))
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

built_allstats <- build_allstats(allstats$data %>% filter(DATE <= drop_date))

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
