source("refresh-utils.R")

# Pull data from sheets
allstats <- get_allstats(delete_before = "2024-09-01") %>% 
  check_allstats()

if (nrow(allstats$errors$games) > 0) {
  warn(glue("Found {nrow(allstats$errors$games)} games with errors that will be dropped."))
  allstats$data <- allstats$data %>% 
    anti_join(allstats$errors$games, by = c("TEAM", "DATE"))
}

built_allstats <- build_allstats(allstats$data)

con = dbConnect(
  Postgres(),
  host = "localhost",
  port = 5432,
  dbname = "nbn",
  user = "postgres",
  password = "mylittl3Farter"
)

# For obs that exist, update values

# Build and pre-process 


# Update SQL database
