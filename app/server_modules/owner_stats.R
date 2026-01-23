# Owner Stats Module ----
# All outputs for the Owner Stats tab

output$owner_stats <- renderDT({
  
  tryCatch({
    # Read owner data from CSV
    owner_data <- read_csv(
      "https://docs.google.com/spreadsheets/d/e/2PACX-1vTB86fJGOAig-7Oh4J8D-0Vq0W2n8r63MxY5fuWWMJz-cCZNU5i384I7_iLSOiA057nmtLMnosXCPO3/pub?gid=0&single=true&output=csv",
      show_col_types = FALSE
    ) %>%
      mutate(
        start_date = mdy(start_date),
        TEAM = toupper(team)
      ) %>%
      select(-team)
    
    # Calculate end dates for each ownership period
    # End date is the day before the next owner's start date, or today if it's the current owner
    owner_data <- owner_data %>%
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
    
    # Get all game data - DATE should already be Date type from RDS
    game_data <- get_dfs_everything() %>%
      filter(!is.na(WL)) %>%
      distinct(TEAM, DATE, WL, gametype) %>%
      mutate(DATE = as.Date(DATE))
    
    # For each owner, calculate wins/losses for all teams they owned
    owner_stats <- owner_data %>%
      group_by(owner) %>%
      group_modify(~ {
        owner_periods <- .x
        
        # Get all games for teams this owner owned during their ownership periods
        owner_games <- game_data %>%
          inner_join(
            owner_periods %>% select(TEAM, start_date, end_date),
            by = "TEAM"
          ) %>%
          filter(as.Date(DATE) >= as.Date(start_date) & as.Date(DATE) <= as.Date(end_date))
        
        # Calculate wins and losses
        wins <- sum(owner_games$WL == "W", na.rm = TRUE)
        losses <- sum(owner_games$WL == "L", na.rm = TRUE)
        wins_regular <- sum(owner_games$WL == "W" & owner_games$gametype == "REG")
        wins_playoff <- sum(owner_games$WL == "W" & owner_games$gametype == "PLAYOFF")
        losses_regular <- sum(owner_games$WL == "L" & owner_games$gametype == "REG")
        losses_playoff <- sum(owner_games$WL == "L" & owner_games$gametype == "PLAYOFF")
        teams <- unique(str_c('<img src="logo-', tolower(owner_games$TEAM), '.png" height=20></img>'))
        
        tibble(
          Teams = str_c(teams, collapse = ""),
          Wins = wins,
          `Regular Season Wins` = wins_regular,
          `Playoff Wins` = wins_playoff,
          Losses = losses,
          `Regular Season Losses` = losses_regular,
          `Playoff Losses` = losses_playoff,
          `Win Pct` = if_else(wins + losses > 0, wins / (wins + losses), 0),
          `Win Pct (Regular)` = if_else(wins_regular + losses_regular > 0, wins_regular / (wins_regular + losses_regular), 0),
          `Win Pct (Playoffs)` = if_else(wins_playoff + losses_playoff > 0, wins_playoff / (wins_playoff + losses_playoff), 0)
        )
      }) %>%
      ungroup() %>%
      arrange(desc(`Win Pct`), desc(Wins)) %>%
      mutate(
        `Win Pct` = str_c(round(`Win Pct` * 100, 1), "%"),
        `Win Pct (Regular)` = str_c(round(`Win Pct (Regular)` * 100, 1), "%"),
        `Win Pct (Playoffs)` = str_c(round(`Win Pct (Playoffs)` * 100, 1), "%")
      ) %>%
      rename(Owner = owner) %>% 
      arrange(desc(Wins))
    
    # Return message if no data
    if (nrow(owner_stats) == 0) {
      owner_stats <- tibble(
        Owner = "No data found",
        Wins = 0L,
        Losses = 0L,
        `Win Pct` = "0.0%"
      )
    }
    
    format_as_datatable(owner_stats, escape = FALSE)
  }, error = function(e) {
    # Return error message in table format
    error_df <- tibble(
      Error = str_c("Error loading data: ", as.character(e))
    )
    format_as_datatable(error_df)
  })
})
