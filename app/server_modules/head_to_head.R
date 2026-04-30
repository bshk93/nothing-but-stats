# Head to Head Module ----
# All outputs for the Head to Head tab

# All-time head-to-head record
output$h2h_alltime <- renderDT({
  req(input$h2h_team1, input$h2h_team2)
  req(input$h2h_team1 != input$h2h_team2)
  
  team1 <- input$h2h_team1
  team2 <- input$h2h_team2
  
  # Get all games between these two teams (regular season + playoffs)
  all_games <- get_dfs_everything() %>%
    filter(
      (TEAM == team1 & str_replace(OPP, "@", "") == team2) |
      (TEAM == team2 & str_replace(OPP, "@", "") == team1)
    ) %>%
    group_by(SEASON, DATE, TEAM, OPP, WL) %>%
    summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))
  
  # Join to get both scores
  games_with_scores <- all_games %>%
    left_join(
      all_games %>% select(SEASON, DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
      by = c("SEASON", "DATE", "OPP_RAW" = "OPP")
    ) %>%
    filter(TEAM == team1) %>%
    mutate(
      WIN = TEAM_PTS > OPP_PTS,
      LOSS = TEAM_PTS < OPP_PTS
    )
  
  # Calculate record
  wins <- sum(games_with_scores$WIN, na.rm = TRUE)
  losses <- sum(games_with_scores$LOSS, na.rm = TRUE)
  
  # Split by regular season and playoffs
  regular_games <- filter_regular(games_with_scores)
  playoff_games <- filter_playoffs(games_with_scores)
  
  regular_wins <- sum(regular_games$WIN, na.rm = TRUE)
  regular_losses <- sum(regular_games$LOSS, na.rm = TRUE)
  playoff_wins <- sum(playoff_games$WIN, na.rm = TRUE)
  playoff_losses <- sum(playoff_games$LOSS, na.rm = TRUE)
  
  result <- tibble(
    `Team 1` = str_c(team1, " ", get_logo(team1, height = 20)),
    `Team 2` = str_c(team2, " ", get_logo(team2, height = 20)),
    `Overall Record` = str_c(wins, "-", losses),
    `Regular Season` = str_c(regular_wins, "-", regular_losses),
    `Playoffs` = str_c(playoff_wins, "-", playoff_losses)
  )
  
  format_as_datatable(result, escape = FALSE)
})

# Playoff series record
output$h2h_playoff_series <- renderDT({
  req(input$h2h_team1, input$h2h_team2)
  req(input$h2h_team1 != input$h2h_team2)
  
  team1 <- input$h2h_team1
  team2 <- input$h2h_team2
  
  # Get all playoff games between these teams
  playoff_games <- dfs_playoffs %>%
    filter(
      (TEAM == team1 & str_replace(OPP, "@", "") == team2) |
      (TEAM == team2 & str_replace(OPP, "@", "") == team1)
    ) %>%
    group_by(SEASON, ROUND, TEAM, GAME, OPP, WL) %>%
    summarize(P = sum(P), .groups = 'drop') %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))
  
  if (nrow(playoff_games) == 0) {
    return(format_as_datatable(tibble(Message = "No playoff series between these teams.")))
  }
  
  # Join with itself to get both teams' scores and determine winners
  games_with_scores <- playoff_games %>%
    left_join(
      playoff_games %>% select(SEASON, ROUND, GAME, OPP = TEAM, OPP_PTS = P),
      by = c("SEASON", "ROUND", "GAME", "OPP_RAW" = "OPP")
    ) %>%
    filter(TEAM == team1) %>%
    mutate(
      WINNER = case_when(
        P > OPP_PTS ~ team1,
        OPP_PTS > P ~ team2,
        TRUE ~ "Tied"
      )
    )
  
  # Group by series (SEASON, ROUND) and count wins
  series_summary <- games_with_scores %>%
    group_by(SEASON, ROUND) %>%
    summarize(
      TEAM1_WINS = sum(WINNER == team1, na.rm = TRUE),
      TEAM2_WINS = sum(WINNER == team2, na.rm = TRUE)
    ) %>%
    mutate(
      `Series` = str_c(SEASON, " - Round ", ROUND),
      `Result` = str_c(team1, " ", TEAM1_WINS, "-", TEAM2_WINS, " ", team2),
      `Winner` = case_when(
        TEAM1_WINS > TEAM2_WINS ~ team1,
        TEAM2_WINS > TEAM1_WINS ~ team2,
        TRUE ~ "Tied"
      )
    ) %>%
    arrange(desc(SEASON), desc(ROUND)) %>%
    select(`Series`, `Result`, `Winner`)
  
  format_as_datatable(series_summary)
})

# Playoff series details with games and top performers
output$h2h_playoff_details <- renderUI({
  req(input$h2h_team1, input$h2h_team2)
  req(input$h2h_team1 != input$h2h_team2)
  
  team1 <- input$h2h_team1
  team2 <- input$h2h_team2
  
  # Get all playoff games between these teams
  playoff_games <- dfs_playoffs %>%
    filter(
      (TEAM == team1 & str_replace(OPP, "@", "") == team2) |
      (TEAM == team2 & str_replace(OPP, "@", "") == team1)
    ) %>%
    group_by(SEASON, ROUND, DATE, TEAM, GAME, OPP, WL) %>%
    summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))
  
  if (nrow(playoff_games) == 0) {
    return(p("No playoff series between these teams."))
  }
  
  # Get scores for each game
  games_with_scores <- playoff_games %>%
    left_join(
      playoff_games %>% select(SEASON, ROUND, GAME, DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
      by = c("SEASON", "ROUND", "GAME", "DATE", "OPP_RAW" = "OPP")
    ) %>%
    filter(TEAM == team1) %>%
    arrange(SEASON, ROUND, GAME)
  
  # Group by series
  series_list <- games_with_scores %>%
    group_by(SEASON, ROUND) %>%
    group_split()
  
  # Create UI for each series
  series_uis <- map(series_list, function(series_df) {
    season <- first(series_df$SEASON)
    round <- first(series_df$ROUND)
    
    # Get top performers for each game in this series (from pre-computed table)
    game_details <- map_dfr(1:nrow(series_df), function(i) {
      game_row <- series_df[i, ]

      game_players <- playoff_top_performers %>%
        filter(
          SEASON == game_row$SEASON,
          ROUND == game_row$ROUND,
          GAME == game_row$GAME,
          DATE == game_row$DATE,
          TEAM %in% c(team1, team2)
        )

      team1_top <- game_players %>% filter(TEAM == team1)

      team1_top_str <- if (nrow(team1_top) > 0) {
        team1_top %>%
          mutate(
            `Team 1 Top Performers` = str_c(PLAYER, " - ", P, " PTS, ", R, " REB, ", A, " AST (", round(GMSC, 1), " GMSC)")
          ) %>%
          pull(`Team 1 Top Performers`) %>%
          str_c(collapse = "<br/>")
      } else {
        "N/A"
      }
      
      team2_top <- game_players %>% filter(TEAM == team2)
      
      team2_top_str <- if (nrow(team2_top) > 0) {
        team2_top %>%
          mutate(
            `Team 2 Top Performers` = str_c(PLAYER, " - ", P, " PTS, ", R, " REB, ", A, " AST (", round(GMSC, 1), " GMSC)")
          ) %>%
          pull(`Team 2 Top Performers`) %>%
          str_c(collapse = "<br/>")
      } else {
        "N/A"
      }
      
      tibble(
        `Game` = game_row$GAME,
        `Score` = str_c(team1, " ", game_row$TEAM_PTS, " - ", game_row$OPP_PTS, " ", team2),
        `Team 1 Top Performers` = team1_top_str,
        `Team 2 Top Performers` = team2_top_str
      )
    })
    
    # Create table for this series
    series_title <- str_c(season, " - Round ", round)
    
    div(
      h4(series_title),
      renderDT({
        format_as_datatable(game_details, escape = FALSE)
      })
    )
  })
  
  # Return all series UIs
  tagList(series_uis)
})
