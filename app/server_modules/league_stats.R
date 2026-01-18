# League Stats Module ----
# All outputs for the League Stats tab

output$franchise_records <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    begin <- Sys.time()
    
    if (input$reg_flag && input$playoff_flag) {
      x <- rbind(dfs, dfs_playoffs)
    } else if (input$reg_flag && !input$playoff_flag) {
      x <- dfs
    } else if (!input$reg_flag && input$playoff_flag) {
      x <- dfs_playoffs
    } else {
      x <- head(dfs, 0)
    }
    
    if (input$season1 != "ALL-TIME") {
      x <- x %>%
        filter(str_detect(SEASON, input$season1))
    }
    
    if (input$team != "NBA") {
      x <- x %>% filter(TEAM == input$team)
    }
    
    x <- x %>%
      group_by(PLAYER) %>%
      summarize(
        G = n(),
        M = sum(M),
        P = sum(P),
        R = sum(R),
        A = sum(A),
        S = sum(S),
        B = sum(B),
        `3PM` = sum(`3PM`),
        `3P` = sum(`3PM`) / sum(`3PA`),
        FG = sum(FGM) / sum(FGA),
        FT = sum(FTM) / sum(FTA),
        GMSC = mean(GMSC)
      ) %>%
      mutate(
        MPG = M/G,
        PPG = P/G,
        RPG = R/G,
        APG = A/G,
        SPG = S/G,
        BPG = B/G,
        `3PMPG` = `3PM`/G
      )
    
    if (input$per_36_flag) {
      x <- x %>%
        
        mutate(
          MP36 = MPG*36/MPG,
          PP36 = PPG*36/MPG,
          RP36 = RPG*36/MPG,
          AP36 = APG*36/MPG,
          SP36 = SPG*36/MPG,
          BP36 = BPG*36/MPG,
          `3PMP36` = `3PMPG`*36/MPG,
          GMSCP36 = GMSC*36/MPG
        ) %>%
        
        select(-ends_with('PG'), -`3P`, -FG, -FT)
    }
    
    x_end <- ncol(x)
    
    x <- x %>%
      datatable(
        options = list(scrollX = TRUE),
        rownames = FALSE
      )
    
    if (input$per_36_flag) {
      x <- x %>% 
        formatRound(
          columns = 2:9,
          digits = 0
        ) %>%
        formatRound(
          columns = 10:x_end,
          digits = 2
        )
    } else {
      x <- x %>%
        formatRound(
          columns = 2:9,
          digits = 0
        ) %>%
        formatRound(
          columns = 10:12,
          digits = 3
        ) %>%
        formatRound(
          columns = 13:x_end,
          digits = 2
        )
    }
    
    print(glue("[{sprintf('%.7f', round(Sys.time() - begin, 7))}] nbn/franchise records generated."))
    
    x
    
  })
  
output$game_high_player <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    format_as_datatable(game_high_player)
  })
  
output$season_high_player <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    format_as_datatable(season_high_player)
  })
  
output$game_high_team <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    format_as_datatable(game_high_team)
  })
  
output$season_high_team <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    format_as_datatable(season_high_team)
  })
  
output$team_ratings <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    team_ratings %>% 
      mutate_if(is.numeric, round, 2) %>% 
      format_as_datatable()
  })
  
output$wl_streaks <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    format_as_datatable(wl_streaks)
  })
  
output$stat_race_plot <- renderPlot({
    req(input$password == myPassword || myPassword == '')
    
    if (input$race_season == "ALL-TIME") {
      x <- dfs
    } else {
      x <- dfs %>% 
        filter(SEASON == input$race_season)
    }
    
    x %>% 
      filter(PLAYER %in% input$race_players) %>% 
      select(PLAYER, DATE, input$race_var) %>% 
      arrange(PLAYER, DATE) %>% 
      group_by(PLAYER) %>% 
      mutate(!!input$race_var := cumsum(!!sym(input$race_var))) %>% 
      ungroup() %>% 
      ggplot(aes(x = DATE, y = !!sym(input$race_var), color = PLAYER)) + 
      geom_line() + 
      geom_point()
    
  })
  
  
