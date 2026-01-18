# Player Compare Module ----
# All outputs for the Player Compare tab

output$player_compare <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    x <- get_dfs_everything() %>%
      filter(PLAYER == input$playercomp1 | PLAYER == input$playercomp2)
    
    if (input$playercomp_season != 'CAREER') {
      x <- x %>%
        filter(SEASON == input$playercomp_season)
    }
    
    y <- x %>%
      group_by(PLAYER) %>%
      summarize(
        G = n(),
        MPG = (sum(M)/G) %>% round(2),
        PPG = (sum(P)/G) %>% round(2),
        APG = (sum(A)/G) %>% round(2),
        RPG = (sum(R)/G) %>% round(2),
        SPG = (sum(S)/G) %>% round(2),
        BPG = (sum(B)/G) %>% round(2),
        TOPG = (sum(TO)/G) %>% round(2),
        GMSC = (sum(GMSC)/G) %>% round(2),
        FG = (sum(FGM)/sum(FGA)) %>% round(3),
        `3P` = (sum(`3PM`)/sum(`3PA`)) %>% round(3),
        FT = (sum(FTM)/sum(FTA)) %>% round(3)
      ) %>%
      pivot_longer(names_to = 'CATEGORY',
                   cols = c('G', 'MPG', 'PPG', 'APG', 'RPG', 'SPG', 'BPG', 'TOPG',
                            'GMSC', 'FG', '3P', 'FT')) %>%
      pivot_wider(names_from = 'PLAYER')
    
    if (!has_name(y, input$playercomp1)) {
      y[input$playercomp1] <- '-'
    }
    
    if (!has_name(y, input$playercomp2)) {
      y[input$playercomp2] <- '-'
    }
    
    
    y %>%
      select('CATEGORY', input$playercomp1, input$playercomp2) %>%
      mutate(winner = case_when(
        .[[input$playercomp1]] > .[[input$playercomp2]] ~ 1,
        .[[input$playercomp1]] < .[[input$playercomp2]] ~ 2,
        TRUE ~ 0
      )) %>%
      datatable(rownames = FALSE,
                options = list(
                  columnDefs = list(list(visible = FALSE, targets = c(3))),
                  pageLength = 50
                )) %>%
      formatStyle(
        input$playercomp1,
        target = "cell",
        valueColumns = 'winner',
        fontWeight = styleEqual(c(1), "bold", default = "normal")
      ) %>%
      formatStyle(
        input$playercomp2,
        target = "cell",
        valueColumns = 'winner',
        fontWeight = styleEqual(c(2), "bold", default = "normal")
      )
    
  })
  
  
