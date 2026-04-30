# Franchise Profiles Module ----
# All outputs for the Franchise Profiles tab

output$team_history_logo <- renderText({
    get_logo(input$team_history, height = 100, align = 'left')
  })
  
output$franchise_history_rings <- renderUI({
    
    x <- champions %>%
      distinct(TEAM, SEASON) %>%
      filter(TEAM == input$team_history) %>%
      mutate(SEASON = str_replace(SEASON, ' Playoffs', ''))
    
    glue("Rings: {nrow(x)} ({str_c(x$SEASON, collapse = ', ')})") %>%
      HTML()
    
  })
  
output$franchise_history_retired <- renderUI({
    
    x <- get_retired_jerseys() %>%
      filter(TEAM == input$team_history) %>%
      mutate(TXT = str_c(NO, ': ', PLAYER)) %>%
      pull(TXT) %>%
      str_c(collapse = '<br/>')
    
    str_c("<br/>Retired Jerseys:<br/>", x) %>%
      HTML()
    
  })
  
output$franchise_history_yoy <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    x <- get_dfs_everything() %>%
      filter(TEAM == input$team_history)
    
    x_leaders <- x %>%
      group_by(SEASON, PLAYER) %>%
      summarize(G = n(), GMSC = mean(GMSC)) %>%
      group_by(SEASON) %>%
      arrange(SEASON, desc(GMSC)) %>%
      mutate(GMSC_RNK = rank(desc(GMSC), ties.method = "min")) %>%
      filter(GMSC_RNK <= 3) %>%
      mutate(GMSC_LEADERS = str_c(GMSC_RNK, ". ", PLAYER, " (", round(GMSC, 2), ")")) %>%
      distinct(SEASON, GMSC_RNK, GMSC_LEADERS) %>%
      pivot_wider(names_from = "GMSC_RNK", values_from = "GMSC_LEADERS") %>%
      mutate(GMSC_LEADERS = str_c(`1`, `2`, `3`, sep = "<br>")) %>%
      select(SEASON, GMSC_LEADERS)
    
    x_season <- x %>%
      distinct(SEASON, DATE, WL) %>%
      group_by(SEASON) %>%
      summarize(
        W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0)),
        L = sum(case_when(WL == "L" ~ 1, TRUE ~ 0))
      ) %>%
      ungroup() %>%
      
      mutate(PCT = round(W / (W + L), 3)) %>%
      
      left_join(get_owners() %>% filter(TEAM == input$team_history) %>% select(-TEAM)) %>%
      fill(OWNER) %>%
      
      left_join(x_leaders)
    
    x_total <- x %>%
      distinct(DATE, WL) %>%
      summarize(
        W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0)),
        L = sum(case_when(WL == "L" ~ 1, TRUE ~ 0))
      ) %>%
      ungroup() %>%
      mutate(PCT = round(W / (W + L), 3),
             SEASON = "TOTAL",
             GMSC_LEADERS = "-")
    
    bind_rows(x_season, x_total) %>%
      
      datatable(
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
          "SEASON BY SEASON RESULTS"
        ),
        options = list(scrollX = TRUE),
        rownames = FALSE,
        escape = FALSE
      ) %>%
      formatStyle(
        "SEASON",
        target = "row",
        fontWeight = styleEqual(c("TOTAL"), "bold", default = "normal")
      )
    
  })
  
output$franchise_history_scatter <- renderPlotly({
    req(input$password == myPassword || myPassword == '')
    
    team_data <- team_ratings %>%
      ungroup() %>%
      mutate(ids = str_c(SEASON, ' ', TEAM, '\nOFF: ', round(OFF_RTG, 2), '\nDEF: ', round(DEF_RTG, 2)),
             color = case_when(TEAM == input$team_history ~ 'red',
                               TRUE ~ 'blue'))
    
    team_data_selected <- team_data %>%
      filter(TEAM == input$team_history) %>%
      arrange(SEASON)
    
    team_data %>%
      plot_ly(
        x = ~OFF_RTG,
        y = ~DEF_RTG,
        text = ~ids,
        #color = ~color,
        #colors = ~color,
        marker = list(
          color = ~color
        ),
        type = 'scatter',
        mode = 'markers',
        showlegend = F
      ) %>%
      
      add_trace(text = ~ids, hoverinfo = 'text', showlegend = F) %>%
      
      add_annotations(
        ax = team_data_selected$OFF_RTG[-nrow(team_data_selected)],
        ay = team_data_selected$DEF_RTG[-nrow(team_data_selected)],
        x = team_data_selected$OFF_RTG[-1],
        y = team_data_selected$DEF_RTG[-1],
        xref = 'x',
        yref = 'y',
        axref = 'x',
        ayref = 'y',
        showarrow = T,
        text = ''
      ) %>%
      layout(
        xaxis = list(
          range = c(-15, 15)
        ),
        yaxis = list(
          range = c(-15, 15)
        )
      )
    
  })
  
output$franchise_history_awards <- renderDT({
    
    req(input$password == myPassword || myPassword == '')
    
    bind_rows(
      get_foty(),
      get_coty()
    ) %>%
      filter(TEAM == input$team_history) %>%
      arrange(SEASON) %>%
      datatable(
        options = list(scrollX = TRUE),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
          "FRONT OFFICE AWARDS"
        ),
        rownames = FALSE
      )
  })
  
output$franchise_history_legends <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    calculate_hof_points(
      get_dfs_everything(),
      dfs_playoffs,
      dfs,
      team_filter = input$team_history
    )
  }, options = list(scrollX = TRUE))
  
output$franchise_history_leaders <- renderDT({
    req(input$password == myPassword || myPassword == '')
    
    ctg <- input$stat_cat_team_history
    
    x <- get_dfs_everything() %>%
      filter(TEAM == input$team_history) %>%
      mutate(G = 1) %>%
      select(PLAYER, DATE, ctg)
    
    if (input$stattype1 == "All-Time") {
      out <- x %>%
        select(-DATE) %>%
        group_by(PLAYER) %>%
        summarize_all(sum) %>%
        mutate(RANK = rank(desc(.[[ctg]]), ties.method = 'min')) %>%
        filter(RANK <= 10) %>%
        arrange(RANK)
    } else if (input$stattype1 == "Single Game") {
      out <- x %>%
        mutate(RANK = rank(desc(.[[ctg]]), ties.method = 'min')) %>%
        filter(RANK <= 10) %>%
        arrange(RANK)
    }
    
    out
    
  }, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  
output$franchise_history_cum_diff <- renderPlot({
    req(input$password == myPassword || myPassword == '')

    cum_diff_precomputed %>%
      filter(TEAM == input$team_history) %>%
      ggplot(aes(x = G, y = CUM_DIFF)) +
      geom_line() +
      geom_point(aes(col = SEASON))
  })
  
  
