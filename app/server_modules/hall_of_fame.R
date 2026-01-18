# Hall of Fame + Awards Module ----
# All outputs for the Hall of Fame + Awards tab

output$hof_points <- renderDT({
    calculate_hof_points(get_dfs_everything(), dfs_playoffs, dfs)
  })
  #}, options = list(scrollX = TRUE))
  
output$hof_plot_bar <- renderPlotly({
    p <- calculate_hof_points(get_dfs_everything(), dfs_playoffs, dfs, raw_data = TRUE) %>% 
      filter(HOF_POINTS >= 100) %>% 
      arrange(desc(HOF_POINTS)) %>% 
      ggplot(aes(x = reorder(PLAYER, -HOF_POINTS), y = HOF_POINTS, color = PLAYER)) + 
      geom_bar(stat = "identity") + 
      theme_minimal()
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(
          tickangle = -45
        )
      )
  })
  
output$hof_plot <- renderPlotly({
    x <- get_dfs_everything() %>% 
      group_by(PLAYER) %>%
      mutate(
        G = 1,
        
        GMSC_WGT_WL = case_when(
          WL == 'W' ~ 1.25,
          TRUE ~ 0.75
        ),
        GMSC_WGT_GAMETYPE = case_when(
          ROUND == 1 ~ 2,
          ROUND == 2 ~ 4,
          ROUND == 3 ~ 8,
          ROUND == 4 ~ 16,
          TRUE ~ 1
        ),
        
        GMSC_WEIGHTED = GMSC * GMSC_WGT_WL * GMSC_WGT_GAMETYPE
      ) %>% 
      arrange(PLAYER, DATE) %>% 
      mutate(HOF_POINTS = cumsum(GMSC_WEIGHTED/100),
             TOT_POINTS = sum(GMSC_WEIGHTED)/100) %>% 
      filter(TOT_POINTS >= 130) %>% 
      ungroup()
    
    x_dates <- x %>% 
      distinct(DATE) %>% 
      arrange(DATE) %>% 
      mutate(date_index = row_number())
    
    # Plot without explicit ordering of SEASON, but removing gaps
    p <- x %>%
      left_join(x_dates, by = "DATE") %>% 
      ggplot(aes(x = date_index, y = HOF_POINTS, group = PLAYER)) +
      geom_line(aes(col = PLAYER)) +
      #geom_point(aes(col = PLAYER), size = 0.5) +
      #scale_y_continuous(labels = scales::dollar_format()) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Smaller, angled labels
        panel.grid.major.x = element_blank()  # Remove excessive gridlines
      ) +
      labs(x = "Date", y = "HOF Points", title = "")
    
    ggplotly(p) %>% 
      layout(
        xaxis = list(
          title = "Date",
          tickmode = "array",
          tickvalues = unique(x_dates$date_index),
          ticktext = unique(format(x_dates$DATE, "%b %d")),
          tickangle = -45
        )
      )
  })
  
output$league_champs <- renderDT({
    x <- champions %>% 
      distinct(SEASON, CHAMPION = TEAM) %>% 
      mutate(CHAMPION = get_logo(CHAMPION, height = 30))
    
    y <- get_runners_up() %>% 
      mutate(
        RUNNER_UP = get_logo(RUNNER_UP, height = 30),
        EAST_RUNNER_UP = get_logo(EAST_RUNNER_UP, height = 30),
        WEST_RUNNER_UP = get_logo(WEST_RUNNER_UP, height = 30)
      )
    
    x %>% 
      full_join(y, by = "SEASON") %>% 
      arrange(SEASON) %>% 
      format_as_datatable(escape = FALSE)
  })
  
output$season_awards_history <- renderDT({
    x <- bind_rows(
      get_mvp() %>% mutate(AWARD = "Most Valuable Player"),
      get_dpoy() %>% mutate(AWARD = "Defensive Player of the Year"),
      get_roy() %>% mutate(AWARD = "Rookie of the Year"),
      get_6moy() %>% mutate(AWARD = "Sixth Man of the Year"),
      get_mip() %>% mutate(AWARD = "Most Improved Player")
    ) %>% 
      select(AWARD, SEASON, PLAYER)
    
    y <- dfs %>% 
      filter(PLAYER %in% x$PLAYER) %>% 
      group_by(PLAYER, SEASON) %>% 
      arrange(PLAYER, SEASON, DATE) %>% 
      summarize(TEAM = last(TEAM), .groups = "drop") %>% 
      mutate(TEAM = get_logo(TEAM, height = 30))
    
    x %>% 
      left_join(
        y,
        by = c("PLAYER", "SEASON")
      ) %>% 
      mutate(PLAYER = str_c(PLAYER, " ", TEAM)) %>% 
      select(-TEAM) %>% 
      format_as_datatable(escape = FALSE)
  })
  
output$front_office_awards <- renderDT({
    bind_rows(
      get_foty(),
      get_coty() %>% 
        mutate(
          x = str_extract(AWARD, "\\(.*\\)"),
          AWARD = "COTY"
        )
    ) %>% 
      arrange(SEASON, AWARD) %>% 
      
      mutate(
        TEAM = str_c(TEAM, " ", get_logo(TEAM, height = 30), " ", coalesce(x, ""))
      ) %>% 
      
      select(SEASON, AWARD, TEAM) %>% 
      format_as_datatable(escape = FALSE)
  })
  
output$all_nbn <- renderDT({
    x <- bind_rows(
      get_allnbn1() %>% mutate(AWARD = "NBN FIRST TEAM"),
      get_allnbn2() %>% mutate(AWARD = "NBN SECOND TEAM"),
      get_allnbn3() %>% mutate(AWARD = "NBN THIRD TEAM")
    ) %>% 
      select(SEASON, AWARD, PLAYER) %>% 
      arrange(SEASON, AWARD, PLAYER)
    
    y <- dfs %>% 
      filter(PLAYER %in% x$PLAYER) %>% 
      group_by(PLAYER, SEASON) %>% 
      arrange(PLAYER, SEASON, DATE) %>% 
      summarize(TEAM = last(TEAM), .groups = "drop") %>% 
      mutate(TEAM = get_logo(TEAM, height = 30))
    
    x %>% 
      left_join(
        y,
        by = c("PLAYER", "SEASON")
      ) %>% 
      mutate(PLAYER = str_c(PLAYER, " ", TEAM)) %>% 
      select(-TEAM) %>% 
      format_as_datatable(escape = FALSE, page_length = 15)
  })
  
  
