# NBN Wall Street Module ----
# All outputs for the NBN Wall Street tab

ws_prices_data <- reactive({
  PRICING_MODELS[[input$ws_pricing_model]](get_dfs_everything())
})

output$ws_model_description <- renderUI({
  helpText(PRICING_MODEL_DESCRIPTIONS[[input$ws_pricing_model]])
})

output$ws_prices <- renderDT({

    diffs <- ws_prices_data()
    
    diffs %>%
      mutate(
        CHG_LAST_10 = round((PRICE / lag(PRICE, 10) - 1) * 1, 4),
        CHG_LAST_82 = round((PRICE / lag(PRICE, 82) - 1) * 1, 4),
        HIGH_82 = rollmax(PRICE, 82, align = "right", fill = NA),
        LOW_82 = rollapply(PRICE, 82, min, align = "right", fill = NA)
      ) %>%
      filter(row_number() == n()) %>%
      select(TEAM, PRICE, CHG_LAST_10, CHG_LAST_82, HIGH_82, LOW_82) %>%
      datatable(rownames = FALSE, options = list(pageLength = 100)) %>%
      formatPercentage(c('CHG_LAST_10', 'CHG_LAST_82'), mark = ".", digits = 2) %>%
      formatCurrency(c('PRICE', 'HIGH_82', 'LOW_82'))
  })
  
output$wallstreet <- renderPlotly({
    
    diffs <- ws_prices_data() %>%
      filter(TEAM %in% input$ws_teams) %>%
      group_by(TEAM, SEASON) %>%
      mutate(rn = row_number(),
             #COLOR = vget_team_color(TEAM),
             N = str_c(SEASON, ' G', rn)) %>%
      ungroup()
    
    diffs_dates <- diffs %>% 
      distinct(DATE) %>% 
      arrange(DATE) %>% 
      mutate(date_index = row_number())
    
    if (!is.null(input$ws_date_min)) {
      diffs <- diffs %>%
        filter(DATE >= input$ws_date_min)
    }
    
    if (!is.null(input$ws_date_max)) {
      diffs <- diffs %>%
        filter(DATE <= input$ws_date_max)
    }
    
    if (nrow(diffs) == 0) {
      NULL
    } else if (length(input$ws_teams) == 1) {
      # 
      # diffs %>%
      #   
      #   select(SEASON, TEAM, DATE, N, DIFF_DIFF, PCT_CHG, PRICE) %>%
      #   group_by(TEAM) %>%
      #   
      #   ggplot(aes(x = DATE, y = PRICE)) +
      #   geom_line() +
      #   geom_point(aes(col = SEASON), size = 0.5) +
      #   scale_y_continuous(
      #     labels=scales::dollar_format()
      #   ) +
      #   #xlim(c(input$ws_date_min, input$ws_date_max)) +
      #   ggtitle('Team Value')
      
      # diffs %>%
      #   mutate(
      #     # Custom ordering: Regular season comes before playoffs
      #     SEASON = factor(SEASON, levels = unique(diffs %>%
      #                                               arrange(DATE) %>%  # Ensure chronological order
      #                                               pull(SEASON)), 
      #                     ordered = TRUE)
      #   ) %>%
      #   mutate(
      #     SEASON_DATE = paste(SEASON, DATE, sep = "_")  # Combine season and date
      #   ) %>%
      #   ggplot(aes(x = SEASON_DATE, y = PRICE, group = TEAM)) +
      #   geom_line() +
      #   geom_point(aes(col = SEASON), size = 0.5) +
      #   scale_y_continuous(labels = scales::dollar_format()) +
      #   scale_x_discrete(
      #     breaks = function(x) x[seq(1, length(x), by = 30)],  # Show every 30th tick
      #     labels = function(x) gsub(".*_(.*)", "\\1", x)  # Only display the date part
      #   ) +
      #   theme_minimal() +
      #   theme(
      #     axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Smaller, angled labels
      #     panel.grid.major.x = element_blank()  # Remove excessive gridlines
      #   ) +
      #   labs(x = "Date", y = "Price", title = "Price Trends by Season")
      
      # Plot without explicit ordering of SEASON, but removing gaps
      p <- diffs %>%
        left_join(diffs_dates, by = "DATE") %>% 
        ggplot(aes(x = date_index, y = PRICE, group = TEAM)) +
        geom_line() +
        geom_point(aes(col = SEASON), size = 0.5) +
        scale_y_continuous(labels = scales::dollar_format()) +
        # scale_x_date(
        #   breaks = "1 month",  # Monthly breaks for better readability
        #   date_labels = "%b %Y",  # Format the dates
        #   expand = c(0, 0)  # Avoid padding
        # ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1, size = 8),  # Smaller, angled labels
          panel.grid.major.x = element_blank()  # Remove excessive gridlines
        ) +
        labs(x = "Date", y = "Price", title = "Price Trends by Season")
      
      ggplotly(p) %>% 
        layout(
          xaxis = list(
            title = "Date",
            tickmode = "array",
            tickvalues = unique(diffs_dates$date_index),
            ticktext = unique(format(diffs_dates$DATE, "%b %d")),
            tickangle = -45
          )
        )
      
      
    } else {
      
      p <- diffs %>%
        left_join(diffs_dates, by = "DATE") %>% 
        
        select(rn, SEASON, TEAM, DATE, PCT_CHG, PRICE, date_index) %>%
        
        ggplot(aes(x = date_index, y = PRICE, color = TEAM)) +
        #ggplot(aes(x = DATE, y = PRICE, color = COLOR)) +
        geom_line() +
        scale_y_continuous(
          labels=scales::dollar_format()
        ) +
        #xlim(c(input$ws_date_min, input$ws_date_max)) +
        ggtitle('Team Value')
      
      ggplotly(p) %>% 
        layout(
          xaxis = list(
            title = "Date",
            tickmode = "array",
            tickvalues = unique(diffs_dates$date_index),
            ticktext = unique(format(diffs_dates$DATE, "%b %d")),
            tickangle = -45
          )
        )
      
    }
  })
  
output$ws_div <- renderPlotly({
    x <- ws_prices_data() %>%
      ungroup() %>%
      mutate(CONF = toupper(get_conference(TEAM)),
             DIV = case_when(
               TEAM %in% c('BOS', 'PHI', 'NYK', 'BKN', 'TOR') ~ 'ATLA',
               TEAM %in% c('MIL', 'CLE', 'CHI', 'IND', 'DET') ~ 'CENT',
               TEAM %in% c('ATL', 'MIA', 'WAS', 'ORL', 'CHA') ~ 'SOEA',
               TEAM %in% c('DEN', 'MIN', 'OKC', 'UTA', 'POR') ~ 'NOWE',
               TEAM %in% c('SAC', 'PHX', 'LAC', 'GSW', 'LAL') ~ 'PACI',
               TEAM %in% c('MEM', 'NOP', 'DAL', 'HOU', 'SAS') ~ 'SOWE',
               TRUE ~ NA_character_
             )) %>%
      
      group_by(DATE, DIV) %>%
      summarize(
        PCT_CHG = mean(PCT_CHG)
      ) %>%
      group_by(DIV) %>%
      arrange(DIV, DATE) %>%
      mutate(PRICE = cumprod(PCT_CHG) * 100)
    
    if (!is.null(input$ws_date_min)) {
      x <- x %>%
        filter(DATE >= input$ws_date_min)
    }
    
    if (!is.null(input$ws_date_max)) {
      x <- x %>%
        filter(DATE <= input$ws_date_max)
    }
    
    x %>%
      
      ggplot(aes(x = DATE, y = PRICE, color = DIV)) +
      geom_line() +
      scale_y_continuous(labels=scales::dollar_format()) +
      ggtitle('Division Indexes')
  })
  
  
