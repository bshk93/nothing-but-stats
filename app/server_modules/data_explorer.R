# Data Explorer Module ----
# All outputs for the Data Explorer tab

output$explore_output <- renderDT({
    x <- get_dfs_everything() %>% mutate(G = 1)
    
    vars_to_summarize <- input$explore_var
    
    if (str_detect(input$explore_var, 'PCT$')) {
      pctvar_m <- str_extract(input$explore_var, '^[A-Z]{2}') %>%
        str_c('M')
      pctvar_a <- str_extract(input$explore_var, '^[A-Z]{2}') %>%
        str_c('A')
      
      vars_to_summarize <- c(pctvar_m, pctvar_a)
    }
    
    if (str_detect(input$explore_level, 'in a season')) {
      x <- x %>%
        group_by(PLAYER, SEASON) %>%
        summarize_at(vars(vars_to_summarize), lst(sum, mean))
    } else if (str_detect(input$explore_level, 'in a career')) {
      x <- x %>%
        group_by(PLAYER) %>%
        summarize_at(vars(vars_to_summarize), lst(sum, mean))
    } else {
      x <- x %>%
        select(c('PLAYER', 'SEASON', 'DATE', input$explore_var,
                 'M', 'P', 'R', 'A', 'S', 'B', 'TO', 'PF', 'GMSC',
                 'FGM', 'FGA', '3PM', '3PA', 'FTM', 'FTA'))
    }
    
    if (str_detect(input$explore_level, '(total)')) {
      sortvar <- 'sum'
      x <- x %>%
        rename(!!input$explore_var := sum) %>%
        select(-mean)
    } else if (str_detect(input$explore_level, '(avg)')) {
      sortvar <- 'mean'
      x <- x %>%
        rename(!!input$explore_var := mean) %>%
        select(-sum)
    } else {
      sortvar <- input$explore_var
    }
    
    if (input$explore_type == 'The highest') {
      x <- arrange(x, desc(pick(input$explore_var)))
    } else if (input$explore_type == 'The lowest') {
      x <- arrange(x, pick(input$explore_var))
    }
    
    x %>%
      mutate_if(is.numeric, function(x) {round(x, 2)}) %>%
      datatable(rownames = TRUE, options = list(pageLength = 100, scrollX = TRUE))
  })
  
