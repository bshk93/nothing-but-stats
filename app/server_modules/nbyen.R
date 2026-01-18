# NBYen Module ----
# All outputs for the NBYen tab

trivia_save_score <- function(streak) {
    
  }
  
output$nbyen_plot <- renderPlotly({
    nbyen %>% 
      plot_ly(
        x = ~date,
        y = ~nby,
        color = ~team,
        type = "scatter",
        mode = "lines"
      )
  })
  
output$nbyen_table <- renderDT({
    nbyen %>% 
      group_by(team) %>% 
      filter(date == max(date)) %>% 
      ungroup() %>% 
      select(team, nby) %>% 
      arrange(desc(nby)) %>% 
      format_as_datatable(
        page_length = 30
      ) %>% 
      formatCurrency("nby", currency = "¥")
  })
  
  
