# Frivolities Module ----
# All outputs for the Frivolities tab

output$stability <- renderPlotly({
    roster_stability(dfs) %>% 
      plot_roster_stability()
  })
  
output$most_teams <- renderDT({
    most_teams(get_dfs_everything()) %>% 
      format_as_datatable(escape = FALSE)
  })
  
output$playoff_risers <- renderDT({
    playoff_risers(get_dfs_everything()) %>% 
      format_as_datatable()
  })
  
  
