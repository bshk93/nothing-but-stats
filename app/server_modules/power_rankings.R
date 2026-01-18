# Power Rankings Module ----
# All outputs for the Power Rankings tab

output$power_rankings <- renderPlot({
    xx <- dfs %>%
      filter(SEASON == input$pr_season) %>%
      calculate_power_rankings() %>%
      filter(TEAM %in% input$pr_teams)
    
    xx %>%
      ggplot(aes(x = DATE, y = TEAM_PR, color = COLOR)) +
      geom_line() +
      geom_point() +
      scale_color_identity(
        guide = 'legend',
        labels = unique(xx$TEAM),
        breaks = vget_team_color(unique(xx$TEAM))
      ) +
      scale_y_reverse(limits = c(30, 0))
  })
  
output$power_rankings_table <- renderReactable({
    out_df <- dfs %>%
      filter(SEASON == input$pr_season) %>%
      calculate_power_rankings() %>%
      select(DATE, TEAM, TEAM_PR) %>%
      
      pivot_wider(id_cols = 'TEAM', names_from = 'DATE', values_from = 'TEAM_PR') %>%
      
      select('TEAM', ncol(.):2) %>%
      
      arrange(.[,2])
    
    my_pal <- function(x) {
      if (!is.na(x)){
        rgb(colorRamp(c("red", "green"))(x), maxColorValue = 255)
      } else {
        "#e9e9e9" #grey
      }
    }
    
    coldefs <- list(
      colDef(
        style = function(value, index, name) {
          color <- my_pal((31-value)/30)
          list(background = color)
        }
      )
    )
    
    coldefs <- rep(coldefs, ncol(out_df) - 1)
    names(coldefs) <- out_df %>%
      select(-TEAM) %>%
      names()
    
    reactable(
      out_df,
      columns = coldefs,
      defaultColDef = colDef(align = 'center'),
      defaultPageSize = 30,
      highlight = TRUE,
      compact = TRUE
    )
  })
  
  
