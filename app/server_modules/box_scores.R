# Box Scores Module ----
# All outputs for the Box Scores tab

output$boxscore_input <- renderUI({
    req(input$boxscoredate)
    selectInput('boxscore_output',
                'Select game:',
                get_dfs_everything() %>%
                  filter(DATE == input$boxscoredate) %>%
                  distinct(TEAM, OPP) %>%
                  filter(str_detect(OPP, '^@')) %>%
                  mutate(x = str_c(TEAM, OPP)) %>%
                  pull(x))
  })
  
output$boxscore_selected <- renderDT({
    req(input$boxscore_output)
    
    get_box_score(get_dfs_everything(), input$boxscoredate, input$boxscore_output)
  }, rownames = FALSE)
  
