# Trade Machine Module ----
# All outputs for the Trade Machine tab

parse_tm_assets <- function(assets) {
    assets %>%
      str_split_1("\\n") %>%
      map_dfr(function(x) {tibble(text = x)}) %>%
      separate(text,
               into = c('PLAYER', 'SALARY', 'TEAM_TO'),
               sep = ';') %>%
      mutate(PLAYER = trim(toupper(PLAYER)),
             SALARY = as.numeric(trim(SALARY)),
             TEAM_TO = trim(toupper(TEAM_TO)))
  }
  
tm_inputs <- eventReactive(input$tm_calculate, {
    
    x <- tribble(
      ~TEAM, ~GUARANTEED, ~CAPPED,
      input$tm_team_1, input$tm_guaranteed_1, input$tm_capped_1,
      input$tm_team_2, input$tm_guaranteed_2, input$tm_capped_2,
      input$tm_team_3, input$tm_guaranteed_3, input$tm_capped_3,
      input$tm_team_4, input$tm_guaranteed_4, input$tm_capped_4
    ) %>%
      filter(trim(TEAM) != '' & trim(TEAM) != 'TEAM') %>%
      mutate(CAP = input$tm_cap,
             APRON_1 = input$tm_apron_1,
             APRON_2 = input$tm_apron_2)
    
    outgoing <- c()
    incoming <- c()
    for (i in 1:nrow(x)) {
      oc <- parse_tm_assets(input[[str_c('tm_players_', i)]]) %>%
        pull(SALARY) %>%
        sum()
      outgoing <- c(outgoing, oc)
      
      ic <- 0
      for (j in 1:nrow(x)) {
        ic_addl <- parse_tm_assets(input[[str_c('tm_players_', j)]]) %>%
          filter(TEAM_TO == input[[str_c('tm_team_', i)]]) %>%
          pull(SALARY) %>%
          sum()
        
        ic <- ic + ic_addl
      }
      incoming <- c(incoming, ic)
    }
    x$OUTGOING = outgoing
    x$INCOMING = incoming
    
    x %>%
      mutate(
        SALARY_CHG = INCOMING - OUTGOING,
        SALARY_AFTER_TRADE = GUARANTEED + SALARY_CHG
      ) %>%
      mutate(VALID = case_when(
        # Check hard cap
        CAPPED != 'None' & SALARY_AFTER_TRADE > APRON_2 ~ 'INVALID (HARD CAP)',
        CAPPED == 'Apron 1' & SALARY_AFTER_TRADE > APRON_1 ~ 'INVALID (HARD CAP)',
        
        # If salary going down, valid
        # If salary is under the cap after trade, valid
        SALARY_CHG <= 0 | SALARY_AFTER_TRADE < CAP ~ 'VALID',
        
        # If salary is over Apron 1, check for 110%
        SALARY_AFTER_TRADE >= APRON_1 & INCOMING > 1.1*OUTGOING ~ 'INVALID (110%)',
        
        # If salary is under Apron 1 (but over cap), check special cases
        OUTGOING <= 7250000 & INCOMING > 2*OUTGOING + 250000 ~ 'INVALID (2*OUT+250K)',
        OUTGOING <= 29000000 & INCOMING > OUTGOING + 7500000 ~ 'INVALID (OUT+7.5M)',
        INCOMING > 1.25*OUTGOING + 250000 ~ 'INVALID (125%)',
        
        TRUE ~ 'VALID'
      )) %>%
      datatable() %>%
      formatCurrency(c('CAP', 'APRON_1', 'APRON_2', 'INCOMING', 'OUTGOING',
                       'SALARY_CHG', 'SALARY_AFTER_TRADE', 'GUARANTEED'),
                     digits = 0)
  })
  
output$tm_output <- renderDT({tm_inputs()})
  
  
