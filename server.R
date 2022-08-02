library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(glue)
library(DT)
library(zoo)

dfs <- list(
  read_csv("allstats-20-21.csv", show_col_types = F) %>% mutate(SEASON = "20-21"),
  read_csv("allstats-21-22.csv", show_col_types = F) %>% mutate(SEASON = "21-22")
) %>% 
  bind_rows() %>% 
  mutate(DATE = mdy(DATE),
         FG = str_c(FGM, "-", FGA),
         `3P` = str_c(`3PM`, "-", `3PA`),
         FT = str_c(FTM, "-", FTA),
         GMSC = P + (0.4 * FGM) - (0.7 * FGA) - (0.4 * (FTA - FTM)) + (0.7 * OR) + 
           (0.3 * DR) + S + (0.7 * A) + (0.7 * B) - (0.4 * PF) - TO) %>% 
  mutate(GMSC = round(GMSC, 2)) %>% 
  arrange(PLAYER, DATE)

dfs_playoffs <- list(
  read_csv("allstats-playoffs-21.csv", show_col_types = F) %>% mutate(SEASON = "20-21-Playoffs"),
  read_csv("allstats-playoffs-22.csv", show_col_types = F) %>% mutate(SEASON = "21-22-Playoffs")
) %>% 
  bind_rows() %>% 
  mutate(DATE = mdy(DATE),
         FG = str_c(FGM, "-", FGA),
         `3P` = str_c(`3PM`, "-", `3PA`),
         FT = str_c(FTM, "-", FTA),
         GMSC = P + (0.4 * FGM) - (0.7 * FGA) - (0.4 * (FTA - FTM)) + (0.7 * OR) + 
           (0.3 * DR) + S + (0.7 * A) + (0.7 * B) - (0.4 * PF) - TO) %>% 
  mutate(GMSC = round(GMSC, 2)) %>% 
  arrange(PLAYER, DATE)

bios <- read_csv("player-bio-database.csv", skip = 1, show_col_types = F) %>% 
  select(-Name...1) %>% 
  rename(Name = Name...2) %>% 
  mutate(DOB = mdy(DOB))

function(input, output, session) {
  
  updateSelectizeInput(
    session, 
    'foo', 
    choices = sort(unique(dfs$PLAYER)), 
    server = TRUE
  )
  
  myPlayerData <- reactive({
    dfs %>% 
      filter(PLAYER == input$name)
  })
  
  myPlayerPlayoffData <- reactive({
    dfs_playoffs %>% 
      filter(PLAYER == input$name)
  })
  
  myCombinedData <- reactive({
    bind_rows(myPlayerData(), myPlayerPlayoffData())
  })
  
  myBiosData <- reactive({
    bios %>% 
      filter(Name == input$name)
  })
  
  output$player_summary <- renderText({
    glue(
      "{input$name} ({myBiosData() %>% pull(`Combo Pos.`)}):\n",
      "Current Team: {myBiosData() %>% pull(Team)}\n",
      "DOB: {myBiosData() %>% pull(DOB)}\n",
      "Age: {round(time_length(interval(myBiosData() %>% pull(DOB), today()), 'years'), 2)}\n",
      "Height: {myBiosData() %>% pull(Height)}\n",
      "Weight: {myBiosData() %>% pull(Weight)}\n",
      "From: {myBiosData() %>% pull(COLLEGE)}"
    )
  })
  
  output$headshot <- renderText({
    c('<img src="', myBiosData() %>% pull(`Img URL`), '">')
  })
  
  output$tbl <- renderDT({
    myCombinedData() %>% 
      select(
        DATE, TEAM, OPP, M, P, R, A, S, B, TO, 
        FG, `3P`, FT, PF, GMSC, WL
      ) %>% 
      arrange(desc(DATE))
  }, server = TRUE)
  
  output$tbl_season <- renderDT({
    myCombinedData() %>% 
      group_by(SEASON, TEAM) %>% 
      mutate(first_date = min(DATE)) %>% 
      group_by(SEASON, first_date, TEAM) %>% 
      summarize(
        G    = n(),
        MPG  = mean(M) %>% round(2) %>% format(nsmall = 2),
        PPG  = mean(P) %>% round(2) %>% format(nsmall = 2),
        RPG  = mean(R) %>% round(2) %>% format(nsmall = 2),
        APG  = mean(A) %>% round(2) %>% format(nsmall = 2),
        SPG  = mean(S) %>% round(2) %>% format(nsmall = 2),
        BPG  = mean(B) %>% round(2) %>% format(nsmall = 2),
        FG   = (sum(FGM) / sum(FGA)) %>% round(3) %>% format(nsmall = 3),
        `3P` = (sum(`3PM`) / sum(`3PA`)) %>% round(3) %>% format(nsmall = 3),
        FT   = (sum(FTM) / sum(FTA)) %>% round(3) %>% format(nsmall = 3)
      ) %>% 
      ungroup() %>% 
      select(-first_date)
  })
  
  output$records <- renderDT({
    bind_rows(
      myCombinedData() %>% 
        mutate(CATEGORY = "POINTS", MAX = max(P)),
      myCombinedData() %>% 
        mutate(CATEGORY = "REBOUNDS", MAX = max(R)),
      myCombinedData() %>% 
        mutate(CATEGORY = "ASSISTS", MAX = max(A)),
      myCombinedData() %>% 
        mutate(CATEGORY = "STEALS", MAX = max(S)),
      myCombinedData() %>% 
        mutate(CATEGORY = "BLOCKS", MAX = max(B)),
      myCombinedData() %>% 
        mutate(CATEGORY = "3PM", MAX = max(`3PM`)),
      myCombinedData() %>% 
        mutate(CATEGORY = "GMSC", MAX = max(GMSC))
    ) %>% 
      select(CATEGORY, MAX) %>% 
      distinct()
  })
  
  output$gmsc_plot <- renderPlot({
    myCombinedData() %>% 
      mutate(trailing_gmsc_10 = rollmean(GMSC, 10, fill = NA, align = "right")) %>% 
      ggplot(aes(x = DATE, y = trailing_gmsc_10)) +
      geom_step(color = "#69b3a2") +
      scale_x_date(date_breaks = "1 month") +
      ylim(c(-10, 30)) + 
      theme(axis.text.x=element_text(angle=60, hjust=1)) 
  })
  
  output$gmsc_histogram <- renderPlot({
    myCombinedData() %>% 
      ggplot(aes(x = GMSC, fill = TEAM)) +
      geom_histogram(binwidth = 1) +
      xlim(c(-20, 80))
  })
  
  output$gmsc_histogram_comp <- renderPlot({
    
    dfs %>% 
      filter(PLAYER == input$comp) %>% 
      bind_rows(dfs_playoffs %>% filter(PLAYER == input$comp)) %>% 
      ggplot(aes(x = GMSC, fill = TEAM)) +
      geom_histogram(binwidth = 1) +
      xlim(c(-20, 80))
  })
  
}