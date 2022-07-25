# setwd("C:/Users/bshk9/OneDrive/home/projects/nbn/nothing-but-stats")
# googledrive::drive_download("https://docs.google.com/spreadsheets/d/1i3vuaoJOxKrynLZsjV8VGv3eslunMPbrGsrax-xCyVk/", "player-bio-database.csv", "csv", overwrite = TRUE)

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(glue)
library(DT)

dfs <- list(
  read_csv("allstats-20-21.csv") %>% mutate(SEASON = "20-21"),
  read_csv("allstats-21-22.csv") %>% mutate(SEASON = "21-22")
) %>% 
  bind_rows() %>% 
  mutate(DATE = mdy(DATE),
         FG = str_c(FGM, "-", FGA),
         `3P` = str_c(`3PM`, "-", `3PA`),
         FT = str_c(FTM, "-", FTA)) %>% 
  arrange(PLAYER, DATE)

bios <- read_csv("player-bio-database.csv", skip = 1) %>% 
  select(-Name...1) %>% 
  rename(Name = Name...2) %>% 
  mutate(DOB = mdy(DOB))

function(input, output, session) {
  
  updateSelectizeInput(session, 'foo', choices = sort(unique(dfs$PLAYER)), server = TRUE)
  
  myPlayerData <- reactive({
    dfs %>% 
      filter(PLAYER == input$name)
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
  
  # output$headshot <- renderUI({
  #   tags$img(src = myBiosData() %>% pull(`Img URL`), 
  #            width = 250, height = 200)
  # })
  output$headshot <- renderText({
    c('<img src="', myBiosData() %>% pull(`Img URL`), '">')
  })
  
  output$tbl <- renderDT({
    myPlayerData() %>% 
      select(
        DATE, TEAM, OPP, M, P, R, A, S, B, TO, 
        FG, `3P`, FT, PF, WL
      ) %>% 
      arrange(desc(DATE))
  })
  
  output$tbl_season <- renderDT({
    myPlayerData() %>% 
      group_by(SEASON, TEAM) %>% 
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
      )
  })
  
  options = list(
    pageLength = 25
  )
  
}