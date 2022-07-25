# setwd("C:/Users/bshk9/OneDrive/home/projects/nbn/nothing-but-stats")

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
  
  output$player_summary <- renderText({
    glue(
      "{input$name}:\n",
      "DOB: {bios %>% filter(Name == input$name) %>% pull(DOB)}\n",
      "Age: {round(time_length(interval(bios %>% filter(Name == input$name) %>% pull(DOB), today()), 'years'), 2)}\n",
      "Height: {bios %>% filter(Name == input$name) %>% pull(Height)}\n",
      "Weight: {bios %>% filter(Name == input$name) %>% pull(Weight)}\n",
      "From: {bios %>% filter(Name == input$name) %>% pull(COLLEGE)}"
    )
  })
  
  output$headshot <- renderUI({
    tags$img(src = "https://cdn.nba.com/headshots/nba/latest/1040x760/1630322.png",
             width = 200, height = 200)
  })
  
  output$tbl <- renderDT({
    dfs %>% 
      filter(PLAYER == input$name) %>% 
      select(
        DATE, TEAM, OPP, M, P, R, A, S, B, TO, 
        FG, `3P`, FT, PF, WL
      ) %>% 
      arrange(desc(DATE))
  })
  
  output$tbl_season <- renderDT({
    dfs %>% 
      filter(PLAYER == input$name) %>% 
      group_by(SEASON) %>% 
      summarize(
        G = n(),
        M = mean(M) %>% round(2),
        P = mean(P) %>% round(2),
        R = mean(R) %>% round(2),
        A = mean(A) %>% round(2),
        S = mean(S) %>% round(2),
        B = mean(B) %>% round(2)
      )
  })
  
  options = list(
    pageLength = 25
  )
  
}