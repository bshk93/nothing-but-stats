library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(DT)

dfs <- list(
  read_csv("allstats-20-21.csv") %>% mutate(SEASON = "20-21"),
  read_csv("allstats-21-22.csv") %>% mutate(SEASON = "21-22")
) %>% 
  bind_rows()

fluidPage(
  
  titlePanel("Gamelog Explorer"),
  
  htmlOutput("headshot"),
  
  sidebarPanel(
    
    selectizeInput(
      'name',
      'Name',
      sort(unique(dfs$PLAYER)),
      selected = NULL,
      multiple = FALSE,
      options = NULL
    ),
    
    verbatimTextOutput("player_summary")
    
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Gamelog", DTOutput("tbl")),
      tabPanel("Season-by-Season", DTOutput("tbl_season"))
    )
  )
)