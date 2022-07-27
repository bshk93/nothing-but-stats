library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(DT)

dfs <- list(
  read_csv("allstats-20-21.csv", show_col_types = F) %>% mutate(SEASON = "20-21"),
  read_csv("allstats-21-22.csv", show_col_types = F) %>% mutate(SEASON = "21-22")
) %>% 
  bind_rows()

player_teams <- read_csv("player-bio-database.csv", skip = 1, show_col_types = F) %>% 
  select(-Name...1) %>% 
  rename(Name = Name...2) %>% 
  distinct(Name, Team)

named_names <- player_teams$Name %>% 
  set_names(str_c(player_teams$Name, " (", coalesce(player_teams$Team, "NONE"), ")"))

fluidPage(
  
  titlePanel("Gamelog Explorer"),
  
  sidebarPanel(
    
    htmlOutput("headshot"),
    
    selectizeInput(
      'name',
      'Name',
      named_names,
      selected = NULL,
      multiple = FALSE,
      options = NULL
    ),
    
    verbatimTextOutput("player_summary")
    
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Season-by-Season",      DTOutput("tbl_season")),
      tabPanel("Gamelog",               DTOutput("tbl")),
      tabPanel("Trailing 10-game GMSC", plotOutput("gmsc_plot")),
      tabPanel("All-Time GMSC Freq",    plotOutput("gmsc_histogram"))
    )
  )
)