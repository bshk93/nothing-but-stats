library(shiny)
library(ggplot2)
library(tidyverse)
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
      'Name (For Player Stats)',
      named_names
    ),
    
    verbatimTextOutput("player_summary"),
    
    selectizeInput(
      'team',
      'Team (for NBA/Franchise Records)',
      c("NBA", "ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
        "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
        "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")
    )
    
  ),
  
  mainPanel(
    tabsetPanel(
      type = "tabs",
      tabPanel("Season-by-Season",      DTOutput("tbl_season")),
      tabPanel("Gamelog",               DTOutput("tbl")),
      tabPanel("Personal Records",      DTOutput("records")),
      tabPanel("Trailing 10-game GMSC", plotOutput("gmsc_plot")),
      tabPanel(
        "All-Time GMSC Freq",
        plotOutput("gmsc_histogram"),
        selectizeInput("comp", "Compare", named_names), 
        plotOutput("gmsc_histogram_comp")
      ),
      
      tabPanel(
        "NBA/Franchise Records",
        DTOutput("franchise_records")
      )
    )
  )
)