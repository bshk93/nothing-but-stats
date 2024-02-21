library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(lubridate)
library(rlang)
library(glue)
library(DT)
library(zoo)
library(data.table)
library(ggimage)
library(plotly)
library(reactable)
library(shinyWidgets)
library(bslib)
library(shinydashboard)


clean_allstats <- function(dfs) {
  dfs %>% 
    bind_rows() %>% 
    select(-any_of(c("...27", "V27"))) %>% 
    mutate(FG = str_c(FGM, "-", FGA),
           `3P` = str_c(`3PM`, "-", `3PA`),
           FT = str_c(FTM, "-", FTA),
           GMSC = P + (0.4 * FGM) - (0.7 * FGA) - (0.4 * (FTA - FTM)) + (0.7 * OR) + 
             (0.3 * DR) + S + (0.7 * A) + (0.7 * B) - (0.4 * PF) - TO,
           TS = 0.5*P/(FGA + .475*FTA)) %>% 
    mutate(PLAYER = case_when(
      PLAYER == "KANTER, ENES" ~ "FREEDOM, ENES",
      PLAYER == "BAMBA, MO" ~ "BAMBA, MOHAMED",
      PLAYER == "CAREY JR., VERNON" ~ "CAREY, VERNON",
      PLAYER == "CHAMAGNIE, JUSTIN" ~ "CHAMPAGNIE, JUSTIN",
      PLAYER == "HAMMONDS, RAYSHON" ~ "HAMMONDS, RAYSHAUN",
      PLAYER == "MATTHEWS, WES" ~ "MATTHEWS, WESLEY",
      PLAYER == "O'NEALE, ROYCE" ~ "ONEALE, ROYCE",
      PLAYER == "PIPPEN, SCOTTIE" ~ "PIPPEN, SCOTTY",
      PLAYER == "ROBINSON, GLENNN" ~ "ROBINSON, GLENN",
      PLAYER == "WHITE, COLBY" ~ "WHITE, COBY",
      PLAYER == "BERTANS,DAVIS" ~ "BERTANS, DAVIS",
      PLAYER == "HIGHSMITH, HAYDEN" ~ "HIGHSMITH, HAYWOOD",
      TRUE ~ PLAYER
    )) %>% 
    mutate(GMSC = round(GMSC, 2)) %>% 
    arrange(PLAYER, DATE)
}


summarize_per_game <- function(df) {
  df %>% 
    summarize(
      G     = n(),
      MPG   = mean(M) %>% round(2) %>% format(nsmall = 2),
      PPG   = mean(P) %>% round(2) %>% format(nsmall = 2),
      RPG   = mean(R) %>% round(2) %>% format(nsmall = 2),
      APG   = mean(A) %>% round(2) %>% format(nsmall = 2),
      SPG   = mean(S) %>% round(2) %>% format(nsmall = 2),
      BPG   = mean(B) %>% round(2) %>% format(nsmall = 2),
      FG    = (sum(FGM) / sum(FGA)) %>% round(3) %>% format(nsmall = 3),
      FGMPG = mean(FGM) %>% round(2) %>% format(nsmall = 2),
      FGAPG = mean(FGA) %>% round(2) %>% format(nsmall = 2),
      `3P`  = (sum(`3PM`) / sum(`3PA`)) %>% round(3) %>% format(nsmall = 3),
      `3PMPG` = mean(`3PM`) %>% round(2) %>% format(nsmall = 2),
      `3PAPG` = mean(`3PA`) %>% round(2) %>% format(nsmall = 2),
      FT    = (sum(FTM) / sum(FTA)) %>% round(3) %>% format(nsmall = 3),
      GMSC  = mean(GMSC) %>% round(2) %>% format(nsmall = 2),
      TS    = (0.5 * sum(P) / (sum(FGA) + .475*sum(FTA))) %>% round(3) %>% format(nsmall = 3)
    ) %>% 
    ungroup()
}


summarize_team_season <- function(dfs) {
  
  dfs %>% 
    group_by(TEAM, SEASON, DATE) %>% 
    summarize(
      P = sum(P),
      R = sum(R),
      A = sum(A),
      S = sum(S),
      B = sum(B),
      FGM = sum(FGM),
      FGA = sum(FGA),
      `3PM` = sum(`3PM`),
      `3PA` = sum(`3PA`),
      FTM = sum(FTM),
      FTA = sum(FTA),
      GMSC = sum(GMSC),
      TEAM_PTS = max(TEAM_PTS),
      OPP_TEAM_PTS = max(OPP_TEAM_PTS)
    ) %>% 
    mutate(DIFF = TEAM_PTS - OPP_TEAM_PTS) %>% 
    
    group_by(TEAM, SEASON) %>% 
    summarize(
      P = sum(P),
      R = sum(R),
      A = sum(A),
      S = sum(S),
      B = sum(B),
      FGM = sum(FGM),
      FGA = sum(FGA),
      `3PM` = sum(`3PM`),
      `3PA` = sum(`3PA`),
      FTM = sum(FTM),
      FTA = sum(FTA),
      GMSC = sum(GMSC),
      DIFF = sum(DIFF)
    )
  
}


get_ranks <- function(dfs) {
  dfs %>% 
    group_by(PLAYER, SEASON) %>% 
    summarize(
      G = n(),
      M = sum(M),
      P = sum(P),
      R = sum(R),
      A = sum(A),
      S = sum(S),
      B = sum(B),
      MPG = sum(M)/G,
      PPG = sum(P)/G,
      RPG = sum(R)/G,
      APG = sum(A)/G,
      SPG = sum(S)/G,
      BPG = sum(B)/G,
      `3PM` = sum(`3PM`),
      FGPCT = sum(FGM)/sum(FGA),
      `3PPCT` = sum(`3PM`)/sum(`3PA`),
      FTPCT = sum(FTM)/sum(FTA),
      GMSC = mean(GMSC),
      PCT = sum(WL == "W")/n()
    ) %>% 
    group_by(SEASON) %>% 
    mutate(
      G_RANK = rank(desc(G), ties.method = "min"),
      M_RANK = rank(desc(M), ties.method = "min"),
      P_RANK = rank(desc(P), ties.method = "min"),
      R_RANK = rank(desc(R), ties.method = "min"),
      A_RANK = rank(desc(A), ties.method = "min"),
      S_RANK = rank(desc(S), ties.method = "min"),
      B_RANK = rank(desc(B), ties.method = "min"),
      `3PM_RANK` = rank(desc(`3PM`), ties.method = "min"),
      FGPCT_RANK = rank(desc(FGPCT), ties.method = "min", na.last = "keep"),
      `3PPCT_RANK` = rank(desc(`3PPCT`), ties.method = "min", na.last = "keep"),
      FTPCT_RANK = rank(desc(FTPCT), ties.method = "min", na.last = "keep"),
      MPG_RANK = rank(desc(MPG), ties.method = "min"),
      PPG_RANK = rank(desc(PPG), ties.method = "min"),
      RPG_RANK = rank(desc(RPG), ties.method = "min"),
      APG_RANK = rank(desc(APG), ties.method = "min"),
      SPG_RANK = rank(desc(SPG), ties.method = "min"),
      BPG_RANK = rank(desc(BPG), ties.method = "min"),
      GMSC_RANK = rank(desc(GMSC), ties.method = "min")
    ) %>% 
    ungroup()
}

get_box_score <- function(dfs_everything, boxscoredate, boxscore_output) {
  message(glue('getting boxscores for {boxscore_output}'))
  x <- dfs_everything %>% 
    filter(DATE == boxscoredate,
           TEAM == str_extract(boxscore_output, "^[A-Z]{3}") | TEAM == str_extract(boxscore_output, "[A-Z]{3}$")) %>% 
    arrange(OPP, desc(P))
  
  my_teams <- x %>% distinct(TEAM) %>% pull(TEAM)
  
  x %>% 
    group_by(TEAM) %>% 
    summarize_at(c("M", "P", "R", "A", "S", "B", "TO", "PF", "FGM", "FGA", "3PM", "3PA", "FTM", "FTA"), sum) %>% 
    mutate(FG = str_c(FGM, "-", FGA), `3P` = str_c(`3PM`, "-", `3PA`), FT = str_c(FTM, "-", FTA),
           GMSC = NA_real_, WL = NA_character_, PLAYER = TEAM) %>%
    select(-FGM, -FGA, -FTM, -FTA, -`3PM`, -`3PA`) %>% 
    rbind(x %>% select(TEAM, PLAYER, M, P, R, A, S, B, TO, PF, FG, `3P`, FT, GMSC, WL), .) %>% 
    arrange(TEAM) %>% 
    select(TEAM, PLAYER, M, P, R, A, S, B, TO, PF, FG, `3P`, FT, GMSC, WL) %>% 
    datatable(rownames = FALSE, options = list(pageLength = 30)) %>% 
    formatStyle(
      "PLAYER",
      target = "row",
      fontWeight = styleEqual(my_teams, c("bold", "bold"), default = "normal")
    )
}


# Miscellaneous ----
allteams <- c("ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DAL", "DEN", "DET", "GSW",
              "HOU", "IND", "LAC", "LAL", "MEM", "MIA", "MIL", "MIN", "NOP", "NYK",
              "OKC", "ORL", "PHI", "PHX", "POR", "SAC", "SAS", "TOR", "UTA", "WAS")

get_team_color <- function(team) {
  switch(team,
         "MIL" = "#00471B",
         "IND" = "#FDBB30",
         "BOS" = "#007A33",
         "BKN" = "#000000",
         "ATL" = "#C1D32F",
         "ORL" = "#0077c0",
         "MIA" = "#db3eb1",
         "PHI" = "#006bb6",
         "WAS" = "#c6ac6a",
         "TOR" = "#ce1141",
         "CHI" = "#ed0808",
         "CHA" = "#0bebed",
         "CLE" = "#860038",
         "NYK" = "#F58426",
         "DET" = "#1d42ba",
         "HOU" = "#ff0000",
         "SAC" = "#9300ff",
         "GSW" = "#0000ff",
         "LAL" = "#663399",
         "DAL" = "#B8C4CA",
         "LAC" = "#C8102E",
         "MIN" = "#78BE20",
         "POR" = "#E03A3E",
         "DEN" = "#FEC524",
         "NOP" = "#85714D",
         "PHX" = "#ff9900",
         "OKC" = "#ef3b24",
         "SAS" = "#FF69B4",
         "UTA" = "#008d36",
         "MEM" = "#5D76A9")
}

vget_team_color <- Vectorize(get_team_color)

get_conference <- function(team) {
  case_when(
    team %in% c("MIL", "IND", "BOS", "BKN", "ATL", "ORL", "MIA", "PHI", "WAS", 
                "TOR", "CHI", "CHA", "CLE", "NYK", "DET") ~ "East",
    team %in% c("HOU", "SAC", "GSW", "LAL", "DAL", "LAC", "MIN", "POR", "DEN",
                "NOP", "PHX", "OKC", "SAS", "UTA", "MEM") ~ "West"
  )
}

get_last_played_for <- function(player, dfs) {
  
  dfs %>% 
    filter(PLAYER == player) %>% 
    arrange(DATE) %>% 
    tail(1) %>% 
    pull(TEAM)
  
}

vget_last_played_for <- Vectorize(get_last_played_for)
