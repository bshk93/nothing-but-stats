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


load_allstats <- function(playoffs = FALSE) {
  ptrn <- "allstats-\\d"
  pstr <- ""
  if (playoffs) {
    ptrn <- "allstats-playoffs"
    pstr <- " Playoffs"
  }
  
  # Find all allstats files in R/ directory
  list.files("data/", ptrn) %>% 
    map(function(fp) {
      tmp_season <- as.numeric(str_extract(fp, "\\d{2}\\."))
      
      data.table::fread(str_c('./data/', fp)) %>% 
        tibble() %>% 
        mutate_if(is.numeric, as.numeric) %>% 
        mutate(SEASON = str_c(tmp_season-1, "-", tmp_season, pstr)) %>% 
        filter(!is.na(SEASON))
    })
}


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


get_achievements_season <- function(player_df, dfs, playername, ach_metadata) {
  # Carry Job
  ach_carry_job <- dfs %>% 
    inner_join(
      player_df %>% 
        distinct(SEASON, DATE, TEAM),
      by = c("SEASON", "DATE", "TEAM")
    ) %>% 
    group_by(SEASON, DATE, TEAM) %>% 
    summarize(TEAM_PTS = sum(P),
              PLAYER_PTS = sum(case_when(PLAYER == playername ~ P, TRUE ~ 0))) %>% 
    group_by(SEASON) %>% 
    summarize(TEAM_PTS = sum(TEAM_PTS), 
              PLAYER_PTS = sum(PLAYER_PTS)) %>% 
    mutate(ACHIEVEMENT = "Carry Job") %>% 
    mutate(PCT = PLAYER_PTS / TEAM_PTS) %>% 
    filter(PCT >= .3) %>% 
    select(ACHIEVEMENT, SEASON)
  
  # The Waiter
  ach_waiter <- dfs %>% 
    inner_join(
      player_df %>% 
        distinct(SEASON, DATE, TEAM),
      by = c("SEASON", "DATE", "TEAM")
    ) %>% 
    group_by(SEASON, DATE, TEAM) %>% 
    summarize(TEAM_A = sum(A),
              PLAYER_A = sum(case_when(PLAYER == playername ~ A, TRUE ~ 0))) %>% 
    group_by(SEASON) %>% 
    summarize(TEAM_A = sum(TEAM_A), 
              PLAYER_A = sum(PLAYER_A)) %>% 
    mutate(PCT = PLAYER_A / TEAM_A) %>% 
    filter(PCT >= .3) %>% 
    mutate(ACHIEVEMENT = "The Waiter") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # I'm Him
  ach_im_him <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(P = sum(P)) %>% 
    group_by(SEASON) %>% 
    filter(P == max(P)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "I'm Him") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Jake from State Farm
  ach_jake <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(A = sum(A)) %>% 
    group_by(SEASON) %>% 
    filter(A == max(A)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Jake from State Farm") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Director of Boards
  ach_dob <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(R = sum(R)) %>% 
    group_by(SEASON) %>% 
    filter(R == max(R)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Director of Boards") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Steal Yo Girl
  ach_steal_yo_girl <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(S = sum(S)) %>% 
    group_by(SEASON) %>% 
    filter(S == max(S)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Mr. Steal Yo Girl") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Turn Down the Sliders
  ach_sliders <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(B = sum(B)) %>% 
    group_by(SEASON) %>% 
    filter(B == max(B)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Turn Down the Sliders") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Splash Brother
  ach_splash <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(`3PM` = sum(`3PM`)) %>% 
    group_by(SEASON) %>% 
    filter(`3PM` == max(`3PM`)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Splash Brother") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Enforcer
  ach_enforcer <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(PF = sum(PF)) %>% 
    group_by(SEASON) %>% 
    filter(PF == max(PF)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Enforcer") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Tank Commander
  ach_tank <- dfs %>% 
    filter(PLAYER == playername) %>% 
    group_by(SEASON) %>% 
    summarize(W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0))) %>% 
    left_join(
      dfs %>% 
        group_by(SEASON) %>% 
        summarize(
          M = sum(case_when(PLAYER == playername ~ M, TRUE ~ 0)),
          G = sum(case_when(PLAYER == playername ~ 1, TRUE ~ 0)),
          MPG = M/G
        ),
      by = "SEASON"
    ) %>% 
    filter(MPG >= 25, W <= 0.25*G, G >= 50) %>% 
    mutate(ACHIEVEMENT = "Tank Commander") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Hot Potato
  ach_hot_potato <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(TEAMS = n_distinct(TEAM)) %>% 
    filter(TEAMS > 2) %>% 
    mutate(ACHIEVEMENT = "Hot Potato") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Singler Line
  ach_singler <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(
      FG = sum(FGM)/sum(FGA),
      `3P` = sum(`3PM`)/sum(`3PA`),
      FT = sum(FTM)/sum(FTA)
    ) %>% 
    filter(FG + `3P` + FT < 1) %>% 
    mutate(ACHIEVEMENT = "The Singler Line") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # 50/40/90 Club
  ach_504090 <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(
      FG = sum(FGM)/sum(FGA),
      `3P` = sum(`3PM`)/sum(`3PA`),
      FT = sum(FTM)/sum(FTA)
    ) %>% 
    filter(FG >= .5, `3P` >= .4, FT >= .9) %>% 
    mutate(ACHIEVEMENT = "50/40/90 Club") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # 2K Club
  ach_2k <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(P = sum(P)) %>% 
    filter(P >= 2000) %>% 
    mutate(ACHIEVEMENT = "2K Club") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Sharing is Caring
  ach_sharing <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(A = sum(A)) %>% 
    filter(A >= 700) %>% 
    mutate(ACHIEVEMENT = "Sharing is Caring") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # GETDAFUCKOUTTAHEREIGOTIT
  ach_getdaf <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(R = sum(R)) %>% 
    filter(R >= 1000) %>% 
    mutate(ACHIEVEMENT = "GETDAFUCKOUTTAHEREIGOTIT") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Grand Theft Auto
  ach_gta <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(S = sum(S)) %>% 
    filter(S >= 150) %>% 
    mutate(ACHIEVEMENT = "Grand Theft Auto") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Send It Back
  ach_senditback <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(B = sum(B)) %>% 
    filter(B >= 1000) %>% 
    mutate(ACHIEVEMENT = "Send It Back") %>% 
    select(ACHIEVEMENT, SEASON)
  
  ach_metadata %>% 
    inner_join(
      bind_rows(
        ach_carry_job,
        ach_waiter,
        ach_im_him,
        ach_jake,
        ach_dob,
        ach_steal_yo_girl,
        ach_sliders,
        ach_splash,
        ach_enforcer,
        ach_tank,
        ach_hot_potato,
        ach_singler,
        ach_504090,
        ach_2k,
        ach_sharing,
        ach_getdaf,
        ach_gta,
        ach_senditback
      ),
      by = "ACHIEVEMENT"
    ) %>% 
    datatable(
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
        "ACHIEVEMENTS (SEASON)"
      ),
      rownames = FALSE
    )
}


get_achievements_game <- function(combined_df, ach_metadata) {
  # The Snell Award
  ach_snell <- combined_df %>% 
    filter(M >= 28, P + A + R + B + S == 0) %>% 
    mutate(ACHIEVEMENT = "The Snell Award") %>% 
    select(ACHIEVEMENT, DATE)
  
  # Stinker
  ach_stinker <- combined_df %>% 
    filter(GMSC <= -10) %>% 
    mutate(ACHIEVEMENT = "Stinker") %>% 
    select(ACHIEVEMENT, DATE)
  
  # One Man Show
  ach_oneman <- combined_df %>% 
    filter(GMSC >= 50) %>% 
    mutate(ACHIEVEMENT = "One Man Show") %>% 
    select(ACHIEVEMENT, DATE)
  
  # Specialist
  ach_specialist <- combined_df %>% 
    filter(P >= 21, FTM == 0, FGM == `3PM`) %>%  
    mutate(ACHIEVEMENT = "Specialist") %>% 
    select(ACHIEVEMENT, DATE)
  
  # Well-Rounded
  ach_wellrounded <- combined_df %>% 
    filter(P >= 5, R >= 5, A >= 5, S >= 5, B >= 5) %>%  
    mutate(ACHIEVEMENT = "Well-Rounded") %>% 
    select(ACHIEVEMENT, DATE)
  
  # Various point thresholds
  ach_pts_all <- combined_df %>% 
    filter(P >= 70) %>% 
    mutate(ACHIEVEMENT = case_when(
      P >= 100 ~ "Son of Wilt",
      P >= 81 ~ "Mamba Mentality",
      TRUE ~ "Empty Calories"
    )) %>% 
    select(ACHIEVEMENT, DATE)
  
  ach_metadata %>% 
    inner_join(
      bind_rows(
        ach_snell,
        ach_stinker,
        ach_oneman,
        ach_specialist,
        ach_wellrounded,
        #ach_triplesingle,
        ach_pts_all
      ),
      by = "ACHIEVEMENT"
    ) %>% 
    group_by(TYPE, ACHIEVEMENT, DESCRIPTION) %>% 
    summarize(DATES = str_c(DATE, collapse = ", ")) %>% 
    datatable(
      caption = htmltools::tags$caption(
        style = 'caption-side: top; text-align: left; color:black; font-size:200% ;',
        "ACHIEVEMENTS (GAME)"
      ),
      rownames = FALSE
    )
}

build_prices <- function(dfs_everything) {
  x <- dfs_everything %>% 
    mutate(OPP_RAW = str_replace(OPP, "@", "")) %>%
    group_by(SEASON, TEAM, OPP, OPP_RAW, DATE) %>% 
    summarize(P = sum(P)) %>% 
    ungroup()
  
  z <- x %>%
    inner_join(
      x %>% select(OPP_RAW = TEAM, DATE, OPP_P = P),
      by = c('OPP_RAW', 'DATE')
    ) %>%
    mutate(DIFF = P - OPP_P) %>%
    group_by(TEAM, SEASON) %>%
    arrange(TEAM, DATE) %>%
    mutate(CUM_DIFF = cumsum(DIFF),
           LAG_CUM_DIFF = lag(CUM_DIFF),
           G = row_number()) %>%
    ungroup
  
  z %>%
    inner_join(
      z %>% select(OPP_RAW = TEAM, DATE, OPP_LAG_CUM_DIFF = LAG_CUM_DIFF, OPP_G = G),
      by = c('OPP_RAW', 'DATE')
    ) %>%
    mutate(OPP_AVG_DIFF = OPP_LAG_CUM_DIFF / (OPP_G - 1),
           DIFF_DIFF = DIFF + OPP_AVG_DIFF) %>%
    
    group_by(TEAM) %>%
    
    mutate(
      CUM_DIFF_DIFF = cumsum(coalesce(DIFF_DIFF, 0))
    ) %>%
    ungroup() %>% 
    
    mutate(PCT_CHG = coalesce(1 + (DIFF_DIFF / 1000), 1)) %>%
    group_by(TEAM) %>%
    arrange(TEAM, DATE) %>%
    mutate(PRICE = round(cumprod(PCT_CHG) * 100, 2),
           N = row_number(),
           DATE = as_date(DATE))
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
