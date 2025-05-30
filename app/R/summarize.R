summarize_player <- function(df) {
  
  df %>% 
    group_by(PLAYER) %>% 
    summarize_per_game(formatting = FALSE)
  
}

summarize_team <- function(df) {
  
  df %>% 
    group_by(TEAM, SEASON, DATE) %>% 
    summarize_if(is.numeric, sum) %>% 
    group_by(TEAM, SEASON) %>% 
    summarize_per_game(formatting = FALSE)
  
}




summarize_per_game <- function(df, formatting = TRUE) {
  # x <- df %>% 
  #   summarize(
  #     G     = n(),
  #     WL    = str_c(
  #       sum(case_when(WL == 'W' ~ 1, TRUE ~ 0)),
  #       "-",
  #       sum(case_when(WL == 'L' ~ 1, TRUE ~ 0))
  #     ),
  #     MPG   = mean(M) %>% round(2),
  #     PPG   = mean(P) %>% round(2),
  #     RPG   = mean(R) %>% round(2),
  #     APG   = mean(A) %>% round(2),
  #     SPG   = mean(S) %>% round(2),
  #     BPG   = mean(B) %>% round(2),
  #     FG    = (sum(FGM) / sum(FGA)) %>% round(3),
  #     FGMPG = mean(FGM) %>% round(2),
  #     FGAPG = mean(FGA) %>% round(2),
  #     `3P`  = (sum(`3PM`) / sum(`3PA`)) %>% round(3),
  #     `3PMPG` = mean(`3PM`) %>% round(2),
  #     `3PAPG` = mean(`3PA`) %>% round(2),
  #     FT    = (sum(FTM) / sum(FTA)) %>% round(3),
  #     FTMPG = mean(FTM) %>% round(2),
  #     FTAPG = mean(FTA) %>% round(2),
  #     GMSC  = mean(GMSC) %>% round(2),
  #     TS    = (0.5 * sum(P) / (sum(FGA) + .475*sum(FTA))) %>% round(3)
  #   ) %>% 
  #   ungroup()
  
  x <- df %>%
    summarize(
      G     = n(),
      W     = sum(WL == "W"),
      L     = sum(WL == "L"),
      WL    = str_c(W, "-", L),
      MPG   = round(mean(M), 2),
      PPG   = round(mean(P), 2),
      RPG   = round(mean(R), 2),
      APG   = round(mean(A), 2),
      SPG   = round(mean(S), 2),
      BPG   = round(mean(B), 2),
      FG    = round(sum(FGM) / sum(FGA), 3),
      FGMPG = round(mean(FGM), 2),
      FGAPG = round(mean(FGA), 2),
      `3P`  = round(sum(`3PM`) / sum(`3PA`), 3),
      `3PMPG` = round(mean(`3PM`), 2),
      `3PAPG` = round(mean(`3PA`), 2),
      FT    = round(sum(FTM) / sum(FTA), 3),
      FTMPG = round(mean(FTM), 2),
      FTAPG = round(mean(FTA), 2),
      GMSC  = round(mean(GMSC), 2),
      TS    = round(0.5 * sum(P) / (sum(FGA) + 0.475 * sum(FTA)), 3)
    ) %>%
    select(-W, -L) %>% 
    ungroup()
  
  if (formatting) {
    x <- x %>% 
      mutate_at(
        c('MPG', 'PPG', 'RPG', 'APG', 'SPG', 'BPG', 'FGMPG', 'FGAPG', 
          '3PMPG', '3PAPG', 'GMSC'),
        format,
        n_small = 2
      ) %>% 
      mutate_at(
        c('FG', '3P', 'FT', 'TS'),
        format,
        n_small = 3
      )
  }
  
  x
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