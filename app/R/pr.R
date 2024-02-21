
calculate_power_rankings <- function(dfs) {
  
  x <- dfs %>% 
    #filter(SEASON == input$pr_season) %>% 
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
  
  diffs <- z %>%
    inner_join(
      z %>% select(OPP_RAW = TEAM, DATE, OPP_LAG_CUM_DIFF = LAG_CUM_DIFF, OPP_G = G),
      by = c('OPP_RAW', 'DATE')
    ) %>%
    mutate(OPP_AVG_DIFF = OPP_LAG_CUM_DIFF / (OPP_G - 1),
           DIFF_DIFF = DIFF + OPP_AVG_DIFF) %>%
    
    group_by(TEAM, SEASON) %>%
    
    mutate(
      CUM_DIFF_DIFF = cumsum(coalesce(DIFF_DIFF, 0))
    ) %>%
    ungroup()
  
  daily_diffs <- diffs %>%
    distinct(SEASON, DATE) %>%
    full_join(distinct(diffs, TEAM), by = character()) %>%
    left_join(
      diffs %>% select(DATE, TEAM, CUM_DIFF_DIFF),
      by = c('DATE', 'TEAM')
    ) %>%
    
    group_by(SEASON, TEAM) %>%
    arrange(SEASON, TEAM, DATE) %>%
    fill(CUM_DIFF_DIFF) %>%
    ungroup()
  
  daily_diffs %>%
    filter(!is.na(CUM_DIFF_DIFF)) %>%
    group_by(SEASON, DATE) %>%
    filter(n() == 30) %>%
    
    mutate(TEAM_PR = rank(desc(CUM_DIFF_DIFF), ties.method = "min"),
           COLOR = vget_team_color(TEAM)) %>%
    ungroup()
  
}

calculate_team_offense_defense <- function(dfs) {
  
  x <- dfs %>% 
    #filter(SEASON == input$pr_season) %>% 
    mutate(OPP_RAW = str_replace(OPP, "@", "")) %>%
    group_by(SEASON, TEAM, OPP, OPP_RAW, DATE) %>% 
    summarize(P = sum(P)) %>% 
    ungroup()
  
  y <- x %>% 
    group_by(TEAM, SEASON) %>% 
    arrange(TEAM, SEASON, DATE) %>% 
    mutate(CUM_PPG = cumsum(P)/row_number()) %>% 
    ungroup()
  
  z <- y %>%
    inner_join(
      y %>% select(OPP_RAW = TEAM, DATE, OPP_P = P, OPP_CUM_PPG = CUM_PPG),
      by = c('OPP_RAW', 'DATE')
    ) %>%
    
    group_by(SEASON, TEAM) %>% 
    arrange(SEASON, TEAM, DATE) %>% 
    mutate(CUM_ALLOWED = cumsum(OPP_P)/row_number()) %>% 
    ungroup()
  
  z2 <- z %>% 
    inner_join(
      z %>% select(OPP_RAW = TEAM, DATE, OPP_CUM_ALLOWED = CUM_ALLOWED),
      by = c('OPP_RAW', 'DATE')
    ) %>% 
    
    mutate(
      DIFF_OFF = P - OPP_CUM_ALLOWED,
      DIFF_DEF = OPP_CUM_PPG - OPP_P
    )
  
  z2 %>% 
    group_by(TEAM, SEASON) %>% 
    summarize(
      OFF_RTG = mean(DIFF_OFF),
      DEF_RTG = mean(DIFF_DEF),
      TOT_RTG = OFF_RTG + DEF_RTG
    )
  
}