get_newsfeed <- function(dfs, gmsc_thresh = 35) {
  
  feed_gmsc <- dfs %>% 
    filter(GMSC >= gmsc_thresh | TD == 1) %>% 
    
    mutate(HEADLINE = str_c(
      PLAYER, " <img src='logo-", tolower(TEAM), ".png' height='20'></img>",
      ' recorded ', P, ' points, ', 
      A, ' assists, ', R, ' rebounds, ', S, ' steals, and ',
      B, ' blocks on ', FGM, '-', FGA, ' shooting in a ',
      TEAM_PTS, '-', OPP_TEAM_PTS, 
      case_when(WL == 'W' ~ ' win over ', TRUE ~ ' loss to '),
      OPP, '.'
    )) %>% 
    select(SEASON, PLAYER, DATE, HEADLINE)
  
  records <- dfs %>% 
    arrange(DATE) %>% 
    mutate(
      RECORD_P = cummax(P),
      RECORD_R = cummax(R),
      RECORD_A = cummax(A),
      RECORD_S = cummax(S),
      RECORD_B = cummax(B)
    )
    
  
  feed_records <- map_dfr(
    c('P', 'R', 'A', 'S', 'B'),
    function(var) {
      record_var = str_c('RECORD_', var)
      
      records %>% 
        filter(get(var) >= lag(get(record_var))) %>% 
        mutate(HEADLINE = str_c(
          PLAYER, " <img src='logo-", tolower(TEAM), ".png' height='20'></img>",
          ' recorded an NBN record ', get(var), ' ', var, ' in a ',
          TEAM_PTS, '-', OPP_TEAM_PTS, 
          case_when(WL == 'W' ~ ' win over ', TRUE ~ ' loss to '),
          OPP, '.'
        )) %>% 
        select(SEASON, PLAYER, DATE, HEADLINE)
    }
  )
  
  highs <- dfs %>% 
    arrange(DATE) %>% 
    group_by(PLAYER) %>% 
    mutate(
      RECORD_P = cummax(P),
      RECORD_R = cummax(R),
      RECORD_A = cummax(A),
      RECORD_S = cummax(S),
      RECORD_B = cummax(B)
    )
  
  feed_highs <- map_dfr(
    c('P', 'R', 'A', 'S', 'B'),
    function(var) {
      record_var = str_c('RECORD_', var)
      
      x <- highs %>% 
        filter(get(var) >= lag(get(record_var))) %>% 
        mutate(HEADLINE = str_c(
          PLAYER, " <img src='logo-", tolower(TEAM), ".png' height='20'></img>",
          ' recorded a career high of ', get(var), ' ', var, ' in a ',
          TEAM_PTS, '-', OPP_TEAM_PTS, 
          case_when(WL == 'W' ~ ' win over ', TRUE ~ ' loss to '),
          OPP, '.'
        ))
      
      if (var == 'P') {
        x <- filter(x, get(var) >= 20)
      } else if (var %in% c('A', 'R')) {
        x <- filter(x, get(var) >= 10)
      } else {
        x <- filter(x, get(var) >= 5)
      }
        
      x %>% 
        select(SEASON, PLAYER, DATE, HEADLINE)
    }
  )
  
  career_totals <- dfs %>% 
    group_by(PLAYER) %>% 
    arrange(PLAYER, DATE) %>% 
    mutate(
      CAREER_P = cumsum(P),
      CAREER_R = cumsum(R),
      CAREER_A = cumsum(A),
      CAREER_S = cumsum(S),
      CAREER_B = cumsum(B),
      CAREER_3PM = cumsum(`3PM`)
    )
  
  feed_milestones_career <- map_dfr(
    c('P', 'R', 'A', 'S', 'B', '3PM'),
    function(var) {
      career_var <- str_c('CAREER_', var)
      
      x <- career_totals %>% 
        filter(floor(get(career_var) / 500) > coalesce(floor(lag(get(career_var))/ 500), 0)) %>% 
        filter(get(career_var) >= 500) %>% 
        mutate(HEADLINE = str_c(
          PLAYER, " <img src='logo-", tolower(TEAM), ".png' height='20'></img>",
          ' reached a milestone of ', get(career_var), ' career ', var, '.'
        ))
      
      x %>% 
        select(SEASON, PLAYER, DATE, HEADLINE)
    }
  )
    
  
  bind_rows(
    feed_gmsc, 
    feed_records, 
    feed_highs,
    feed_milestones_career
  ) %>% 
    arrange(desc(DATE))
  
}