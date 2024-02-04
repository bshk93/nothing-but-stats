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
    
  
  bind_rows(
    feed_gmsc, 
    feed_records, 
    feed_highs
  ) %>% 
    arrange(desc(DATE))
  
}