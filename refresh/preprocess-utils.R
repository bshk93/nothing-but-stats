# News ----
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

# Achievements ----
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
              PLAYER_PTS = sum(case_when(PLAYER == playername ~ P, TRUE ~ 0)),
              .groups = "drop") %>% 
    group_by(SEASON) %>% 
    summarize(TEAM_PTS = sum(TEAM_PTS), 
              PLAYER_PTS = sum(PLAYER_PTS),
              .groups = "drop") %>% 
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
              PLAYER_A = sum(case_when(PLAYER == playername ~ A, TRUE ~ 0)),
              .groups = "drop") %>% 
    group_by(SEASON) %>% 
    summarize(TEAM_A = sum(TEAM_A), 
              PLAYER_A = sum(PLAYER_A),
              .groups = "drop") %>% 
    mutate(PCT = PLAYER_A / TEAM_A) %>% 
    filter(PCT >= .3) %>% 
    mutate(ACHIEVEMENT = "The Waiter") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # I'm Him
  ach_im_him <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(P = sum(P),
              .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(P == max(P)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "I'm Him") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Jake from State Farm
  ach_jake <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(A = sum(A), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(A == max(A)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Jake from State Farm") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Director of Boards
  ach_dob <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(R = sum(R), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(R == max(R)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Director of Boards") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Steal Yo Girl
  ach_steal_yo_girl <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(S = sum(S), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(S == max(S)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Mr. Steal Yo Girl") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Turn Down the Sliders
  ach_sliders <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(B = sum(B), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(B == max(B)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Turn Down the Sliders") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Splash Brother
  ach_splash <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(`3PM` = sum(`3PM`), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(`3PM` == max(`3PM`)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Splash Brother") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Enforcer
  ach_enforcer <- dfs %>% 
    group_by(SEASON, PLAYER) %>% 
    summarize(PF = sum(PF), .groups = "drop") %>% 
    group_by(SEASON) %>% 
    filter(PF == max(PF)) %>% 
    filter(PLAYER == playername) %>% 
    mutate(ACHIEVEMENT = "Enforcer") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Tank Commander
  ach_tank <- dfs %>% 
    filter(PLAYER == playername) %>% 
    group_by(SEASON) %>% 
    summarize(W = sum(case_when(WL == "W" ~ 1, TRUE ~ 0)),
              .groups = "drop") %>% 
    left_join(
      dfs %>% 
        group_by(SEASON) %>% 
        summarize(
          M = sum(case_when(PLAYER == playername ~ M, TRUE ~ 0)),
          G = sum(case_when(PLAYER == playername ~ 1, TRUE ~ 0)),
          MPG = M/G,
          .groups = "drop"
        ),
      by = "SEASON"
    ) %>% 
    filter(MPG >= 25, W <= 0.25*G, G >= 50) %>% 
    mutate(ACHIEVEMENT = "Tank Commander") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Hot Potato
  ach_hot_potato <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(TEAMS = n_distinct(TEAM),
              .groups = "drop") %>% 
    filter(TEAMS > 2) %>% 
    mutate(ACHIEVEMENT = "Hot Potato") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Singler Line
  ach_singler <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(
      FG = sum(FGM)/sum(FGA),
      `3P` = sum(`3PM`)/sum(`3PA`),
      FT = sum(FTM)/sum(FTA),
      .groups = "drop"
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
      FT = sum(FTM)/sum(FTA),
      .groups = "drop"
    ) %>% 
    filter(FG >= .5, `3P` >= .4, FT >= .9) %>% 
    mutate(ACHIEVEMENT = "50/40/90 Club") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # 2K Club
  ach_2k <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(P = sum(P), .groups = "drop") %>% 
    filter(P >= 2000) %>% 
    mutate(ACHIEVEMENT = "2K Club") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Sharing is Caring
  ach_sharing <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(A = sum(A), .groups = "drop") %>% 
    filter(A >= 700) %>% 
    mutate(ACHIEVEMENT = "Sharing is Caring") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # GETDAFUCKOUTTAHEREIGOTIT
  ach_getdaf <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(R = sum(R), .groups = "drop") %>% 
    filter(R >= 1000) %>% 
    mutate(ACHIEVEMENT = "GETDAFUCKOUTTAHEREIGOTIT") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Grand Theft Auto
  ach_gta <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(S = sum(S), .groups = "drop") %>% 
    filter(S >= 150) %>% 
    mutate(ACHIEVEMENT = "Grand Theft Auto") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Send It Back
  ach_senditback <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(B = sum(B), .groups = "drop") %>% 
    filter(B >= 1000) %>% 
    mutate(ACHIEVEMENT = "Send It Back") %>% 
    select(ACHIEVEMENT, SEASON)
  
  # Free Throw Merchant
  ach_freethrowmerchant <- player_df %>% 
    group_by(SEASON) %>% 
    summarize(P = sum(P), P_FT = sum(FTM),
              .groups = "drop") %>% 
    mutate(PCT = P_FT/P) %>% 
    filter(P >= 500, PCT >= 0.25) %>% 
    mutate(ACHIEVEMENT = "Free Throw Merchant") %>% 
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
        ach_senditback,
        ach_freethrowmerchant
      ),
      by = "ACHIEVEMENT"
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
    summarize(DATES = str_c(DATE, collapse = ", "))
}