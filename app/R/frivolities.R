
win_streaks <- function(dfs) {
  
  dfs %>% 
    distinct(TEAM, SEASON, DATE, WL) %>% 
    group_by(TEAM, SEASON) %>% 
    arrange(TEAM, SEASON, DATE) %>% 
    mutate(flag = WL != coalesce(lag(WL), "X")) %>% 
    mutate(streak_group = cumsum(flag)) %>% 
    group_by(TEAM, SEASON, WL, streak_group) %>% 
    summarize(
      min_dt = min(DATE),
      max_dt = max(DATE),
      streak = n()
    )
  
}

big_three <- function(dfs) {
  
  x <- dfs %>% 
    group_by(PLAYER, DATE, TEAM) %>% 
    summarize(P = sum(P))
  
  x %>% 
    full_join(x, by = c('TEAM', 'DATE')) %>% 
    filter(PLAYER.x > PLAYER.y) %>% 
    full_join(x, by = c('TEAM', 'DATE')) %>% 
    filter(PLAYER > PLAYER.x, PLAYER > PLAYER.y) %>% 
    mutate(COMBINED_P = P + P.x + P.y) %>% 
    group_by(PLAYER.x, PLAYER.y, PLAYER) %>% 
    summarize(PPG = sum(COMBINED_P)/n(),
              G = n()) %>% 
    arrange(desc(PPG))
  
}


most_improved <- function(dfs) {
  
  dfs %>% 
    group_by(PLAYER, SEASON) %>% 
    summarize(
      G = n(),
      MPG = sum(M)/G,
      GMSC = sum(GMSC)/G
    ) %>% 
    arrange(PLAYER, SEASON) %>% 
    mutate(G_CHG = G - lag(G),
           MPG_CHG = MPG - lag(MPG),
           GMSC_CHG = GMSC - lag(GMSC)) %>% 
    mutate_if(is.numeric, round, 2)
  
}


roster_stability <- function(dfs) {
  
  x <- dfs %>% 
    group_by(PLAYER, TEAM, SEASON) %>% 
    summarize(M = sum(M),
              .groups = "drop") %>% 
    mutate(PREV_SEASON = str_c(
      as.numeric(str_extract(SEASON, '^\\d{2}')) - 1,
      '-',
      as.numeric(str_extract(SEASON, '\\d{2}$')) - 1
    ))
  
  x %>% 
    left_join(
      x %>% select(PLAYER, TEAM, PREV_SEASON = SEASON, PREV_M = M),
      by = c('PLAYER', 'TEAM', 'PREV_SEASON')
    ) %>% 
    mutate(M_STABLE = (!is.na(PREV_M)) * M) %>% 
    group_by(TEAM, SEASON) %>% 
    summarize(M_STABLE = sum(M_STABLE),
              M_TOTAL = sum(M),
              .groups = "drop") %>% 
    mutate(STABILITY = M_STABLE / M_TOTAL)
}

plot_roster_stability <- function(stability_df) {
  stability_df <- stability_df %>% 
    filter(SEASON != "20-21")
  # Prepare the data and calculate averages
  heatmap_data <- stability_df %>%
    select(TEAM, SEASON, STABILITY) %>%
    # group_by(TEAM) %>%                               # Calculate average stability for each team
    # mutate(TEAM_AVG = mean(STABILITY, na.rm = TRUE)) %>%
    # ungroup() %>%
    # group_by(SEASON) %>%                             # Calculate average stability for each season
    # mutate(SEASON_AVG = mean(STABILITY, na.rm = TRUE)) %>%
    # ungroup() %>%
    bind_rows(
      tibble(TEAM = "League Avg",                    # Add row for league-wide averages by season
             SEASON = unique(stability_df$SEASON),
             STABILITY = stability_df %>% 
               group_by(SEASON) %>%
               summarize(STABILITY = mean(STABILITY, na.rm = TRUE)) %>%
               pull(STABILITY))
    ) %>%
    bind_rows(
      tibble(SEASON = "Team Avg",                   # Add column for team-wide averages across all seasons
             TEAM = unique(stability_df$TEAM),
             STABILITY = stability_df %>%
               group_by(TEAM) %>%
               summarize(STABILITY = mean(STABILITY, na.rm = TRUE)) %>%
               pull(STABILITY))
    )
  
  # Reshape to wide format
  heatmap_data <- heatmap_data %>%
    pivot_wider(names_from = TEAM, values_from = STABILITY) %>%
    arrange(SEASON)
  
  # Convert to matrix for heatmap
  heatmap_matrix <- as.matrix(heatmap_data[,-1])
  rownames(heatmap_matrix) <- heatmap_data$SEASON
  
  # Custom hover text (formatted as percentages)
  hover_text <- heatmap_matrix %>%
    apply(2, function(x) ifelse(!is.na(x), sprintf("%.1f%%", x * 100), NA))
  
  num_teams <- ncol(heatmap_matrix) - 1 # Number of teams
  num_seasons <- nrow(heatmap_matrix) - 1 # Number of seasons
  
  border_shapes <- list(
    # Vertical line between teams and "League Avg"
    list(
      type = "line",
      x0 = num_teams - 0.5, x1 = num_teams - 0.5,   # Position after the last team
      y0 = -.5, y1 = num_seasons + .5,            # Full height of the heatmap
      line = list(color = "black", width = 2)
    ),
    # Horizontal line between seasons and "Team Avg"
    list(
      type = "line",
      x0 = -0.5, x1 = num_teams + 0.5,              # Full width of the heatmap
      y0 = num_seasons - .5, y1 = num_seasons - .5, # Position after the last season
      line = list(color = "black", width = 2)
    )
  )
  
  # Create heatmap
  plot_ly(
    x = colnames(heatmap_matrix),  # Teams as columns
    y = rownames(heatmap_matrix),  # Seasons as rows
    z = heatmap_matrix,            # Stability values
    text = hover_text,             # Custom hover text
    hoverinfo = "text",            # Show only hover text
    texttemplate = "%{text}",
    textfont = list(size = 9),
    type = "heatmap",
    colors = colorRamp(c("red", "yellow", "green")) # Stability color scale
  ) %>%
    layout(
      #title = "Roster Stability",
      xaxis = list(title = "", tickangle = 45, automargin = TRUE),
      yaxis = list(title = "", autorange = "reversed", automargin = TRUE),
      legend = list(orientation = "h", y = -0.2, x = 0.5, xanchor = "center"), # Horizontal legend
      margin = list(l = 50, r = 50, t = 50, b = 50),
      shapes = border_shapes
    ) %>%
    config(scrollZoom = TRUE) %>%
    layout(
      xaxis = list(scaleanchor = "y"), # Keep cells square
      yaxis = list(scaleanchor = "x")
    )
}



most_teams <- function(dfs_everything) {
  dfs_everything %>% 
    group_by(PLAYER, TEAM) %>% 
    summarize(G = n(), .groups = "drop") %>% 
    group_by(PLAYER, G) %>% 
    mutate(TEAM = str_c('<img src="logo-', tolower(TEAM), '.png" height=20></img>')) %>% 
    group_by(PLAYER) %>% 
    summarize(N_TEAMS = n(),
              GAMES_W_ONE_TEAM = max(G),
              THE_TEAM = TEAM[which.max(G)],
              ALL_TEAMS = str_c(TEAM, collapse = ''),
              .groups = "drop") %>% 
    arrange(desc(N_TEAMS))
}


calculate_hof_points <- function(dfs_everything, dfs_playoffs, dfs,
                                 team_filter = NULL, raw_data = FALSE) {
  
  temp_everything <- dfs_everything
  temp_playoffs <- dfs_playoffs
  temp_non_playoffs <- dfs
  temp_champs <- get_champions(dfs_playoffs)
  # temp_mvp <- get_mvp() %>% select(PLAYER, SEASON)
  # temp_dpoy <- get_dpoy() %>% select(PLAYER, SEASON)
  # temp_others <- get_roy() %>% 
  #   bind_rows(get_6moy()) %>% 
  #   bind_rows(get_mip()) %>% 
  #   select(PLAYER, SEASON)
  
  if (!is.null(team_filter)) {
    temp_everything <- temp_everything %>% 
      filter(TEAM == team_filter)
    temp_champs <- temp_champs %>% 
      filter(TEAM == team_filter)
    temp_playoffs <- temp_playoffs %>% 
      filter(TEAM == team_filter)
    temp_non_playoffs <- temp_non_playoffs %>% 
      filter(TEAM == team_filter)
  }
  
  x <- temp_everything %>% 
    mutate(
      G = 1,
      
      GMSC_WGT_WL = case_when(
        WL == 'W' ~ 1.25,
        TRUE ~ 0.75
      ),
      GMSC_WGT_GAMETYPE = case_when(
        ROUND == 1 ~ 2,
        ROUND == 2 ~ 4,
        ROUND == 3 ~ 8,
        ROUND == 4 ~ 16,
        TRUE ~ 1
      )
    ) %>% 
    
    # normalize playoff games to 5.5 games per round
    group_by(SEASON, TEAM, ROUND) %>% 
    mutate(GMSC_WGT_ROUNDLEN = case_when(
      GMSC_WGT_GAMETYPE == 1 ~ 1,
      TRUE ~ 5.5/n_distinct(DATE)
    )) %>% 
    
    ungroup() %>% 
    mutate(GMSC_WEIGHTED = GMSC * GMSC_WGT_WL * GMSC_WGT_GAMETYPE * GMSC_WGT_ROUNDLEN) %>% 
    
    group_by(PLAYER) %>%
    summarize_at(
      vars(c('G', 'M', 'P', 'R', 'A', 'S', 'B', 'GMSC_WEIGHTED')), 
      sum
    ) %>% 
    
    left_join(
      temp_champs %>% 
        group_by(PLAYER) %>% 
        summarize(RINGS = n_distinct(SEASON)) %>% 
        mutate(RINGS = strrep('<img src="ring.png" height=20></img>', RINGS))
    ) %>% 
    
    left_join(
      temp_playoffs %>% 
        group_by(PLAYER) %>% 
        summarize(PLAYOFF_APPS = n_distinct(SEASON))
    ) %>% 
    
    left_join(
      dfs %>% 
        distinct(PLAYER, SEASON, TEAM) %>% 
        mutate(ACTIVE = SEASON == max(SEASON)) %>% 
        group_by(PLAYER) %>% 
        summarize(ACTIVE = max(ACTIVE))
    ) %>% 
    
    left_join(
      dfs %>% 
        distinct(PLAYER, TEAM) %>% 
        mutate(TEAM = str_c('<img src="logo-', tolower(TEAM), '.png" height=20></img>')) %>% 
        group_by(PLAYER) %>% 
        summarize(TEAMS = str_c(TEAM, collapse = ''))
    ) %>% 
    
    left_join(
      get_allnbn1() %>% 
        group_by(PLAYER) %>% 
        summarize(ALL_NBN_1 = n()) %>% 
        mutate(ALL_NBN_1 = strrep('<img src="medal1.png" height=20></img>', ALL_NBN_1))
    ) %>% 
    
    left_join(
      get_allnbn2() %>% 
        group_by(PLAYER) %>% 
        summarize(ALL_NBN_2 = n()) %>% 
        mutate(ALL_NBN_2 = strrep('<img src="medal2.png" height=20></img>', ALL_NBN_2))
    ) %>% 
    
    left_join(
      get_allnbn3() %>% 
        group_by(PLAYER) %>% 
        summarize(ALL_NBN_3 = n()) %>% 
        mutate(ALL_NBN_3 = strrep('<img src="medal3.png" height=20></img>', ALL_NBN_3))
    ) %>% 
    
    mutate(ALL_NBN = str_c(coalesce(ALL_NBN_1, ''), 
                           coalesce(ALL_NBN_2, ''), 
                           coalesce(ALL_NBN_3, ''))) %>% 
    
    left_join(
      get_allstars() %>% 
        group_by(PLAYER) %>% 
        summarize(ALLSTARS = n(), .groups = "drop")
    ) %>% 
    
    arrange(desc(GMSC_WEIGHTED)) %>% 
    
    mutate(HOF_POINTS = round(GMSC_WEIGHTED / 100, 1)) %>% 
    
    select(PLAYER,
           TEAMS,
           HOF_POINTS,
           RINGS, 
           PLAYOFF_APPS,
           ALLSTARS,
           starts_with('ALL_'),
           everything(),
           -starts_with('ALL_NBN_'),
           -GMSC_WEIGHTED)
  
  if (!is.null(team_filter)) {
    x <- x %>% 
      rename(
        RINGS_W_TEAM = RINGS,
        PLAYOFF_APPS_W_TEAM = PLAYOFF_APPS,
        HOF_POINTS_W_TEAM = HOF_POINTS
      )
  }
  
  if (raw_data) return(x)
  
  x %>% 
    
    datatable(
      escape = FALSE,
      options = list(pageLength = 50,
                     scrollX = TRUE,
                     columnDefs = list(
                       list(visible = FALSE,
                            targets = 'ACTIVE'),
                       list(className = 'dt-center', 
                            targets = '_all')
                     )),
      selection = list(mode = 'single', 
                       target = 'cell',
                       selectable = matrix(c(1:nrow(.), rep(1, nrow(.))), nrow(.), 2))
    ) %>% 
    formatStyle(
      "ACTIVE",
      target = "row",
      backgroundColor = styleEqual(c(1, 0), 
                                   c('#cae1ff', '#ffcacb'), 
                                   default = "normal")
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


playoff_risers <- function(dfs_everything) {
  dfs_everything %>% 
    group_by(PLAYER, SEASON) %>% 
    summarize(
      G = n(),
      GMSC = mean(GMSC),
      .groups = "drop"
    ) %>% 
    mutate(
      PLAYOFFS = str_detect(SEASON, "Playoffs"),
      SEASON = str_extract(SEASON, "\\d{2}-\\d{2}")
    ) %>% 
    pivot_wider(
      names_from = PLAYOFFS,
      values_from = c(G, GMSC),
      names_prefix = "PLAYOFFS_"
    ) %>% 
    filter(!is.na(G_PLAYOFFS_TRUE)) %>% 
    mutate(
      PLAYOFF_GMSC_DIFF = GMSC_PLAYOFFS_TRUE - GMSC_PLAYOFFS_FALSE
    ) %>% 
    group_by(PLAYER) %>% 
    summarize(
      AVERAGE_PLAYOFF_GMSC_DIFF = mean(PLAYOFF_GMSC_DIFF, na.rm = TRUE),
      N_PLAYOFF_SEASONS = n(),
      G_REGULAR = sum(G_PLAYOFFS_FALSE, na.rm = TRUE),
      G_PLAYOFFS = sum(G_PLAYOFFS_TRUE, na.rm = TRUE)
    ) %>% 
    arrange(desc(AVERAGE_PLAYOFF_GMSC_DIFF)) %>% 
    mutate(AVERAGE_PLAYOFF_GMSC_DIFF = round(AVERAGE_PLAYOFF_GMSC_DIFF, 2))
}