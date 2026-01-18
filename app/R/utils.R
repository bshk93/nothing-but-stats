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
      TO = sum(TO),
      MPG = sum(M)/G,
      PPG = sum(P)/G,
      RPG = sum(R)/G,
      APG = sum(A)/G,
      SPG = sum(S)/G,
      BPG = sum(B)/G,
      TOPG = sum(TO)/G,
      `3PM` = sum(`3PM`),
      FGPCT = sum(FGM)/sum(FGA),
      `3PPCT` = sum(`3PM`)/sum(`3PA`),
      FTPCT = sum(FTM)/sum(FTA),
      GMSC = mean(GMSC),
      PCT = sum(WL == "W")/n(),
      .groups = 'drop'
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
      TO_RANK = rank(desc(TO), ties.method = "min"),
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
      TOPG_RANK = rank(desc(TOPG), ties.method = "min"),
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

get_division <- function(team) {
  case_when(
    team %in% c("NYK", "TOR", "BOS", "PHI", "BKN") ~ "Atlantic",
    team %in% c("DET", "CLE", "MIL", "CHI", "IND") ~ "Central",
    team %in% c("ORL", "ATL", "MIA", "CHA", "WAS") ~ "Southeast",
    team %in% c("OKC", "DEN", "MIN", "UTA", "POR") ~ "Northwest",
    team %in% c("LAL", "PHX", "GSW", "SAC", "LAC") ~ "Pacific",
    team %in% c("SAS", "HOU", "MEM", "DAL", "NOP") ~ "Southwest"
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

get_last_played_for_2 <- function(dfs) {
  # Try applying to entire dfs table
  dfs %>% 
    group_by(PLAYER) %>% 
    arrange(PLAYER, DATE) %>% 
    summarize(TEAM = last(TEAM))
}

get_logo <- function(
  TEAM,
  height = NULL,
  align = NULL
) {
  attr_str <- ""
  if (!is.null(height)) {
    attr_str <- str_c(attr_str, " height='", height, "'")
  }
  if (!is.null(align)) {
    attr_str <- str_c(attr_str, " align='", align, "'")
  }
  
  str_c("<img src='logo-", 
        tolower(TEAM), 
        ".png'",
        attr_str,
        "></img>")
}

# Pre-computed standings and team stats functions ----
compute_standings <- function(season_df) {
  # season_df should be filtered to a single season
  x <- season_df %>%
    group_by(DATE, TEAM, OPP) %>%
    summarize(TEAM_PTS = sum(P), .groups = 'drop') %>%
    ungroup() %>%
    mutate(OPP = str_replace(OPP, "@", ""))
  
  games <- x %>%
    left_join(
      x %>%
        select(DATE, OPP = TEAM, OPP_PTS = TEAM_PTS),
      by = c("DATE", "OPP")
    ) %>%
    mutate(
      WIN  = TEAM_PTS > OPP_PTS,
      LOSS = TEAM_PTS < OPP_PTS,
      CONF = get_conference(TEAM),
      DIV  = get_division(TEAM),
      OPP_CONF = get_conference(OPP),
      OPP_DIV  = get_division(OPP)
    )
  
  standings <- games %>%
    group_by(TEAM) %>%
    summarize(
      W = sum(WIN),
      L = sum(LOSS),
      CONF = first(CONF),
      DIV  = first(DIV),
      CONF_W = sum(WIN & CONF == OPP_CONF),
      CONF_L = sum(LOSS & CONF == OPP_CONF),
      DIV_W  = sum(WIN & DIV == OPP_DIV),
      DIV_L  = sum(LOSS & DIV == OPP_DIV),
      PPG = mean(TEAM_PTS),
      OPPG = mean(OPP_PTS),
      .groups = "drop"
    ) %>%
    mutate(
      PCT = W / (W + L),
      CONF_PCT = CONF_W / (CONF_W + CONF_L),
      DIV_PCT  = DIV_W / (DIV_W + DIV_L),
      DIFF = PPG - OPPG
    )
  
  h2h <- games %>%
    group_by(TEAM, OPP) %>%
    summarize(
      W = sum(WIN),
      L = sum(LOSS),
      PCT = W / (W + L),
      .groups = "drop"
    )
  
  division_winners <- standings %>%
    group_by(DIV) %>%
    arrange(desc(PCT), desc(CONF_PCT), desc(DIFF)) %>%
    slice(1) %>%
    mutate(DIV_WINNER = TRUE) %>%
    select(TEAM, DIV_WINNER)
  
  standings <- standings %>%
    left_join(division_winners, by = "TEAM") %>%
    mutate(DIV_WINNER = if_else(is.na(DIV_WINNER), FALSE, DIV_WINNER))
  
  resolve_nba_ties <- function(df, h2h) {
    if (nrow(df) == 1) return(df)
    
    teams <- df$TEAM
    
    h2h_tied <- h2h %>%
      filter(TEAM %in% teams, OPP %in% teams) %>%
      group_by(TEAM) %>%
      summarize(H2H_PCT = mean(PCT, na.rm = TRUE), .groups = "drop")
    
    df2 <- df %>%
      left_join(h2h_tied, by = "TEAM") %>%
      mutate(H2H_PCT = replace_na(H2H_PCT, 0))
    
    ordering <- df2 %>%
      arrange(
        desc(H2H_PCT),
        desc(DIV_WINNER),
        desc(DIV_PCT),
        desc(CONF_PCT),
        desc(DIFF)
      )
    
    # recursive elimination
    top <- ordering[1, ]
    
    tied <- ordering %>%
      filter(
        H2H_PCT == top$H2H_PCT,
        DIV_WINNER == top$DIV_WINNER,
        DIV_PCT == top$DIV_PCT,
        CONF_PCT == top$CONF_PCT,
        DIFF == top$DIFF
      )
    
    result <-
      if (nrow(tied) == nrow(ordering)) {
        ordering
      } else {
        bind_rows(
          top,
          resolve_nba_ties(
            ordering[-1, ] %>% select(-H2H_PCT),
            h2h
          )
        )
      }
    
    # 🔑 CRITICAL: drop H2H_PCT before returning
    result %>% select(-H2H_PCT)
  }
  
  final_standings <- standings %>%
    group_by(CONF, W, L) %>%
    group_modify(~ resolve_nba_ties(.x, h2h)) %>%
    ungroup() %>%
    group_by(CONF) %>%
    arrange(CONF, desc(W), .by_group = TRUE) %>%
    mutate(
      GB = (max(W - L) - (W - L))/2,
      SEED = row_number()
    ) %>%
    ungroup()
  
  final_standings %>%
    transmute(
      SEED = paste0(CONF, "-", SEED),
      TEAM, GB, W, L,
      PCT = round(PCT, 3),
      PPG = round(PPG, 1),
      OPPG = round(OPPG, 1),
      DIFF = round(DIFF, 1)
    )
}

compute_team_stats <- function(season_df) {
  # season_df should be filtered to a single season
  season_df %>%
    group_by(TEAM, DATE) %>%
    summarize(
      P = sum(P), R = sum(R), A = sum(A), S = sum(S), B = sum(B),
      TO = sum(TO), PF = sum(PF), `3PM` = sum(`3PM`), `3PA` = sum(`3PA`),
      .groups = 'drop'
    ) %>%
    group_by(TEAM) %>%
    summarize(
      PPG = mean(P), RPG = mean(R), APG = mean(A), SPG = mean(S), BPG = mean(B),
      TOPG = mean(TO), PFPG = mean(PF), `3PMPG` = mean(`3PM`), `3PAPG` = mean(`3PA`)
    ) %>%
    mutate_if(is.numeric, round, 2) %>%
    mutate(`3PPCT` = round(`3PMPG`/`3PAPG`, 3))
}

# Helper function for league leaders
leader_helper <- function(category, summary_df, dfs, min_games = 1) {
  tmpvarname1 <- str_c(category, 'PG')
  tmpvarname2 <- str_c('PLAYER_', category)
  
  x <- summary_df %>%
    filter(G >= min_games) %>%
    arrange_at(tmpvarname1) %>%
    arrange(desc(row_number())) %>%
    head(10)
  
  x <- x %>%
    left_join(get_last_played_for_2(dfs), by = 'PLAYER')
  
  x %>%
    mutate(PLAYER = str_c(PLAYER, ' ', get_logo(TEAM, height = 20))) %>%
    select({{ tmpvarname2 }} := PLAYER, tmpvarname1) %>%
    mutate(rn = row_number()) %>%
    mutate({{ tmpvarname1 }} := round(get(tmpvarname1), 1))
}
