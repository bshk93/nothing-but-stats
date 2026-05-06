# Playoff Archive Module ----
# All outputs for the Playoff Archive tab

playoff_games <- reactive({
  season_str <- str_c(input$seasonplayoffs, " Playoffs")

  x <- dfs_playoffs %>%
    filter(SEASON == season_str) %>%
    group_by(SEASON, ROUND, TEAM, GAME, OPP, WL) %>%
    summarize(P = sum(P), .groups = "drop") %>%
    mutate(OPP_RAW = str_replace(OPP, "@", ""))

  x %>%
    left_join(x, by = c("SEASON", "ROUND", "GAME", "OPP_RAW" = "TEAM")) %>%
    mutate(SCORE = str_c(TEAM, " ", P.x, "-", P.y, " ", OPP.x)) %>%
    group_by(ROUND, TEAM, OPP_RAW) %>%
    mutate(tmp_x = first(OPP.x)) %>%
    mutate(HOME_TEAM = if_else(str_detect(tmp_x, "@"), OPP_RAW, TEAM)) %>%
    filter(TEAM == HOME_TEAM) %>%
    mutate(WINNER = case_when(WL.x == "W" ~ TEAM, TRUE ~ OPP_RAW)) %>%
    group_by(TEAM, ROUND) %>%
    mutate(
      SERIES1 = cumsum(WL.x == "W"),
      SERIES2 = cumsum(WL.y == "W")
    ) %>%
    ungroup()
})

output$playoff_bracket <- renderDT({
  y <- playoff_games()

  seeds <- get_playoff_seeds() %>%
    filter(SEASON == str_c(input$seasonplayoffs, " Playoffs"))

  seed <- function(conf, s) {
    seeds %>%
      filter(CONF == conf, SEED == s) %>%
      mutate(TEAM = str_c(TEAM, " (", s, ")")) %>%
      pull(TEAM)
  }

  series <- function(t1, t2) {
    if (is.na(t1) || is.na(t2)) return(NA_character_)
    t1c <- str_extract(t1, "^[A-Z]{3}")
    t2c <- str_extract(t2, "^[A-Z]{3}")
    z <- y %>%
      filter((TEAM == t1c & OPP_RAW == t2c) | (TEAM == t2c & OPP_RAW == t1c)) %>%
      mutate(SERIES_WINNER = case_when(
        SERIES1 > SERIES2 ~ TEAM,
        SERIES2 > SERIES1 ~ OPP_RAW,
        TRUE ~ "TIED"
      )) %>%
      mutate(RESULT = str_c(SERIES_WINNER, " (", SERIES1, "-", SERIES2, ")")) %>%
      tail(1) %>%
      pull(RESULT)
    if (length(z) == 0) NA_character_ else z
  }

  w <- lapply(1:8, function(s) seed("WEST", s))
  e <- lapply(1:8, function(s) seed("EAST", s))

  # Round 1
  wr1 <- list(
    top  = series(w[[1]], w[[8]]),
    mid1 = series(w[[4]], w[[5]]),
    mid2 = series(w[[3]], w[[6]]),
    bot  = series(w[[2]], w[[7]])
  )
  er1 <- list(
    top  = series(e[[1]], e[[8]]),
    mid1 = series(e[[4]], e[[5]]),
    mid2 = series(e[[3]], e[[6]]),
    bot  = series(e[[2]], e[[7]])
  )

  # Round 2
  wr2_top <- series(wr1$top,  wr1$mid1)
  wr2_bot <- series(wr1$mid2, wr1$bot)
  er2_top <- series(er1$top,  er1$mid1)
  er2_bot <- series(er1$mid2, er1$bot)

  # Conference Finals and NBA Finals
  wcf    <- series(wr2_top, wr2_bot)
  ecf    <- series(er2_top, er2_bot)
  finals <- series(wcf, ecf)

  tribble(
    ~WEST_R1,  ~WEST_R2,   ~WCF,     ~WCF_CHAMP, ~FINALS, ~ECF_CHAMP, ~ECF,     ~EAST_R2,   ~EAST_R1,
    w[[1]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[1]],
    NA,        wr1$top,    NA,        NA,          NA,      NA,         NA,        er1$top,    NA,
    w[[8]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[8]],
    NA,        NA,         wr2_top,   NA,          NA,      NA,         er2_top,   NA,         NA,
    w[[4]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[4]],
    NA,        wr1$mid1,   NA,        NA,          NA,      NA,         NA,        er1$mid1,   NA,
    w[[5]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[5]],
    NA,        NA,         NA,        wcf,         finals,  ecf,        NA,        NA,         NA,
    w[[3]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[3]],
    NA,        wr1$mid2,   NA,        NA,          NA,      NA,         NA,        er1$mid2,   NA,
    w[[6]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[6]],
    NA,        NA,         wr2_bot,   NA,          NA,      NA,         er2_bot,   NA,         NA,
    w[[2]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[2]],
    NA,        wr1$bot,    NA,        NA,          NA,      NA,         NA,        er1$bot,    NA,
    w[[7]],    NA,         NA,        NA,          NA,      NA,         NA,        NA,         e[[7]]
  ) %>%
    mutate(across(everything(), ~replace(., is.na(.), ""))) %>%
    datatable(
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        dom = "t",
        columnDefs = list(list(className = "dt-center", targets = "_all"))
      )
    )
})

output$playoff_series <- renderDT({
  playoff_games() %>%
    mutate(SERIES = str_c(TEAM, " ", SERIES1, "-", SERIES2, " ", OPP_RAW)) %>%
    select(ROUND, GAME, SCORE, WINNER, SERIES) %>%
    datatable(rownames = FALSE, options = list(pageLength = 100, scrollX = TRUE))
})
