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

  with_logo <- function(x) {
    ifelse(is.na(x), NA_character_,
      str_c("<img src='logo-", tolower(str_extract(x, "^[A-Z]{3}")), ".png' height='24'> ", x)
    )
  }

  # Extract 3-letter team code; returns NA for ties/missing
  tc <- function(x) {
    if (is.na(x)) return(NA_character_)
    code <- str_extract(x, "^[A-Z]{3}")
    if (is.na(code) || code == "TIE") NA_character_ else code
  }

  # Convert hex team color to rgba tint
  team_rgba <- function(team) {
    if (is.na(team)) return("")
    hex <- get_team_color(team)
    if (is.null(hex)) return("")
    r <- strtoi(substr(hex, 2, 3), 16L)
    g <- strtoi(substr(hex, 4, 5), 16L)
    b <- strtoi(substr(hex, 6, 7), 16L)
    sprintf("rgba(%d,%d,%d,0.35)", r, g, b)
  }

  # Paint a range of rows in one column
  paint <- function(mat, rows, col, team) {
    clr <- team_rgba(team)
    if (nchar(clr) > 0) mat[rows, col] <- clr
    mat
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

  # ---- Color matrix ----
  # Columns: 1=WEST_R1, 2=WEST_R2, 3=WCF, 4=WCF_CHAMP, 5=FINALS,
  #          6=ECF_CHAMP, 7=ECF, 8=EAST_R2, 9=EAST_R1
  # Rows 1-15 match the tribble below.
  #
  # For each team, the colored path is:
  #   seed cell → bridge (same row, adj col) → R1 result
  #   R1 result → bridge right + vertical → R2 result
  #   R2 result → bridge right + vertical span → conf finals
  #   conf finals → finals (already adjacent, no bridge needed)

  cm <- matrix("", nrow = 15, ncol = 9)

  # Seeds and their same-row bridges toward the R1 result column
  seed_rows <- list(
    list(row=1,  w=tc(w[[1]]), e=tc(e[[1]])),
    list(row=3,  w=tc(w[[8]]), e=tc(e[[8]])),
    list(row=5,  w=tc(w[[4]]), e=tc(e[[4]])),
    list(row=7,  w=tc(w[[5]]), e=tc(e[[5]])),
    list(row=9,  w=tc(w[[3]]), e=tc(e[[3]])),
    list(row=11, w=tc(w[[6]]), e=tc(e[[6]])),
    list(row=13, w=tc(w[[2]]), e=tc(e[[2]])),
    list(row=15, w=tc(w[[7]]), e=tc(e[[7]]))
  )
  for (s in seed_rows) {
    cm <- paint(cm, s$row, 1, s$w)  # West seed cell
    cm <- paint(cm, s$row, 2, s$w)  # West bridge → R1 result col
    cm <- paint(cm, s$row, 9, s$e)  # East seed cell
    cm <- paint(cm, s$row, 8, s$e)  # East bridge → R1 result col
  }

  # R1 result cells
  cm <- paint(cm, 2,  2, tc(wr1$top));  cm <- paint(cm, 6,  2, tc(wr1$mid1))
  cm <- paint(cm, 10, 2, tc(wr1$mid2)); cm <- paint(cm, 14, 2, tc(wr1$bot))
  cm <- paint(cm, 2,  8, tc(er1$top));  cm <- paint(cm, 6,  8, tc(er1$mid1))
  cm <- paint(cm, 10, 8, tc(er1$mid2)); cm <- paint(cm, 14, 8, tc(er1$bot))

  # R1→R2 bridges (right from result, then vertical to R2 result row) + R2 result cells
  # West: top half (1v8 → wr2_top at row 4, 4v5 → wr2_top at row 4)
  cm <- paint(cm, 2:3,   3, tc(wr1$top));  cm <- paint(cm, 4, 3, tc(wr2_top))
  cm <- paint(cm, 5:6,   3, tc(wr1$mid1))
  # West: bottom half (3v6 → wr2_bot at row 12, 2v7 → wr2_bot at row 12)
  cm <- paint(cm, 10:11, 3, tc(wr1$mid2)); cm <- paint(cm, 12, 3, tc(wr2_bot))
  cm <- paint(cm, 13:14, 3, tc(wr1$bot))

  # East: top half
  cm <- paint(cm, 2:3,   7, tc(er1$top));  cm <- paint(cm, 4, 7, tc(er2_top))
  cm <- paint(cm, 5:6,   7, tc(er1$mid1))
  # East: bottom half
  cm <- paint(cm, 10:11, 7, tc(er1$mid2)); cm <- paint(cm, 12, 7, tc(er2_bot))
  cm <- paint(cm, 13:14, 7, tc(er1$bot))

  # R2→conf finals bridges (right from R2, then vertical span to row 8) + conf finals cells
  cm <- paint(cm, 4:7,  4, tc(wr2_top)); cm <- paint(cm, 8, 4, tc(wcf))
  cm <- paint(cm, 9:12, 4, tc(wr2_bot))
  cm <- paint(cm, 4:7,  6, tc(er2_top)); cm <- paint(cm, 8, 6, tc(ecf))
  cm <- paint(cm, 9:12, 6, tc(er2_bot))

  # Finals
  cm <- paint(cm, 8, 5, tc(finals))

  color_rows <- lapply(seq_len(nrow(cm)), function(i) unname(cm[i, ]))
  colors_json <- jsonlite::toJSON(color_rows)

  # ---- Display bracket ----
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
    mutate(across(everything(), ~if_else(is.na(.), NA_character_, with_logo(.)))) %>%
    mutate(across(everything(), ~replace(., is.na(.), ""))) %>%
    datatable(
      escape = FALSE,
      rownames = FALSE,
      selection = list(mode = "single", target = "cell"),
      options = list(
        pageLength = 100,
        scrollX = TRUE,
        dom = "t",
        columnDefs = list(
          list(className = "dt-center", targets = "_all"),
          list(
            targets = "_all",
            createdCell = JS(sprintf(
              "function(td, cellData, rowData, rowIndex, colIndex) {
                var colors = %s;
                if (colors[rowIndex] && colors[rowIndex][colIndex] !== '') {
                  td.style.backgroundColor = colors[rowIndex][colIndex];
                }
              }",
              colors_json
            ))
          )
        )
      )
    )
})

output$playoff_series <- renderDT({
  playoff_games() %>%
    mutate(SERIES = str_c(TEAM, " ", SERIES1, "-", SERIES2, " ", OPP_RAW)) %>%
    select(ROUND, GAME, SCORE, WINNER, SERIES) %>%
    datatable(rownames = FALSE, options = list(pageLength = 100, scrollX = TRUE))
})
