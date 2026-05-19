# Owner Stats Module ----
# All outputs for the Owner Stats tab

output$owner_stats <- renderDT({

  tryCatch({
    owner_stats <- read_csv(
      file.path(DATA_DIR, "owner_stats.csv"),
      show_col_types = FALSE
    ) %>%
      mutate(
        Teams = map_chr(str_split(teams, ", "), ~ str_c(
          str_c('<img src="logo-', tolower(.x), '.png" height=20></img>'),
          collapse = ""
        )),
        `Win Pct`            = str_c(round(total_pct * 100, 1), "%"),
        `Win Pct (Regular)`  = str_c(round(reg_pct * 100, 1), "%"),
        `Win Pct (Playoffs)` = if_else(
          !is.na(playoff_pct),
          str_c(round(playoff_pct * 100, 1), "%"),
          "—"
        )
      ) %>%
      select(
        Owner                   = owner,
        Teams,
        Wins                    = total_w,
        `Regular Season Wins`   = reg_w,
        `Playoff Wins`          = playoff_w,
        Losses                  = total_l,
        `Regular Season Losses` = reg_l,
        `Playoff Losses`        = playoff_l,
        `Win Pct`,
        `Win Pct (Regular)`,
        `Win Pct (Playoffs)`
      )

    format_as_datatable(owner_stats, escape = FALSE)
  }, error = function(e) {
    format_as_datatable(tibble(Error = str_c("Error loading data: ", as.character(e))))
  })
})
