clean_allstats <- function(df) {
  df %>% 
    bind_rows() %>% 
    mutate(DATE = mdy(DATE),
           FG = str_c(FGM, "-", FGA),
           `3P` = str_c(`3PM`, "-", `3PA`),
           FT = str_c(FTM, "-", FTA),
           GMSC = P + (0.4 * FGM) - (0.7 * FGA) - (0.4 * (FTA - FTM)) + (0.7 * OR) + 
             (0.3 * DR) + S + (0.7 * A) + (0.7 * B) - (0.4 * PF) - TO) %>% 
    mutate(GMSC = round(GMSC, 2)) %>% 
    arrange(PLAYER, DATE)
}



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
         "CLE" = "#FDBB30",
         "NYK" = "#F58426",
         "DET" = "#1d42ba",
         "HOU" = "#ff0000",
         "SAC" = "#9300ff",
         "GSW" = "#0000ff",
         "LAL" = "#663399",
         "DAL" = "#B8C4CA",
         "LAC" = "#000000",
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
