get_champion_list <- function() {
  tribble(
    ~SEASON, ~TEAM,
    '20-21 Playoffs', 'ATL',
    '21-22 Playoffs', 'ATL',
    '22-23 Playoffs', 'PHX',
    '23-24 Playoffs', 'CLE',
    '24-25 Playoffs', 'PHX'
  )
}

get_champions <- function(dfs_playoffs) {
  dfs_playoffs %>%
    inner_join(get_champion_list(), by = c("SEASON", "TEAM"))
}

get_runners_up <- function() {
  tribble(
    ~SEASON, ~RUNNER_UP, ~EAST_RUNNER_UP, ~WEST_RUNNER_UP,
    '20-21 Playoffs', 'DAL', 'MIL', 'DEN',
    '21-22 Playoffs', 'NOP', 'WAS', 'GSW',
    '22-23 Playoffs', 'CLE', 'BKN', 'DEN',
    '23-24 Playoffs', 'PHX', 'NYK', 'UTA',
    '24-25 Playoffs', 'MIL', 'ATL', 'OKC'
  )
}

get_allstars <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    
    # 2021
    'DURANT, KEVIN', '20-21',
    'JAMES, LEBRON', '20-21',
    'ANTETOKOUNMPO, GIANNIS', '20-21',
    'YOUNG, TRAE', '20-21',
    'BEAL, BRADLEY', '20-21',
    'LEONARD, KAWHI', '20-21',
    'JOKIC, NIKOLA', '20-21',
    'TOWNS, KARL-ANTHONY', '20-21',
    'CURRY, STEPHEN', '20-21',
    'HARDEN, JAMES', '20-21',
    'WILLIAMSON, ZION', '20-21',
    'WESTBROOK, RUSSELL', '20-21',
    'SIMMONS, BEN', '20-21',
    'TATUM, JAYSON', '20-21',
    'DAVIS, ANTHONY', '20-21',
    'ADEBAYO, BAM', '20-21',
    'BROWN, JAYLEN', '20-21',
    'EMBIID, JOEL', '20-21',
    'LILLARD, DAMIAN', '20-21',
    'DONCIC, LUKA', '20-21',
    'INGRAM, BRANDON', '20-21',
    'VUCEVIC, NIKOLA', '20-21',
    'DEROZAN, DEMAR', '20-21',
    'LAVINE, ZACH', '20-21',
    'IRVING, KYRIE', '20-21',
    
    # 2022
    'JOKIC, NIKOLA', '21-22',
    'WILLIAMSON, ZION', '21-22',
    'LEONARD, KAWHI', '21-22',
    'DONCIC, LUKA', '21-22',
    'MORANT, JA', '21-22',
    'ANTETOKOUNMPO, GIANNIS', '21-22',
    'JAMES, LEBRON', '21-22',
    'DURANT, KEVIN', '21-22',
    'YOUNG, TRAE', '21-22',
    'IRVING, KYRIE', '21-22',
    'BUTLER, JIMMY', '21-22',
    'SABONIS, DOMANTAS', '21-22',
    'INGRAM, BRANDON', '21-22',
    'TOWNS, KARL-ANTHONY', '21-22',
    'DEROZAN, DEMAR', '21-22',
    'HARDEN, JAMES', '21-22',
    'LILLARD, DAMIAN', '21-22',
    'CURRY, STEPHEN', '21-22',
    'GILGEOUS-ALEXANDER, SHAI', '21-22',
    'TATUM, JAYSON', '21-22',
    'EMBIID, JOEL', '21-22',
    'DAVIS, ANTHONY', '21-22',
    'GEORGE, PAUL', '21-22',
    'ADEBAYO, BAM', '21-22',
    'BEAL, BRADLEY', '21-22',
    'PAUL, CHRIS', '21-22',
    
    # 2023
    'DONCIC, LUKA', '22-23',
    'MORANT, JA', '22-23',
    'JOKIC, NIKOLA', '22-23',
    'INGRAM, BRANDON', '22-23',
    'LEONARD, KAWHI', '22-23',
    'CURRY, STEPHEN', '22-23',
    'HARDEN, JAMES', '22-23',
    'DEROZAN, DEMAR', '22-23',
    'GEORGE, PAUL', '22-23',
    'SABONIS, DOMANTAS', '22-23',
    'MITCHELL, DONOVAN', '22-23',
    'SIAKAM, PASCAL', '22-23',
    'IRVING, KYRIE', '22-23',
    'BEAL, BRADLEY', '22-23',
    'JAMES, LEBRON', '22-23',
    'DURANT, KEVIN', '22-23',
    'ANTETOKOUNMPO, GIANNIS', '22-23',
    'YOUNG, TRAE', '22-23',
    'LILLARD, DAMIAN', '22-23',
    'TATUM, JAYSON', '22-23',
    'EMBIID, JOEL', '22-23',
    'DAVIS, ANTHONY', '22-23',
    'GARLAND, DARIUS', '22-23',
    'WILLIAMSON, ZION', '22-23',
    
    # 2024
    'DONCIC, LUKA', '23-24',
    'CURRY, STEPHEN', '23-24',
    'GILGEOUS-ALEXANDER, SHAI', '23-24',
    'JOKIC, NIKOLA', '23-24',
    'MITCHELL, DONOVAN', '23-24',
    'HARDEN, JAMES', '23-24',
    'MORANT, JA', '23-24',
    'BUTLER, JIMMY', '23-24',
    'BOOKER, DEVIN', '23-24',
    'BALL, LAMELO', '23-24',
    'SABONIS, DOMANTAS', '23-24',
    'FOX, DEAARON', '23-24',
    'JAMES, LEBRON', '23-24',
    'ANTETOKOUNMPO, GIANNIS', '23-24',
    'TATUM, JAYSON', '23-24',
    'DURANT, KEVIN', '23-24',
    'IRVING, KYRIE', '23-24',
    'EMBIID, JOEL', '23-24',
    'DAVIS, ANTHONY', '23-24',
    'YOUNG, TRAE', '23-24',
    'HALIBURTON, TYRESE', '23-24',
    'BRUNSON, JALEN', '23-24',
    'BEAL, BRADLEY', '23-24',
    'EDWARDS, ANTHONY', '23-24',
    
    # 2025
    'DONCIC, LUKA', '24-25',
    'JOKIC, NIKOLA', '24-25',
    'GILGEOUS-ALEXANDER, SHAI', '24-25',
    'BOOKER, DEVIN', '24-25',
    'TATUM, JAYSON', '24-25',
    'EMBIID, JOEL', '24-25',
    'SABONIS, DOMANTAS', '24-25',
    'MAXEY, TYRESE', '24-25',
    'MITCHELL, DONOVAN', '24-25',
    'MORANT, JA', '24-25',
    'FOX, DEAARON', '24-25',
    'BROWN, JAYLEN', '24-25',
    'BUTLER, JIMMY', '24-25',
    'WILLIAMS, JALEN', '24-25',
    'JAMES, LEBRON', '24-25',
    'DAVIS, ANTHONY', '24-25',
    'ANTETOKOUNMPO, GIANNIS', '24-25',
    'CURRY, STEPHEN', '24-25',
    'WEMBANYAMA, VICTOR', '24-25',
    'BRUNSON, JALEN', '24-25',
    'EDWARDS, ANTHONY', '24-25',
    'YOUNG, TRAE', '24-25',
    'TOWNS, KARL-ANTHONY', '24-25',
    'HALIBURTON, TYRESE', '24-25',
    'BALL, LAMELO', '24-25',
    'CUNNINGHAM, CADE', '24-25',
    'ADEBAYO, BAM', '24-25',
    
    # 2026
    'CUNNINGHAM, CADE', '25-26',
    'EDWARDS, ANTHONY', '25-26',
    'JAMES, LEBRON', '25-26',
    'WEMBANYAMA, VICTOR', '25-26',
    'HALIBURTON, TYRESE', '25-26',
    'ANTETOKOUNMPO, GIANNIS', '25-26',
    'CURRY, STEPHEN', '25-26',
    'DAVIS, ANTHONY', '25-26',
    'JACKSON, JAREN', '25-26',
    'WAGNER, FRANZ', '25-26',
    'BRUNSON, JALEN', '25-26',
    'RANDLE, JULIUS', '25-26',
    'DONCIC, LUKA', '25-26',
    'GILGEOUS-ALEXANDER, SHAI', '25-26',
    'JOKIC, NIKOLA', '25-26',
    'MITCHELL, DONOVAN', '25-26',
    'BARNES, SCOTTIE', '25-26',
    'EMBIID, JOEL', '25-26',
    'BOOKER, DEVIN', '25-26',
    'BROWN, JAYLEN', '25-26',
    'SENGUN, ALPEREN', '25-26',
    'MOBLEY, EVAN', '25-26',
    'BALL, LAMELO', '25-26',
    'HOLMGREN, CHET', '25-26'
  ) %>% 
    mutate(star = "<img src='star.png' height='20'></img>")
}


get_mvp <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'HARDEN, JAMES', '20-21',
    'JOKIC, NIKOLA', '21-22',
    'DONCIC, LUKA', '22-23',
    'MITCHELL, DONOVAN', '23-24',
    'DONCIC, LUKA', '24-25'
  ) %>% 
    mutate(crown = "<img src='crown.png' height='20'></img>")
}


get_dpoy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'ANTETOKOUNMPO, GIANNIS', '20-21',
    'GOBERT, RUDY', '21-22',
    'DAVIS, ANTHONY', '22-23',
    'WEMBANYAMA, VICTOR', '23-24',
    'WEMBANYAMA, VICTOR', '24-25'
  ) %>% 
    mutate(hand = "<img src='hand.png' height='20'></img>")
}


get_roy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'BALL, LAMELO', '20-21',
    'BARNES, SCOTTIE', '21-22',
    'BANCHERO, PAOLO', '22-23',
    'WEMBANYAMA, VICTOR', '23-24',
    'SARR, ALEX', '24-25'
  ) %>% 
    mutate(baby = "<img src='baby.png' height='20'></img>")
}

get_6moy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'DINWIDDIE, SPENCER', '20-21',
    'ANTHONY, COLE', '21-22',
    'FULTZ, MARKELLE', '22-23',
    'BANE, DESMOND', '23-24',
    'BRIDGES, MILES', '24-25'
  ) %>% 
    mutate(six = "<img src='six.png' height='20'></img>")
}

get_mip <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'WOOD, CHRISTIAN', '20-21',
    'GARLAND, DARIUS', '21-22',
    'BANE, DESMOND', '22-23',
    'OKONGWU, ONYEKA', '23-24',
    'JOHNSON, JALEN', '24-25'
  ) %>% 
    mutate(chart = "<img src='chart.png', height='20'></img>")
}

get_allnbn1 <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'ANTETOKOUNMPO, GIANNIS', '20-21',
    'DONCIC, LUKA', '20-21',
    'HARDEN, JAMES', '20-21',
    'JAMES, LEBRON', '20-21',
    'JOKIC, NIKOLA', '20-21',
    
    'ANTETOKOUNMPO, GIANNIS', '21-22',
    'DONCIC, LUKA', '21-22',
    'IRVING, KYRIE', '21-22',
    'JAMES, LEBRON', '21-22',
    'JOKIC, NIKOLA', '21-22',
    
    'DONCIC, LUKA', '22-23',
    'JOKIC, NIKOLA', '22-23',
    'MORANT, JA', '22-23',
    'JAMES, LEBRON', '22-23',
    'DURANT, KEVIN', '22-23',
    
    'MITCHELL, DONOVAN', '23-24',
    'BUTLER, JIMMY', '23-24',
    'JAMES, LEBRON', '23-24',
    'TATUM, JAYSON', '23-24',
    'JOKIC, NIKOLA', '23-24',
    
    'ANTETOKOUNMPO, GIANNIS', '24-25',
    'DONCIC, LUKA', '24-25',
    'GILGEOUS-ALEXANDER, SHAI', '24-25',
    'JOKIC, NIKOLA', '24-25',
    'EMBIID, JOEL', '24-25'
  ) %>% 
    mutate(medal1 = "<img src='medal1.png' height='20'></img>")
}

get_allnbn2 <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'CURRY, STEPHEN', '20-21',
    'DAVIS, ANTHONY', '20-21',
    'DURANT, KEVIN', '20-21',
    'TOWNS, KARL-ANTHONY', '20-21',
    'WESTBROOK, RUSSELL', '20-21',
    
    'BEAL, BRADLEY', '21-22',
    'BUTLER, JIMMY', '21-22',
    'EMBIID, JOEL', '21-22',
    'MORANT, JA', '21-22',
    'WILLIAMSON, ZION', '21-22',
    
    'TATUM, JAYSON', '22-23',
    'HARDEN, JAMES', '22-23',
    'ANTETOKOUNMPO, GIANNIS', '22-23',
    'IRVING, KYRIE', '22-23',
    'DAVIS, ANTHONY', '22-23',
    
    'DURANT, KEVIN', '23-24',
    'GILGEOUS-ALEXANDER, SHAI', '23-24',
    'ANTETOKOUNMPO, GIANNIS', '23-24',
    'HARDEN, JAMES', '23-24',
    'CURRY, STEPHEN', '23-24',
    
    'JAMES, LEBRON', '24-25',
    'BRUNSON, JALEN', '24-25',
    'CURRY, STEPHEN', '24-25',
    'TATUM, JAYSON', '24-25',
    'DAVIS, ANTHONY', '24-25'
  ) %>% 
    mutate(medal2 = "<img src='medal2.png' height='20'></img>")
}

get_allnbn3 <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'DEROZAN, DEMAR', '20-21',
    'EMBIID, JOEL', '20-21',
    'LILLARD, DAMIAN', '20-21',
    'SIMMONS, BEN', '20-21',
    'TATUM, JAYSON', '20-21',
    
    'BOOKER, DEVIN', '21-22',
    'CURRY, STEPHEN', '21-22',
    'DURANT, KEVIN', '21-22',
    'TATUM, JAYSON', '21-22',
    'TOWNS, KARL-ANTHONY', '21-22',
    
    'WILLIAMSON, ZION', '22-23',
    'EMBIID, JOEL', '22-23',
    'LILLARD, DAMIAN', '22-23',
    'INGRAM, BRANDON', '22-23',
    'GEORGE, PAUL', '22-23',
    
    'MORANT, JA', '23-24',
    'IRVING, KYRIE', '23-24',
    'DAVIS, ANTHONY', '23-24',
    'BOOKER, DEVIN', '23-24',
    'GEORGE, PAUL', '23-24',
    
    'WEMBANYAMA, VICTOR', '24-25',
    'DURANT, KEVIN', '24-25',
    'MAXEY, TYRESE', '24-25',
    'BOOKER, DEVIN', '24-25',
    'WILLIAMSON, ZION', '24-25'
  ) %>% 
    mutate(medal3 = "<img src='medal3.png' height='20'></img>")
}

get_alldef <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    
    'ANTETOKOUNMPO, GIANNIS', '20-21',
    'DAVIS, ANTHONY', '20-21',
    'DRUMMOND, ANDRE', '20-21',
    'GEORGE, PAUL', '20-21',
    'SIMMONS, BEN', '20-21',
    'GOBERT, RUDY', '20-21',
    'HOLIDAY, JRUE', '20-21',
    'ISAAC, JONATHAN', '20-21',
    'JAMES, LEBRON', '20-21',
    'PAUL, CHRIS', '20-21',
    
    'SIMMONS, BEN', '21-22',
    'CARUSO, ALEX', '21-22',
    'BUTLER, JIMMY', '21-22',
    'ANTETOKOUNMPO, GIANNIS', '21-22',
    'GOBERT, RUDY', '21-22',
    'HOLIDAY, JRUE', '21-22',
    'SMART, MARCUS', '21-22',
    'LEONARD, KAWHI', '21-22',
    'JACKSON, JAREN', '21-22',
    'WILLIAMS, ROBERT', '21-22',
    
    'DAVIS, ANTHONY', '22-23',
    'JACKSON, JAREN', '22-23',
    'LEONARD, KAWHI', '22-23',
    'EMBIID, JOEL', '22-23',
    'WILLIAMS, ROBERT', '22-23',
    'PAUL, CHRIS', '22-23',
    'GEORGE, PAUL', '22-23',
    'SIMMONS, BEN', '22-23',
    'ISAAC, JONATHAN', '22-23',
    'BUTLER, JIMMY', '22-23',
    
    'WEMBANYAMA, VICTOR', '23-24',
    'DAVIS, ANTHONY', '23-24',
    'BUTLER, JIMMY', '23-24',
    'GEORGE, PAUL', '23-24',
    'GILGEOUS-ALEXANDER, SHAI', '23-24',
    'LEONARD, KAWHI', '23-24',
    'BALL, LONZO', '23-24',
    'JACKSON, JAREN', '23-24',
    'ANTETOKOUNMPO, GIANNIS', '23-24',
    'HOLMGREN, CHET', '23-24',
    
    'WEMBANYAMA, VICTOR', '24-25',
    'DAVIS, ANTHONY', '24-25',
    'TURNER, MYLES', '24-25',
    'GILGEOUS-ALEXANDER, SHAI', '24-25',
    'THOMPSON, AMEN', '24-25',
    'BALL, LONZO', '24-25',
    'ANTETOKOUNMPO, GIANNIS', '24-25',
    'HOLIDAY, JRUE', '24-25',
    'DANIELS, DYSON', '24-25',
    'WILLIAMS, ROBERT', '24-25'
  ) %>% 
    mutate(fence = "<img src='fence.png' height='20'></img>")
}

get_allrookie <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    
    'CUNNINGHAM, CADE', '21-22',
    'BARNES, SCOTTIE', '21-22',
    'MOBLEY, EVAN', '21-22',
    'GIDDEY, JOSH', '21-22',
    'SENGUN, ALPEREN', '21-22',
    'GREEN, JALEN', '21-22',
    'SUGGS, JALEN', '21-22',
    'DUARTE, CHRIS', '21-22',
    'WAGNER, FRANZ', '21-22',
    'MITCHELL, DAVION', '21-22',
    
    'BANCHERO, PAOLO', '22-23',
    'DUREN, JALEN', '22-23',
    'SMITH, JABARI', '22-23',
    'HOLMGREN, CHET', '22-23',
    'MURRAY, KEEGAN', '22-23',
    'IVEY, JADEN', '22-23',
    'MATHURIN, BENNEDICT', '22-23',
    'KESSLER, WALKER', '22-23',
    'WILLIAMS, JALEN', '22-23',
    'SHARPE, SHAEDON', '22-23',
    
    'WEMBANYAMA, VICTOR', '23-24',
    'THOMPSON, AUSAR', '23-24',
    'THOMPSON, AMEN', '23-24',
    'BLACK, ANTHONY', '23-24',
    'HENDRICKS, TAYLOR', '23-24',
    'MILLER, BRANDON', '23-24',
    'WHITMORE, CAM', '23-24',
    'HENDERSON, SCOOT', '23-24',
    'GEORGE, KEYONTE', '23-24',
    'VEZENKOV, SASHA', '23-24',
    
    'SARR, ALEX', '24-25',
    'MCCAIN, JARED', '24-25',
    'WARE, KELEL', '24-25',
    'CASTLE, STEPHON', '24-25',
    'KNECHT, DALTON', '24-25',
    'JACKSON, GG', '24-25',
    'EDEY, ZACH', '24-25',
    'CARRINGTON, CARLTON', '24-25',
    'RISACHER, ZACHARIE', '24-25',
    'CARTER, DEVIN', '24-25'
  ) %>% 
    mutate(seed = "<img src='seed.png' height='20'></img>")
}

get_coty <- function() {
  tribble(
    ~AWARD, ~TEAM, ~SEASON,
    'COTY (That1gal)', 'SAC', '20-21',
    'COTY (Kid Monotone)', 'IND', '21-22',
    'COTY (bryn and Q)', 'SAS', '22-23',
    'COTY (Schu)', 'UTA', '23-24',
    'COTY (CF)', 'MEM', '24-25'
  )
}

get_foty <- function() {
  tribble(
    ~TEAM, ~SEASON, ~AWARD,
    'ATL', '20-21', 'FOTY',
    'NOP', '21-22', 'FOTY',
    'SAS', '22-23', 'FOTY',
    'UTA', '23-24', 'FOTY',
    'MEM', '24-25', 'FOTY'
  )
}

get_playoff_seeds <- function() {
  tribble(
    ~SEASON, ~CONF, ~SEED, ~TEAM,
    
    # 25-26
    '25-26 Playoffs', 'EAST', 1, 'CHI',
    '25-26 Playoffs', 'EAST', 2, 'ORL',
    '25-26 Playoffs', 'EAST', 3, 'MIL',
    '25-26 Playoffs', 'EAST', 4, 'TOR',
    '25-26 Playoffs', 'EAST', 5, 'PHI',
    '25-26 Playoffs', 'EAST', 6, 'CLE',
    '25-26 Playoffs', 'EAST', 7, 'MIA',
    '25-26 Playoffs', 'EAST', 8, 'NYK',
    
    '25-26 Playoffs', 'WEST', 1, 'PHX',
    '25-26 Playoffs', 'WEST', 2, 'LAL',
    '25-26 Playoffs', 'WEST', 3, 'DAL',
    '25-26 Playoffs', 'WEST', 4, 'OKC',
    '25-26 Playoffs', 'WEST', 5, 'SAC',
    '25-26 Playoffs', 'WEST', 6, 'SAS',
    '25-26 Playoffs', 'WEST', 7, 'GSW',
    '25-26 Playoffs', 'WEST', 8, 'MEM',
    
    # 24-25
    '24-25 Playoffs', 'EAST', 1, 'BKN',
    '24-25 Playoffs', 'EAST', 2, 'CHI',
    '24-25 Playoffs', 'EAST', 3, 'MIL',
    '24-25 Playoffs', 'EAST', 4, 'IND',
    '24-25 Playoffs', 'EAST', 5, 'PHI',
    '24-25 Playoffs', 'EAST', 6, 'NYK',
    '24-25 Playoffs', 'EAST', 7, 'CLE',
    '24-25 Playoffs', 'EAST', 8, 'ATL',
    
    '24-25 Playoffs', 'WEST', 1, 'SAS',
    '24-25 Playoffs', 'WEST', 2, 'MEM',
    '24-25 Playoffs', 'WEST', 3, 'UTA',
    '24-25 Playoffs', 'WEST', 4, 'NOP',
    '24-25 Playoffs', 'WEST', 5, 'PHX',
    '24-25 Playoffs', 'WEST', 6, 'OKC',
    '24-25 Playoffs', 'WEST', 7, 'SAC',
    '24-25 Playoffs', 'WEST', 8, 'GSW',
    
    # 23-24
    '23-24 Playoffs', 'EAST', 1, 'BKN',
    '23-24 Playoffs', 'EAST', 2, 'IND',
    '23-24 Playoffs', 'EAST', 3, 'MIL',
    '23-24 Playoffs', 'EAST', 4, 'CLE',
    '23-24 Playoffs', 'EAST', 5, 'BOS',
    '23-24 Playoffs', 'EAST', 6, 'CHI',
    '23-24 Playoffs', 'EAST', 7, 'NYK',
    '23-24 Playoffs', 'EAST', 8, 'DET',
    
    '23-24 Playoffs', 'WEST', 1, 'UTA',
    '23-24 Playoffs', 'WEST', 2, 'SAS',
    '23-24 Playoffs', 'WEST', 3, 'GSW',
    '23-24 Playoffs', 'WEST', 4, 'OKC',
    '23-24 Playoffs', 'WEST', 5, 'SAC',
    '23-24 Playoffs', 'WEST', 6, 'PHX',
    '23-24 Playoffs', 'WEST', 7, 'HOU',
    '23-24 Playoffs', 'WEST', 8, 'DAL',
    
    
    # 22-23
    '22-23 Playoffs', 'EAST', 1, 'BKN',
    '22-23 Playoffs', 'EAST', 2, 'IND',
    '22-23 Playoffs', 'EAST', 3, 'CLE',
    '22-23 Playoffs', 'EAST', 4, 'BOS',
    '22-23 Playoffs', 'EAST', 5, 'DET',
    '22-23 Playoffs', 'EAST', 6, 'MIL',
    '22-23 Playoffs', 'EAST', 7, 'NYK',
    '22-23 Playoffs', 'EAST', 8, 'ATL',
    
    '22-23 Playoffs', 'WEST', 1, 'SAS',
    '22-23 Playoffs', 'WEST', 2, 'SAC',
    '22-23 Playoffs', 'WEST', 3, 'DAL',
    '22-23 Playoffs', 'WEST', 4, 'DEN',
    '22-23 Playoffs', 'WEST', 5, 'NOP',
    '22-23 Playoffs', 'WEST', 6, 'HOU',
    '22-23 Playoffs', 'WEST', 7, 'PHX',
    '22-23 Playoffs', 'WEST', 8, 'MIN',
    
    # 21-22
    '21-22 Playoffs', 'EAST', 1, 'IND',
    '21-22 Playoffs', 'EAST', 2, 'MIL',
    '21-22 Playoffs', 'EAST', 3, 'MIA',
    '21-22 Playoffs', 'EAST', 4, 'BKN',
    '21-22 Playoffs', 'EAST', 5, 'DET',
    '21-22 Playoffs', 'EAST', 6, 'ATL',
    '21-22 Playoffs', 'EAST', 7, 'TOR',
    '21-22 Playoffs', 'EAST', 8, 'WAS',
    
    '21-22 Playoffs', 'WEST', 1, 'OKC',
    '21-22 Playoffs', 'WEST', 2, 'NOP',
    '21-22 Playoffs', 'WEST', 3, 'PHX',
    '21-22 Playoffs', 'WEST', 4, 'HOU',
    '21-22 Playoffs', 'WEST', 5, 'DEN',
    '21-22 Playoffs', 'WEST', 6, 'LAL',
    '21-22 Playoffs', 'WEST', 7, 'POR',
    '21-22 Playoffs', 'WEST', 8, 'GSW',
    
    # 20-21
    '20-21 Playoffs', 'EAST', 1, 'MIL',
    '20-21 Playoffs', 'EAST', 2, 'ATL',
    '20-21 Playoffs', 'EAST', 3, 'IND',
    '20-21 Playoffs', 'EAST', 4, 'BKN',
    '20-21 Playoffs', 'EAST', 5, 'BOS',
    '20-21 Playoffs', 'EAST', 6, 'ORL',
    '20-21 Playoffs', 'EAST', 7, 'MIA',
    '20-21 Playoffs', 'EAST', 8, 'TOR',
    
    '20-21 Playoffs', 'WEST', 1, 'SAC',
    '20-21 Playoffs', 'WEST', 2, 'HOU',
    '20-21 Playoffs', 'WEST', 3, 'DAL',
    '20-21 Playoffs', 'WEST', 4, 'GSW',
    '20-21 Playoffs', 'WEST', 5, 'NOP',
    '20-21 Playoffs', 'WEST', 6, 'POR',
    '20-21 Playoffs', 'WEST', 7, 'MIN',
    '20-21 Playoffs', 'WEST', 8, 'DEN'
    
    
  )
}


get_owners <- function() {
  data_dir <- Sys.getenv("NBS_DATA_DIR", "/home/skim/nbs-data")

  season_year <- function(d) {
    yr <- as.integer(format(d, "%Y"))
    mo <- as.integer(format(d, "%m"))
    yr - as.integer(mo < 6L)
  }

  fmt_season <- function(sy) {
    paste0(sprintf("%02d", sy %% 100L), "-", sprintf("%02d", (sy + 1L) %% 100L))
  }

  read_csv(file.path(data_dir, "owners.csv"), show_col_types = FALSE) %>%
    mutate(
      start_date = mdy(start_date),
      TEAM = toupper(team)
    ) %>%
    arrange(TEAM, start_date) %>%
    group_by(TEAM) %>%
    mutate(
      end_date = if_else(
        row_number() < n(),
        lead(start_date) - days(1),
        as.Date(Sys.Date())
      )
    ) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(SEASON = list(fmt_season(season_year(start_date):season_year(end_date)))) %>%
    unnest(SEASON) %>%
    select(SEASON, TEAM, OWNER = owner) %>%
    distinct()
}


get_retired_jerseys <- function() {
  
  tribble(
    ~TEAM, ~PLAYER, ~DATE, ~NO,
    'LAC', 'PAYNE, CAM', '2024-02-08', 15,
    'HOU', 'HARDEN, JAMES', '2024-02-09', 13,
    'SAC', 'POKUSEVSKI, ALEKSEJ', '2024-02-10', 17
  )

}

# Consolidated helper — returns all individual player awards with AWARD label.
# Pass player to filter; omit for the full league-wide table.
get_all_player_awards <- function(player = NULL) {
  x <- bind_rows(
    get_allstars()  %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-Star"),
    get_mvp()       %>% select(PLAYER, SEASON) %>% mutate(AWARD = "Most Valuable Player"),
    get_dpoy()      %>% select(PLAYER, SEASON) %>% mutate(AWARD = "Defensive Player of the Year"),
    get_6moy()      %>% select(PLAYER, SEASON) %>% mutate(AWARD = "Sixth Man of the Year"),
    get_roy()       %>% select(PLAYER, SEASON) %>% mutate(AWARD = "Rookie of the Year"),
    get_mip()       %>% select(PLAYER, SEASON) %>% mutate(AWARD = "Most Improved Player"),
    get_allnbn1()   %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-NBN First Team"),
    get_allnbn2()   %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-NBN Second Team"),
    get_allnbn3()   %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-NBN Third Team"),
    get_alldef()    %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-Defense"),
    get_allrookie() %>% select(PLAYER, SEASON) %>% mutate(AWARD = "All-Rookie")
  )
  if (!is.null(player)) x %>% filter(PLAYER == player) else x
}
