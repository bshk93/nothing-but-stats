get_champions <- function(dfs_playoffs) {
  x <- tribble(
    ~SEASON, ~TEAM,
    '20-21 Playoffs', 'ATL',
    '21-22 Playoffs', 'ATL',
    '22-23 Playoffs', 'PHX',
    '23-24 Playoffs', 'CLE'
  )
  
  dfs_playoffs %>% 
    inner_join(x, by = c("SEASON", "TEAM"))
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
    'EDWARDS, ANTHONY', '23-24'
  ) %>% 
    mutate(star = "<img src='star.png' height='20'></img>")
}


get_mvp <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'HARDEN, JAMES', '20-21',
    'JOKIC, NIKOLA', '21-22',
    'DONCIC, LUKA', '22-23',
    'MITCHELL, DONOVAN', '23-24'
  ) %>% 
    mutate(crown = "<img src='crown.png' height='20'></img>")
}


get_dpoy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'ANTETOKOUNMPO, GIANNIS', '20-21',
    'GOBERT, RUDY', '21-22',
    'DAVIS, ANTHONY', '22-23',
    'WEMBANYAMA, VICTOR', '23-24'
  ) %>% 
    mutate(hand = "<img src='hand.png' height='20'></img>")
}


get_roy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'BALL, LAMELO', '20-21',
    'BARNES, SCOTTIE', '21-22',
    'BANCHERO, PAOLO', '22-23',
    'WEMBANYAMA, VICTOR', '23-24'
  ) %>% 
    mutate(baby = "<img src='baby.png' height='20'></img>")
}

get_6moy <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'DINWIDDIE, SPENCER', '20-21',
    'ANTHONY, COLE', '21-22',
    'FULTZ, MARKELLE', '22-23',
    'BANE, DESMOND', '23-24'
  ) %>% 
    mutate(six = "<img src='six.png' height='20'></img>")
}

get_mip <- function() {
  tribble(
    ~PLAYER, ~SEASON,
    'WOOD, CHRISTIAN', '20-21',
    'GARLAND, DARIUS', '21-22',
    'BANE, DESMOND', '22-23',
    'OKONGWU, ONYEKA', '23-24'
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
    'JOKIC, NIKOLA', '23-24'
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
    'CURRY, STEPHEN', '23-24'
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
    'GEORGE, PAUL', '23-24'
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
    'HOLMGREN, CHET', '23-24'
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
    'VEZENKOV, SASHA', '23-24'
  ) %>% 
    mutate(seed = "<img src='seed.png' height='20'></img>")
}

get_coty <- function() {
  tribble(
    ~AWARD, ~TEAM, ~SEASON,
    'COTY (That1guy)', 'SAC', '20-21',
    'COTY (Kid Monotone)', 'IND', '21-22',
    'COTY (bryn and Q)', 'SAS', '22-23',
    'COTY (Schu)', 'UTA', '23-24'
  )
}

get_foty <- function() {
  tribble(
    ~TEAM, ~SEASON, ~AWARD,
    'ATL', '20-21', 'FOTY',
    'NOP', '21-22', 'FOTY',
    'SAS', '22-23', 'FOTY',
    'UTA', '23-24', 'FOTY'
  )
}

get_playoff_seeds <- function() {
  tribble(
    ~SEASON, ~CONF, ~SEED, ~TEAM,
    
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
  tribble(
    ~SEASON, ~TEAM, ~OWNER,
    
    '24-25', 'ATL', 'KVL',
    '24-25', 'BOS', 'Adams17',
    '24-25', 'BKN', 'Egghead',
    '24-25', 'CHA', 'Imma',
    '24-25', 'CHI', 'chitownloyalty',
    '24-25', 'CLE', 'killerdawg7',
    '24-25', 'DAL', 'Guy Fawkes',
    '24-25', 'DEN', 'Darth Awn',
    '24-25', 'DET', 'Ghost',
    '24-25', 'GSW', 'Yerr_ItsKev',
    '24-25', 'HOU', 'Kamal',
    '24-25', 'IND', 'KidMonotone',
    '24-25', 'LAC', 'Mega',
    '24-25', 'LAL', 'AK41',
    '24-25', 'MEM', 'meem',
    '24-25', 'MIA', 'Lance G Buckets',
    '24-25', 'MIL', 'Everinski',
    '24-25', 'MIN', 'Jonny',
    '24-25', 'NOP', 'JDDN',
    '24-25', 'NYK', 'cheppywire',
    '24-25', 'OKC', 'Rodney McDoom',
    '24-25', 'ORL', 'hkd',
    '24-25', 'PHI', 'Kman',
    '24-25', 'PHX', 'chuck',
    '24-25', 'POR', 'FlashThompson11',
    '24-25', 'SAC', 'That1guy',
    '24-25', 'SAS', 'bryn',
    '24-25', 'TOR', 'Not Chris',
    '24-25', 'UTA', 'Schu',
    '24-25', 'WAS', 'Avatar',
    
    '23-24', 'ATL', 'KVL',
    '23-24', 'BOS', 'Adams17',
    '23-24', 'BKN', 'Egghead',
    '23-24', 'CHA', 'fella',
    '23-24', 'CHI', 'chitownloyalty',
    '23-24', 'CLE', 'killerdawg7',
    '23-24', 'DAL', 'Guy Fawkes',
    '23-24', 'DEN', 'Darth Awn',
    '23-24', 'DET', 'CF',
    '23-24', 'GSW', 'Yerr_ItsKev',
    '23-24', 'HOU', 'Kamal',
    '23-24', 'IND', 'KidMonotone',
    '23-24', 'LAC', 'Mega',
    '23-24', 'LAL', 'AK41',
    '23-24', 'MEM', 'meem',
    '23-24', 'MIA', 'HeatCulture',
    '23-24', 'MIL', 'Everinski',
    '23-24', 'MIN', 'Jonny',
    '23-24', 'NOP', 'JDDN',
    '23-24', 'NYK', 'cheppywire',
    '23-24', 'OKC', 'KyleWTF',
    '23-24', 'ORL', 'hkd',
    '23-24', 'PHI', 'Kman',
    '23-24', 'PHX', 'chuck',
    '23-24', 'POR', 'FlashThompson11',
    '23-24', 'SAC', 'That1guy',
    '23-24', 'SAS', 'bryn',
    '23-24', 'TOR', 'Not Chris',
    '23-24', 'UTA', 'Schu',
    '23-24', 'WAS', 'djgmoneyfef',
    
    '22-23', 'ATL', 'nelson',
    '22-23', 'BOS', 'Adams17',
    '22-23', 'BKN', 'Egghead',
    '22-23', 'CHA', 'fella',
    '22-23', 'CHI', 'chitownloyalty',
    '22-23', 'CLE', 'killerdawg7',
    '22-23', 'DAL', 'Guy Fawkes',
    '22-23', 'DEN', 'Darth Awn',
    '22-23', 'DET', 'CF',
    '22-23', 'GSW', 'Yerr_ItsKev',
    '22-23', 'HOU', 'Kamal',
    '22-23', 'IND', 'KidMonotone',
    '22-23', 'LAC', 'Mega',
    '22-23', 'LAL', 'AK41',
    '22-23', 'MEM', 'meem',
    '22-23', 'MIA', 'HeatCulture',
    '22-23', 'MIL', 'Everinski',
    '22-23', 'MIN', 'Jonny',
    '22-23', 'NOP', 'Avatar',
    '22-23', 'NYK', 'cheppywire',
    '22-23', 'OKC', 'KyleWTF',
    '22-23', 'ORL', 'hkd',
    '22-23', 'PHI', 'Kman',
    '22-23', 'PHX', 'chuck',
    '22-23', 'POR', 'FlashThompson11',
    '22-23', 'SAC', 'That1guy',
    '22-23', 'SAS', 'bryn',
    '22-23', 'TOR', 'Not Chris',
    '22-23', 'UTA', 'Schu',
    '22-23', 'WAS', 'djgmoneyfef',
    
    '21-22', 'ATL', 'nelson',
    '21-22', 'BOS', 'Adams17',
    '21-22', 'BKN', 'Egghead',
    '21-22', 'CHA', 'fella',
    '21-22', 'CHI', 'chitownloyalty',
    '21-22', 'CLE', 'killerdawg7',
    '21-22', 'DAL', 'Guy Fawkes',
    '21-22', 'DEN', 'Darth Awn',
    '21-22', 'DET', 'CF',
    '21-22', 'GSW', 'Yerr_ItsKev',
    '21-22', 'HOU', 'Kamal',
    '21-22', 'IND', 'KidMonotone',
    '21-22', 'LAC', 'Mega',
    '21-22', 'LAL', 'AK41',
    '21-22', 'MEM', 'meem',
    '21-22', 'MIA', 'HeatCulture',
    '21-22', 'MIL', 'Everinski',
    '21-22', 'MIN', 'Jonny',
    '21-22', 'NOP', 'Avatar',
    '21-22', 'NYK', 'cheppywire',
    '21-22', 'OKC', 'KyleWTF',
    '21-22', 'ORL', 'AJGoh',
    '21-22', 'PHI', 'Kman',
    '21-22', 'PHX', 'chuck',
    '21-22', 'POR', 'FlashThompson11',
    '21-22', 'SAC', 'That1guy',
    '21-22', 'SAS', 'bryn',
    '21-22', 'TOR', 'Benson - Not Chris',
    '21-22', 'UTA', 'Schu',
    '21-22', 'WAS', 'djgmoneyfef',
    
    '20-21', 'ATL', 'nelson',
    '20-21', 'BOS', 'bjbren',
    '20-21', 'BKN', 'Egghead',
    '20-21', 'CHA', 'fella',
    '20-21', 'CHI', 'chitownloyalty',
    '20-21', 'CLE', 'killerdawg7',
    '20-21', 'DAL', 'Guy Fawkes',
    '20-21', 'DEN', 'Darth Awn',
    '20-21', 'DET', 'CF',
    '20-21', 'GSW', 'Yerr_ItsKev',
    '20-21', 'HOU', 'Kamal',
    '20-21', 'IND', 'KidMonotone',
    '20-21', 'LAC', 'rjsnellings - Mega',
    '20-21', 'LAL', 'Coco',
    '20-21', 'MEM', 'meem',
    '20-21', 'MIA', 'HeatCulture',
    '20-21', 'MIL', 'Everinski',
    '20-21', 'MIN', 'Jonny',
    '20-21', 'NOP', 'Avatar',
    '20-21', 'NYK', 'cheppywire',
    '20-21', 'OKC', 'KyleWTF',
    '20-21', 'ORL', 'AJGoh',
    '20-21', 'PHI', 'Kman',
    '20-21', 'PHX', 'chuck',
    '20-21', 'POR', 'FlashThompson11',
    '20-21', 'SAC', 'That1guy',
    '20-21', 'SAS', 'bryn',
    '20-21', 'TOR', 'odehs',
    '20-21', 'UTA', 'OlePhil - Schu',
    '20-21', 'WAS', 'djgmoneyfef'
    
  )
}


get_retired_jerseys <- function() {
  
  tribble(
    ~TEAM, ~PLAYER, ~DATE, ~NO,
    'LAC', 'PAYNE, CAM', '2024-02-08', 15,
    'HOU', 'HARDEN, JAMES', '2024-02-09', 13,
    'SAC', 'POKUSEVSKI, ALEKSEJ', '2024-02-10', 17
  )
  
}
