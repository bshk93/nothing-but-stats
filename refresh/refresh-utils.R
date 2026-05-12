library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(glue)
library(magrittr)
library(rlang)
library(lubridate)

get_allstats <- function(delete_before = NULL) {
  
  published_sheets <- list(
    ATL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSVQPPiBZXGvr5_YrTPOcrtSmhGx_5h3OvFrtixmj90UKoB8MieGp4pI5zWZ8mK5BbVs5Y-BcMNjNpr/pub?output=csv",
    BKN = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS48bNFTtW4Xa9nTv1EmvVUnXjyd7b8HwJoiQSbJxRmAZdwQpeWBmS8jBSfDw3vBlr7wLNVTUanHnh6/pub?output=csv",
    BOS = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTgg-WcXdqi_DA__nsExAHO5mlwvmKPvMus5LPtxo0sAmxmksY_-_wv7xIRetusk5T3S8oaMDbxgMEl/pub?output=csv",
    CHA = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQSnhgTiJsHPf9RDVDW2gd2e2nmD3cCaLRqPl7oAgHZQJHVEoctk0Ya4PsGWqY_hPitWCShGoh6VWN4/pub?output=csv",
    CHI = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQVBXhAUc5j_ZMV6wlT2w2jhEigsd2-31vEQVqPUX2pjmq6Etke5gNDnWLHUNX1E4yAk2UwgZuecIRQ/pub?output=csv",
    CLE = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQPVjXtt6wvdqDsirS0gGfWP-SJXrgrNmk55K19-3Hg6k94iz6sSvZpCicjpj6OnIlRbeTxZeoeLAOR/pub?output=csv",
    DAL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTaQ7Fnw9HZwfdXh5qapGpkivgFxQGadm4GdX3rtE_rTcBtGbuqv1qBioSECPWd4ldNHZmvXgn5G0rr/pub?output=csv",
    DEN = "https://docs.google.com/spreadsheets/d/e/2PACX-1vS0m8veRtnfg8yZs0TqSFeBnmaMERtqkEqorXuRWOyy3nQ3vaVEWBTKMsyP9hLPbV4hCxjEtaVs-dP9/pub?output=csv",
    DET = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRbTp_ohikAg4v7TLMZaqxdkDWtnAEsAZWW3j_xxMzHKFb4Q9yOG9ae11ibMcA2vj_2fYSLqmRBkAgY/pub?output=csv",
    GSW = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTyLQ_7xcUFEqTB8s1s_uLIis5-KujCvVPZpTSGV8UeW2w0kJUnJmwRbKVvGxq5l88qTuAxM4uuDXzb/pub?output=csv",
    HOU = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQLHKR8aUkeOOEWD68GBS8I6aQ2MgWpxsXQAPp1Q-ncjMzIJP0xbGYIRNzumS2gAvxSopAOhc9p-IqD/pub?output=csv",
    IND = "https://docs.google.com/spreadsheets/d/e/2PACX-1vT9zyCX1e4yX7P30wSOlTkCzHVFLVWaSfh_Fp256y4WqxmAOHjxRnCuEJxtrDKTYZ4-xgQz1Vs8L5Gr/pub?output=csv",
    LAC = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ7JWGfSFGRs8tVZuQIzdhmWNKxvhgKF5bID4mCX_a4adCH9tChmXmsVb44G1l-wd01cX-7vRQHHeym/pub?output=csv",
    LAL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vSCLJYAjNjMhfKpTcWNyeTvuM2giiPJKKD1y5wlGCFW9NFvQXb2UW2dCW43b-RdpK-qtRH6BOF1M0zO/pub?output=csv",
    MEM = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRMy08E-qrlvIcYjLyoi1NLcP9VBXUXaUG7ER6BqOJP318rxKoU35ZvrooYZnLTv-K0GHFIaqHIPSZ5/pub?output=csv",
    MIA = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTKlLEk44g6sDeRGquubGIf6-C7dbiRTuAbtmEPXCUtKs1KuZp_UFHcDcMlzhrXFGTs742KQ_uYdaPx/pub?output=csv",
    MIL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRvrO7RIo9wiRRu2M-oQkf-VLB7U2g1kn925c_FlcbAk7J1rFga5qJRSOhdAGfP7WsJmAm-_Vwl21p-/pub?output=csv",
    MIN = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRx9A4qqy_9OVny4T3USO3-6gPibYkiVnJ_LnUjGcK-VeGYDycEKJ6dsmqLvwW25Wq7jPaBQXK0O_d7/pub?output=csv",
    NOP = "https://docs.google.com/spreadsheets/d/e/2PACX-1vT-ntsnGJUgGQ6vwhMAJdnyKTNtx6o_bALCYGqj5xh6IXPOVJAMdJz6JnRuyMEYdFNvScOse7OTygK9/pub?output=csv",
    NYK = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRZ-M-La7MiF_drCEZiv4V32lsDL7hki7LbXieEC-K4f-oj296qPgoL9KJEUDHP-kaN6znCIIH-he8K/pub?output=csv",
    OKC = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTsoHz2tRavkWoGI41riJTDQBzIl0iID8TeOXfB7jy7OQVYOvtZ3j0AAqraG0zhBzar-dEK695EypT_/pub?output=csv",
    ORL = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQVz9dlSs1i5oQgOIqQRANso8aglLukGM93J8-gV2H2sInyLs6vusPYOffDdiZvRTE_jqODfdDirbGW/pub?output=csv",
    PHI = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRyinaO06CrlUcuC64F83yOYVa0AzAW4NMhlJIAX0stw3vZL5l_hOILqAEeKxLDC75w5DyVJFJOfjgK/pub?output=csv",
    PHX = "https://docs.google.com/spreadsheets/d/e/2PACX-1vRXDbSndKtCBXOrv9kuq0OWCD8JQuZs_vzQMuCZy7kRxfoJR9KDxVHP0gcmXIvTwk8fNr9vl_PgZogT/pub?output=csv",
    POR = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQT8xLexHvD7IBwbx_xPQx14rFEiIgBaZFlKKGQRWqMtNX4k-HyPEoyrduQzNW6R7mfPzWi1Eg6Fdyf/pub?output=csv",
    SAC = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTUJh_w8_E8FB9A1KfaaQ4NbvC9v-9NeinAsKEPmk4a4KjN5n-luRqYAhMKxHSfO_XQSxS6FBKvNwnR/pub?output=csv",
    SAS = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQTAENSLXS5eP4xlKLdMBp5DSJEUghm7ITWAtFrJty2C0iBvniKTJ1rlQxGzTAfgZbtXfbMNuHVgv5J/pub?output=csv",
    TOR = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ2-mu5DU266E5qr2BPePYZ2Q65ar_4ODqgi4Mm9XEBr6PX6X88cBMndwRdZhhHVueLANusUZTmH3Lc/pub?output=csv",
    UTA = "https://docs.google.com/spreadsheets/d/e/2PACX-1vQgpUd8iXQKt6BaqSIMHriujTU00LSN5_BPUuHZXVn9HMoSyTC8X7jvJdCDrLOQtCiwofqWiAp2bMK7/pub?output=csv",
    WAS = "https://docs.google.com/spreadsheets/d/e/2PACX-1vT74XJ3ZjaXzFqo67C54JaehUSOP5aMk2Iim9jk9xNPcmB2r6wuFaN1bzAAFSnXNFZvWmCu0lI3I8qN/pub?output=csv"
  )
  
  # Pull down stats
  x <- published_sheets %>% 
    imap_dfr(import_team_sheet)
  
  if (!is.null(delete_before)) {
    x <- x %>% 
      filter(DATE >= delete_before)
  }
  
  x
}

import_team_sheet <- function(myurl, team) {
  if (myurl != "") {
    message(glue("Downloading data from {team}..."))
    read_csv(myurl,
             col_types = cols(
               DATE = col_character(),
               OPP = col_character(),
               PLAYER = col_character(),
               M = col_integer(),
               P = col_integer(),
               R = col_integer(),
               A = col_integer(),
               S = col_integer(),
               B = col_integer(),
               TO = col_integer(),
               FGM = col_integer(),
               FGA = col_integer(),
               `3PM` = col_integer(),
               `3PA` = col_integer(),
               FTM = col_integer(),
               FTA = col_integer(),
               OR = col_integer(),
               PF = col_integer()
             )) %>% 
      
      # Filter out sample rows
      #filter(PLAYER != "Roast, Brandon", PLAYER != "Huck, Charles") %>% 
      
      # Filter out unfinished rows
      filter(!is.na(M)) %>% 
      
      # Clean
      mutate(DATE = lubridate::as_date(DATE, format = "%m/%d/%Y"),
             PLAYER = toupper(PLAYER),
             TEAM = team,
             OPP_RAW = str_replace(OPP, "^@", ""),
             DR = R - OR,
             FGPCT = case_when(
               FGA == 0 ~ NA_real_,
               TRUE ~ FGM/FGA
             ),
             `3PPCT` = case_when(
               `3PA` == 0 ~ NA_real_,
               TRUE ~ `3PM`/`3PA`
             ),
             FTPCT = case_when(
               FTA == 0 ~ NA_real_,
               TRUE ~ FTM/FTA
             )) %>% 
      select(TEAM, DATE, OPP, OPP_RAW, PLAYER, M, P, R, OR, DR, A, S, B, TO, 
             FGM, FGA, FGPCT, `3PM`, `3PA`, `3PPCT`, FTM, FTA, FTPCT, PF,
             everything()) %>% 
      
      mutate()
  } else {
    tibble()
  }
}


# Build/check functions ----
check_allstats <- function(allstats) {
  bad_minute_games = NULL
  bad_sanity_checks = NULL
  bad_missing = NULL
  
  bad_minute_games <- allstats %>% group_by(TEAM, DATE, OPP) %>% 
    summarize(t_min = sum(M)) %>% 
    filter(t_min != 240 & t_min != 265 & t_min != 290 & t_min != 315)
  
  if (nrow(bad_minute_games) > 0) {
    warning(glue("There are {nrow(bad_minute_games)} game(s) where the total minutes doesn't make sense: {str_c(bad_minute_games$t_min, collapse = ', ')}"))
  }
  
  bad_sanity_checks <- allstats %>% 
    filter(OR > R | DR > R | FGM > FGA | `3PM` > `3PA` | FTM > FTA | PF > 6 | P != FTM + 2*FGM + 1*`3PM`)
  
  if (nrow(bad_sanity_checks) > 0) {
    warning(glue("There are {nrow(bad_sanity_checks)} row(s) where the numbers don't make sense: {str_c(bad_sanity_checks$PLAYER, collapse = ', ')}"))
  }
  
  bad_missing <- allstats %>% 
    filter(if_any(c(DATE, PLAYER, M, P, R, A, S, B, TO, FGA, FGM, `3PA`, `3PM`, FTM, FTA, PF), is.na))
  
  if (nrow(bad_missing) > 0) {
    warning(glue("There are {nrow(bad_missing)} row(s) with missing data."))
  }
  
  list(
    data = allstats,
    errors = list(
      bad_minute_games = bad_minute_games,
      bad_sanity_checks = bad_sanity_checks,
      bad_missing = bad_missing,
      games = bind_rows(
        select(bad_minute_games, "TEAM", "DATE") %>% mutate(REASON = "bad total minutes"),
        select(bad_sanity_checks, "TEAM", "DATE") %>% mutate(REASON = "data don't make sense"),
        select(bad_missing, "TEAM", "DATE") %>% mutate(REASON = "missing data")
      ) %>% 
        distinct()
    )
  )
}





# test_allstats <- function(allstats, check_start_date) {
#   # allstats %>% select(TEAM, DATE, OPP, PLAYER) %>% 
#   #   group_by(DATE, PLAYER) %>% mutate(date_n = n()) %>% 
#   #   filter(date_n > 1)
#   
#   for (mytestdate in ymd(check_start_date):max(allstats$DATE)) {
#     x <- allstats %>% filter(DATE == mytestdate) %>% 
#       distinct(PLAYER, TEAM) %>% 
#       anti_join(allstats %>% filter(DATE < mytestdate) %>% distinct(PLAYER),
#                 by = "PLAYER")
#     
#     if (nrow(x) > 0) {
#       for (i in 1:nrow(x)) {
#         warning(x$PLAYER[i], " (", x$TEAM[i], ") appeared in the data for the first time on ", mytestdate, ".")
#       }
#     }
#   }
#   
# }


build_allstats <- function(allstats) {
  player_data <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQe3Mo26VfCnIpb28_fOM_3866nb2nE2sK3WcHIf0rgR_YfxdC_NFWQBWMPc6XbkK2LHxJti9IBsjod/pub?gid=0&single=true&output=csv",
                          skip = 1) %>% 
    select(NAME = Name...1, 
           DOB,
           EXP = `2021 Exp`, COLLEGE, COUNTRY)
  
  allstats %>% 
    mutate(OPP_TEAM = str_replace(OPP, "@", ""),
           TD = case_when(
             (P > 9) + (R > 9) + (A > 9) + (S > 9) + (B > 9) >= 3 ~ 1,
             TRUE ~ 0
           ),
           BOX = P + R + A + S + B)  %>% 
    mutate(" " = "") %>% 
    group_by(TEAM, DATE, OPP) %>% 
    mutate(TEAM_PTS = sum(P)) %>% 
    ungroup() %>% 
    left_join(
      allstats %>% group_by(TEAM, DATE) %>% summarize(OPP_TEAM_PTS = sum(P)),
      by = c("OPP_TEAM" = "TEAM", "DATE")
    ) %>% 
    left_join(player_data %>% mutate(DOB = mdy(DOB)) %>% select(NAME, DOB),
              by = c("PLAYER" = "NAME")) %>% 
    mutate(AGE = interval(DOB, DATE) / years(1)) %>% 
    select(-DOB) %>% 
    mutate(WL = case_when(
      TEAM_PTS > OPP_TEAM_PTS ~ "W",
      TEAM_PTS < OPP_TEAM_PTS ~ "L",
      TRUE ~ NA_character_
    ))
}

# Add playoff info
add_playoff_info <- function(built_allstats) {
  game_round <- built_allstats %>% 
    distinct(TEAM, DATE, OPP_TEAM) %>%
    arrange(TEAM, DATE, OPP_TEAM) %>%
    group_by(TEAM, OPP_TEAM) %>%
    mutate(GAME = row_number()) %>%
    ungroup() %>%
    group_by(TEAM) %>%
    arrange(TEAM, DATE) %>%
    mutate(ROUND = cumsum(case_when(OPP_TEAM == lag(OPP_TEAM) ~ 0, TRUE ~ 1)))
  
  built_allstats %>% 
    left_join(game_round)
}

clean_allstats <- function(dfs) {
  dfs_bind <- dfs %>% 
    bind_rows() %>% 
    select(-any_of(c("...27", "V27", "...28", "V28"))) %>% 
    mutate(FG = str_c(FGM, "-", FGA),
           `3P` = str_c(`3PM`, "-", `3PA`),
           FT = str_c(FTM, "-", FTA),
           GMSC = P + (0.4 * FGM) - (0.7 * FGA) - (0.4 * (FTA - FTM)) + (0.7 * OR) + 
             (0.3 * DR) + S + (0.7 * A) + (0.7 * B) - (0.4 * PF) - TO,
           TS = 0.5*P/(FGA + .475*FTA)) %>% 
    mutate(PLAYER = case_when(
      PLAYER == "KANTER, ENES" ~ "FREEDOM, ENES",
      PLAYER == "BAMBA, MO" ~ "BAMBA, MOHAMED",
      PLAYER == "CAREY JR., VERNON" ~ "CAREY, VERNON",
      PLAYER == "CHAMAGNIE, JUSTIN" ~ "CHAMPAGNIE, JUSTIN",
      PLAYER == "HAMMONDS, RAYSHON" ~ "HAMMONDS, RAYSHAUN",
      PLAYER == "MATTHEWS, WES" ~ "MATTHEWS, WESLEY",
      PLAYER == "O'NEALE, ROYCE" ~ "ONEALE, ROYCE",
      PLAYER == "PIPPEN, SCOTTIE" ~ "PIPPEN, SCOTTY",
      PLAYER == "ROBINSON, GLENNN" ~ "ROBINSON, GLENN",
      PLAYER == "WHITE, COLBY" ~ "WHITE, COBY",
      PLAYER == "BERTANS,DAVIS" ~ "BERTANS, DAVIS",
      PLAYER == "HIGHSMITH, HAYDEN" ~ "HIGHSMITH, HAYWOOD",
      PLAYER == "THOMAS, CAMERON" ~ "THOMAS, CAM",
      PLAYER == "REDDISH, CAMERON" ~ "REDDISH, CAM",
      TRUE ~ PLAYER
    )) %>% 
    mutate(GMSC = round(GMSC, 2)) %>% 
    
    arrange(PLAYER, DATE)
  
  if (!("OPP_RAW" %in% names(dfs_bind))) {
    dfs_bind <- dfs_bind %>% 
      mutate(OPP_RAW = str_replace(OPP, "^@", ""))
  }
  
  dfs_bind
}

load_allstats <- function(playoffs = FALSE, data_dir = Sys.getenv("NBS_DATA_DIR", "/home/skim/nbs-data")) {
  ptrn <- "allstats-\\d"
  pstr <- ""
  if (playoffs) {
    ptrn <- "allstats-playoffs"
    pstr <- " Playoffs"
  }

  list.files(data_dir, ptrn) %>%
    map(function(fp) {
      tmp_season <- as.numeric(str_extract(fp, "\\d{2}\\."))

      data.table::fread(file.path(data_dir, fp)) %>%
        tibble() %>%
        mutate_if(is.numeric, as.numeric) %>%
        mutate(DATE = as.Date(DATE)) %>%
        mutate(SEASON = str_c(tmp_season-1, "-", tmp_season, pstr)) %>%
        filter(!is.na(SEASON))
    })
}

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

write_team_profiles <- function(dfs, dfs_playoffs, standings_list, team_ratings, out_dir) {
  teams <- sort(unique(dfs$TEAM))

  po_wins <- dfs_playoffs %>%
    distinct(SEASON, TEAM, DATE, WL) %>%
    mutate(SEASON = str_remove(SEASON, " Playoffs")) %>%
    group_by(SEASON, TEAM) %>%
    summarize(PO_W = sum(WL == "W", na.rm = TRUE), .groups = "drop")

  champions <- get_champion_list() %>%
    mutate(SEASON = str_remove(SEASON, " Playoffs"))

  playoff_result_table <- get_playoff_seeds() %>%
    mutate(SEASON = str_remove(SEASON, " Playoffs")) %>%
    select(SEASON, TEAM) %>%
    left_join(po_wins, by = c("SEASON", "TEAM")) %>%
    mutate(PO_W = replace_na(PO_W, 0L)) %>%
    left_join(champions %>% mutate(IS_CHAMPION = TRUE), by = c("SEASON", "TEAM")) %>%
    mutate(
      IS_CHAMPION    = replace_na(IS_CHAMPION, FALSE),
      PLAYOFF_RESULT = case_when(
        IS_CHAMPION ~ "Champion",
        PO_W >= 12  ~ "Runner-Up",
        PO_W >= 8   ~ "Conf Finals",
        PO_W >= 4   ~ "Second Round",
        TRUE        ~ "First Round"
      )
    )

  foty <- get_foty() %>% select(TEAM, SEASON)
  coty <- get_coty() %>% select(TEAM, SEASON)

  for (team in teams) {
    slug <- tolower(team)

    players_df <- dfs %>%
      filter(TEAM == team) %>%
      group_by(PLAYER) %>%
      summarize(
        GP       = n(),
        GMSC_TOT = round(sum(GMSC,   na.rm = TRUE), 1),
        GMSC_AVG = round(mean(GMSC,  na.rm = TRUE), 2),
        PPG      = round(mean(P,     na.rm = TRUE), 1),
        RPG      = round(mean(R,     na.rm = TRUE), 1),
        APG      = round(mean(A,     na.rm = TRUE), 1),
        SPG      = round(mean(S,     na.rm = TRUE), 1),
        BPG      = round(mean(B,     na.rm = TRUE), 1),
        `3PMPG`  = round(mean(`3PM`, na.rm = TRUE), 1),
        SEASONS  = paste(sort(unique(SEASON)), collapse = ", "),
        .groups  = "drop"
      ) %>%
      arrange(desc(GMSC_TOT)) %>%
      slice_head(n = 100)

    write_csv(players_df, file.path(out_dir, paste0(slug, "-players.csv")))

    seasons_df <- map_dfr(names(standings_list), function(s) {
      row <- standings_list[[s]] %>% filter(TEAM == team)
      if (nrow(row) == 0) return(NULL)
      row %>% mutate(
        SEASON   = s,
        SEED_NUM = as.integer(str_extract(SEED, "\\d+$"))
      )
    }) %>%
      left_join(
        team_ratings %>% ungroup() %>% filter(TEAM == team) %>% select(SEASON, OFF_RTG, DEF_RTG),
        by = "SEASON"
      ) %>%
      left_join(
        playoff_result_table %>% filter(TEAM == team) %>% select(SEASON, PLAYOFF_RESULT),
        by = "SEASON"
      ) %>%
      left_join(foty %>% filter(TEAM == team) %>% transmute(SEASON, FOTY = TRUE), by = "SEASON") %>%
      left_join(coty %>% filter(TEAM == team) %>% transmute(SEASON, COTY = TRUE), by = "SEASON") %>%
      mutate(
        PLAYOFF_RESULT = replace_na(PLAYOFF_RESULT, "Missed"),
        FOTY           = replace_na(FOTY, FALSE),
        COTY           = replace_na(COTY, FALSE)
      ) %>%
      select(SEASON, W, L, PCT, PPG, OPPG, DIFF, SEED, SEED_NUM,
             OFF_RTG, DEF_RTG, PLAYOFF_RESULT, FOTY, COTY) %>%
      arrange(SEASON)

    write_csv(seasons_df, file.path(out_dir, paste0(slug, "-seasons.csv")))
  }
}