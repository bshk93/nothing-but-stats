load_allstats <- function(playoffs = FALSE) {
  ptrn <- "allstats-\\d"
  pstr <- ""
  if (playoffs) {
    ptrn <- "allstats-playoffs"
    pstr <- " Playoffs"
  }
  
  # Find all allstats files in R/ directory
  list.files("./data/", ptrn) %>% 
    map(function(fp) {
      tmp_season <- as.numeric(str_extract(fp, "\\d{2}\\."))
      
      data.table::fread(str_c('./data/', fp)) %>% 
        tibble() %>% 
        mutate_if(is.numeric, as.numeric) %>% 
        mutate(SEASON = str_c(tmp_season-1, "-", tmp_season, pstr)) %>% 
        filter(!is.na(SEASON))
    })
}