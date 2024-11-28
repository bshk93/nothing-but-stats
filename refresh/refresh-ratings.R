library(rvest)
library(purrr)

get_2k_ratings <- function(urls) {
  map_dbl(urls, function(url) {
    tryCatch({
      page <- read_html(url)
      
      rating <- page %>%
        html_element('body > div.outside-wrapper > div > div.wrapper > div > main > div > div.row.mt-36 > div.col-12.col-md-2.col-lg-2 > div > div > span') %>%
        html_text(trim = TRUE)
      
      #data.frame(rating2k = as.numeric(rating))
      as.numeric(rating)
    }, error = function(e) {
      NA
    })
  })
}

setwd("~/nothing-but-stats")
source("refresh/refresh-utils.R")

dfs <- load_allstats() %>%
  clean_allstats() %>% 
  mutate(gametype = 'REG',
         GAME = NA_integer_,
         ROUND = NA_integer_) %>%
  group_by(PLAYER) %>% 
  mutate(ROOKIE = SEASON == min(SEASON)) %>% 
  ungroup()

dfs_playoffs <- load_allstats(playoffs = TRUE) %>%
  clean_allstats() %>% 
  mutate(gametype = 'PLAYOFF',
         ROOKIE = NA)

dfs_everything <- rbind(dfs, dfs_playoffs)

ratings <- dfs_everything %>% 
  distinct(PLAYER) %>% 
  head(100) %>% 
  mutate(url2k = tolower(str_c(
    "https://2kratings.com/",
    str_extract(PLAYER, "[^,\\s]+$"),
    "-",
    str_extract(PLAYER, "^[^,\\s]+")
  ))) %>% 
  mutate(rating2k = get_2k_ratings(url2k)) %>% 
  select(PLAYER, rating2k)
