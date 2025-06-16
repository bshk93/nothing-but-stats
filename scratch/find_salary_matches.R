salary_sas <- c(53142264, 30360000, 2349578, 2349578, 2349578)
salary_lac <- c(35000000, 31000000, 14262300, 12822000, 12400000, 7275600, 5168000, 2781053)

salary_lac <- c(35000000, 31000000, 13642200)

df_sas <- tibble(i_33000000 = c(T, F)) %>% 
  cross_join(tibble(i_2162606 = c(T, F))) %>% 
  cross_join(tibble(i2_2162606 = c(T, F))) %>% 
  #cross_join(tibble(i3_2349578 = c(T, F))) %>% 
  
  mutate(total_sas = {
    logical_cols <- select(., where(is.logical))
    weights <- str_extract(names(logical_cols), "\\d+$") %>% as.numeric()
    rowSums(as.matrix(logical_cols) * matrix(rep(weights, each = nrow(.)), ncol = length(weights)))
  })

df_lac <- cross_join(
  tibble(i_35000000 = c(T, F)),
  tibble(i_31000000 = c(T, F))
) %>% 
  cross_join(tibble(i_13642200 = c(T, F))) %>% 
  cross_join(tibble(i_11600000 = c(T, F))) %>% 
  cross_join(tibble(i_6945240 = c(T, F))) %>% 
  cross_join(tibble(i_5168000 = c(T, F))) %>% 
  cross_join(tibble(i_2625000 = c(T, F))) %>% 
  
  mutate(total_lac = {
    logical_cols <- select(., where(is.logical))
    weights <- str_extract(names(logical_cols), "\\d+$") %>% as.numeric()
    rowSums(as.matrix(logical_cols) * matrix(rep(weights, each = nrow(.)), ncol = length(weights)))
  })

df_sas %>% 
  distinct(total_sas) %>% 
  cross_join(distinct(df_lac, total_lac)) %>% 
  mutate(diff = abs(total_sas - total_lac)) %>% 
  arrange(diff)
