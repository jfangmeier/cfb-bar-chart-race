library(tidyverse)
library(rvest)
library(janitor)

school_list_html <- 'https://www.sports-reference.com/cfb/schools/' %>% 
  read_html()

school_link <- school_list_html %>% 
  html_nodes(xpath = '//*[@id="schools"]/tbody/tr/td/a') %>% 
  html_attr('href') %>% 
  str_subset(pattern = "schools")

school_tbl <- school_list_html %>% 
  html_nodes(xpath = '//*[@id="schools"]') %>% 
  html_table() %>% 
  as.data.frame() %>% 
  row_to_names(row_number = 1, remove_row = TRUE) %>% 
  clean_names() %>% 
  filter(!school %in% c("", "School")) %>% 
  cbind(school_link) %>% 
  mutate(school_link = as.character(school_link))

saveRDS(school_tbl)

school_vector <- school_tbl %>% 
  pull(school)


school_records_fcn <- function(x, .pb=NULL) {
  # setup the progress bar
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  Sys.sleep(0.001)
  
  # the rest of the function
  school_name_proper <- x
  
  school_url <- school_tbl %>%
    filter(school == x) %>% 
    pull(school_link)
  
  school_name_lower <- strsplit(school_url, split = "/")[[1]] %>% 
    tail(1)
    
  Sys.sleep(runif(n=1,min = 0.5,max = 2))
  
  school_html <- paste0('https://www.sports-reference.com', school_url) %>% 
    read_html()
  
  school_html %>%
    html_nodes(xpath = paste0('//*[@id="', school_name_lower, '"]')) %>%
    html_table() %>%
    as.data.frame() %>%
    clean_names() %>%
    filter(rk != "Rk") %>% #remove extra header rows
    mutate(school = school_name_proper) %>%
    select(-rk) %>%
    select(school, year, everything()) %>%
    mutate_all(list(~as.character(.))) %>% #convert all columns to character
    as_tibble()
}

pb <- progress_estimated(length(school_vector))

team_record_raw <- map_dfr(school_vector, school_records_fcn, .pb = pb) %>% 
  inner_join(school_tbl %>% select(school, from, to), by = "school")

saveRDS(team_record_raw, here::here("data", "team_record_raw.rds"))