

# Set-up and read data
#------------------------------------------------------------------------------------------

# Install / load packages
list_of_packages <- c("tidyverse", "readxl", "stringr", "magrittr", "lubridate", "read_xl")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]

if(length(new_packages)) install.packages(new_packages)

lapply(X = list_of_packages, FUN = require, character.only = TRUE)

# Function to clean column names
col_clean <- function(x){ 
  colnames(x) <- str_replace_all(colnames(x), " ", "_")
  colnames(x) <- str_to_lower(colnames(x))
  x
}

# Read data, clean col names and split creation time into date/time cols
hpsaw <- read_excel(path = "data/hpsaw_test_excel.xlsx",
                    sheet = "Incident List") %>%
  col_clean() %>% 
  select(id:title, priority, name_name,job_title:incident_location_name,
         status:category_title, current_assignment_name:completion_code,
         creation_time:last_update_time, solved_time) %>%
  mutate(create_date = dmy(format(creation_time, '%d/%m/%Y')),
         create_time = format(creation_time, '%H:%M:%S'),
         solved_dt = dmy(format(solved_time, '%d/%m/%Y')),
         solved_tm = format(solved_time, '%H:%M:%S'))

#------------------------------------------------------------------------------------------
# create / closed / active trend
#------------------------------------------------------------------------------------------

# New incidents 
t_created <- hpsaw %>% 
  select(create_date, category_title, solved_time) %>% 
  group_by(category_title, create_date) %>%
  summarise(created = n() ) %>% 
  ungroup() %>% 
  arrange(category_title, create_date) %>%  # redundant?
  print

# Closed incidents
t_closed <- hpsaw %>% 
  select(category_title, solved_time) %>% 
  mutate(solved_dt = dmy(format(solved_time, '%d/%m/%Y')),
         solved_tm = format(solved_time, '%H:%M:%S')) %>%  # <~ redundant?
  group_by(category_title, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>%                                            # <~ redundant?
  arrange(category_title, solved_dt) %>% 
  print


# Create blank trend table
category <- hpsaw %>%
  select(category_title) %>%
  distinct() %>%
  extract2(1)
  
start_date <- ymd( readline(prompt = "Please enter start date of report (YYYY/MM/DD): ") )
end_date <- ymd( readline(prompt = "Please enter end date of report (YYYY/MM/DD): ") )
date_seq <- as_date(start_date:end_date)

trend_tbl <- tibble( dates = rep(date_seq, times = length(category)),
                     category = rep(category, each = length(date_seq)) ) %>% 
             print

# populate table with new / closed
trend_tbl %<>% 
  left_join(y = t_created, 
            by = c("dates" = "create_date", "category" = "category_title")) %>% 
  mutate(created = if_else(is.na(created), as.integer(0),created)) %>% 
  left_join(y = t_closed, 
            by = c("dates" = "solved_dt", "category" = "category_title")) %>% 
  mutate(closed = if_else(is.na(closed), as.integer(0),closed)) %>% 
  print

# create active column

# created before 5pm, status = active or solved date < i 
# <= 3pm, status = active, category = x



for(i in extract(date_seq)){
  hpsaw %>% 
    select(category_title, status, create_date, solved_dt) %>% 
    group_by(category_title) %>% 
    mutate(
      active = case_when(
        is.na(solved_dt) ~ 1,
        tolower(status) == "active" ~ 1,
        i < solved_dt ~ 1,
        TRUE ~ 0
      )
    ) %>%
    summarise(active = sum(active)) %>%
    mutate(dates = as.Date(i, origin = "1970-01-01")) %>% 
    print
}


  
  
