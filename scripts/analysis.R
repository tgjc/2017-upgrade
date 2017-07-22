
#------------------------------------------------------------------------------------------
# Set-up and read data
#------------------------------------------------------------------------------------------

# Install / load packages
list_of_packages <- c("tidyverse", "readxl", "stringr", "magrittr", "lubridate", "ggplot2")
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
hpsaw_extract <- 
  read_excel(path = "data/hpsaw-extract.xlsx",
             sheet = "Incident List") %>%
  col_clean() %>% 
  select(id:title, priority, name_name,job_title:incident_location_name,
         status:category_title, current_assignment_name:completion_code,
         creation_time:last_update_time, solved_time) %>%
  rename(category = category_title) %>% 
  mutate(create_date = dmy(format(creation_time, '%d/%m/%Y')),
         create_time = format(creation_time, '%H:%M:%S'),
         solved_dt = dmy(format(solved_time, '%d/%m/%Y')),
         solved_tm = format(solved_time, '%H:%M:%S'))

hpsaw_active_extract <- 
  read_excel(path = "data/hpsaw-active-extract.xlsx",
             sheet = "Incident List") %>%
  col_clean() %>% 
  select(id:title, priority, name_name,job_title:incident_location_name,
         status:category_title, current_assignment_name:completion_code,
         creation_time:last_update_time, solved_time) %>%
  rename(category = category_title) %>% 
  mutate(create_date = dmy(format(creation_time, '%d/%m/%Y')),
         create_time = format(creation_time, '%H:%M:%S'),
         solved_time = as.POSIXct(solved_time))

## test for duplicates in report:
## hpsaw <- semi_join(hpsaw_active_extract, hpsaw_extract, by = "id")

hpsaw <- bind_rows(hpsaw_active_extract, hpsaw_extract)


#------------------------------------------------------------------------------------------
# create / closed / active trend
#------------------------------------------------------------------------------------------

# Created incidents 
t_created <- hpsaw %>% 
  select(create_date, category, solved_time) %>% 
  group_by(category, create_date) %>%
  summarise(created = n() ) %>% 
  ungroup() %>% 
  arrange(category, create_date) %>%  # redundant?
  print

# Closed incidents
t_closed <- hpsaw %>% 
  select(category, solved_time) %>% 
  mutate(solved_dt = dmy(format(solved_time, '%d/%m/%Y')),
         solved_tm = format(solved_time, '%H:%M:%S')) %>%  # <~ redundant?
  group_by(category, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>%                                            # <~ redundant?
  arrange(category, solved_dt) %>% 
  print


# Create blank trend table
category_list <- hpsaw %>%
  select(category) %>%
  distinct() %>%
  extract2(1)
  
start_date <- ymd( readline(prompt = "Please enter start date of report (YYYY/MM/DD): ") )
end_date <- ymd( readline(prompt = "Please enter end date of report (YYYY/MM/DD): ") )
date_seq <- as_date(start_date:end_date)

trend_tbl <- tibble( dates = rep(date_seq, times = length(category_list)),
                     category = rep(category_list, each = length(date_seq)) ) %>% 
             print

# populate table with created / closed
trend_tbl %<>% 
  left_join(y = t_created, 
            by = c("dates" = "create_date", "category" = "category")) %>% 
  mutate(created = if_else(is.na(created), as.integer(0),created)) %>% 
  left_join(y = t_closed, 
            by = c("dates" = "solved_dt", "category" = "category")) %>% 
  mutate(closed = if_else(is.na(closed), as.integer(0),closed)) %>% 
  print

# create active column
t_active <- tibble(dates = as.character(), 
                   category = character(), 
                   active = double())

for(i in extract(date_seq)){
  t_active <- hpsaw %>% 
    select(category, status, create_date, solved_dt) %>% 
    group_by(category) %>% 
    mutate(active = case_when(create_date <= i & is.na(solved_dt) ~ 1,
                              create_date <= i & i < solved_dt ~ 1,
                              TRUE ~ 0) ) %>% 
    summarise(active = sum(active)) %>%
    mutate(dates = as.Date(i, origin = "1970-01-01")) %>% 
    bind_rows(t_active)
}

# Join active to trend table
trend_tbl %<>% 
  left_join(t_active, by = c("dates", "category"))

# Summarise by day
trend_by_day <- 
  trend_tbl %>%
  select(dates:active) %>% 
  group_by(dates) %>% 
  summarise(created = sum(created),
            closed = sum(closed),
            active = sum(active)) %>% 
  print

# Seperate active from created/closed for plotting
by_day_open_closed <- 
  trend_tbl %>%
  select(dates:active) %>% 
  group_by(dates) %>% 
  summarise(created = sum(created),
            closed = sum(closed)) %>% 
  gather(created, closed, key = count, value = value)

by_day_active <- 
  trend_tbl %>%
  select(dates:category, active) %>% 
  group_by(dates) %>% 
  summarise(active = sum(active)) 



