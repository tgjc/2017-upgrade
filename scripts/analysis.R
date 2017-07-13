

# TO DO::
# for loop active filterd
# Logic for active filter
# 
# move col clean etc. to functions script
#
#
#
#





# Set-up and read data
#------------------------------------------------------------------------------------------

# Install / load packages
list_of_packages <- c("tidyverse", "readxl", "stringr", "magrittr", "lubridate")
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
         create_time = format(creation_time, '%H:%M:%S'))  # can this be moved to t_created? or other way round?

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
         solved_tm = format(solved_time, '%H:%M:%S')) %>%  # redundant
  group_by(category_title, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>% 
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
                     category = rep(category, each = length(date_seq)) ) 

# populate blank table with new / closed

trend_tbl %<>% left_join(y = dates)


# replace NAs with zeros 

trend_tbl %<>%


# create active column

trend_tbl %<>%
  group_by(dates) %>%
  mutate(active = sum( )) # if_else







trend_table = NULL
for(i in 1:length(date_lab)){
  for(j in 1:length(emr_cat)){
    n = n+1
  }
  m = m+1
}


x <- matrix(1:6, 2, 3)

for(i in seq_len(nrow(x))) {
  for(j in seq_len(ncol(x))) {
    print(x[i, j])
  }   
}


# Active:
# created before 5pm, status = active, 

# <= 3pm, status = active, category = x


# 1. create vector of date cols from start of report to today()-1

# 2. t_create <- 
  
# 3. 

# 4. left join 

  
  
