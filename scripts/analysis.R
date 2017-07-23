
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
  arrange(category, create_date)  # redundant?

# Closed incidents
t_closed <- hpsaw %>% 
  select(category, solved_time) %>% 
  mutate(solved_dt = dmy(format(solved_time, '%d/%m/%Y')),
         solved_tm = format(solved_time, '%H:%M:%S')) %>%  # <~ redundant?
  group_by(category, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>%                                            # <~ redundant?
  arrange(category, solved_dt) 


# Create blank trend table
category_list <- hpsaw %>%
  select(category) %>%
  distinct() %>%
  extract2(1)
  
start_date <- ymd( readline(prompt = "Please enter start date of report (YYYY/MM/DD): ") )
end_date <- ymd( readline(prompt = "Please enter end date of report (YYYY/MM/DD): ") )
date_seq <- as_date(start_date:end_date)

trend_tbl <- tibble( dates = rep(date_seq, times = length(category_list)),
                     category = rep(category_list, each = length(date_seq)) )

# populate table with created / closed
trend_tbl %<>% 
  left_join(y = t_created, 
            by = c("dates" = "create_date", "category" = "category")) %>% 
  mutate(created = if_else(is.na(created), as.integer(0),created)) %>% 
  left_join(y = t_closed, 
            by = c("dates" = "solved_dt", "category" = "category")) %>% 
  mutate(closed = if_else(is.na(closed), as.integer(0),closed))

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
  gather(created, closed, active, key = count, value = value) %>% 
  print

# Plot trend created, closed + active trend line
t <- ggplot(data = trend_by_day, aes(dates, value, group = count))

t + geom_col(data = filter(trend_by_day, count != "active"), aes(fill = count), position_dodge(.6)) +
  geom_text(data = filter(trend_by_day,count != "active", value > 0), 
            aes(label = value, y = value + 0.3), 
            position = position_dodge(1), vjust = 0, size = 3) + 
  geom_line(data = filter(trend_by_day, count == "active"), size = .75, color = "grey39") +
  geom_text(data = filter(trend_by_day,count == "active"), aes(label = value, y = value + 1.5), size = 3) +
  geom_point(data = filter(trend_by_day, count == "active"), size = 1.5, color = "grey39") +
  labs(x = "Week Begining", y = "Total", title = "Active Incidents Trend") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Legend"))
  


#------------------------------------------------------------------------------------------
# Active Summaries
#------------------------------------------------------------------------------------------

# data for active summaries
active_sum <- hpsaw %>% 
  select(category, priority, status, create_date) %>% 
  filter(status == "Active")  # <~ need to add create_date >= 2017-07-22

# Active incidents by priority
by_priority <- ggplot(active_sum, aes(priority)) +
  geom_bar(fill = "royalblue") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1) +
  labs(x = "Priority", y = "Total", title = "Active Cases by Priority") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

# Active incidents by category
by_category <- ggplot(active_sum, aes(category)) +
  geom_bar(aes(fill = priority)) + 
  geom_text(stat = 'count', aes(label = ..count..), vjust=-1) +
  labs(x = "Category", y = "Total", title = "Active Cases by Category") + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggplot2::ylim(0,20)

