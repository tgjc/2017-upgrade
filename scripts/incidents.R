
#------------------------------------------------------------------------------------------
# Set-up and read data
#------------------------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(magrittr)
library(stringr)
library(readxl)
library(lubridate)

# Function to clean column names
col_clean <- function(x){ 
  colnames(x) <- str_replace_all(colnames(x), " ", "_")
  colnames(x) <- str_to_lower(colnames(x))
  x
}

# Read data, clean col names and split creation time into date/time cols
hpsaw_extract <- 
  read_excel(path = "data/extract.xlsx",
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
  read_excel(path = "data/active-extract.xlsx",
             sheet = "Incident List") %>%
  col_clean() %>% 
  select(id:title, priority, name_name,job_title:incident_location_name,
         status:category_title, current_assignment_name:completion_code,
         creation_time:last_update_time, solved_time) %>%
  rename(category = category_title) %>% 
  mutate(create_date = dmy(format(creation_time, '%d/%m/%Y')),
         create_time = format(creation_time, '%H:%M:%S'),
         solved_time = as.POSIXct(solved_time))

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
         solved_tm = format(solved_time, '%H:%M:%S')) %>%  
  group_by(category, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>%                                            
  arrange(category, solved_dt) 


# Create blank trend table
category_list <- hpsaw %>%
  select(category) %>%
  distinct() %>%
  extract2(1)

# ask user for start end dates for table
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

# for each team:
# - apply case_when to each of their tickets
# - for each active ticket return 1 to active column, otherwise 0
# - sum add the count to their tickets then add to active table
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
  gather(created, closed, active, key = count, value = value)

# Plot trend created, closed + active trend line
t <- ggplot(data = trend_by_day, aes(dates, value, group = count)) +
  geom_col(data = filter(trend_by_day, count != "active"), aes(fill = count), position_dodge(.6)) +
  geom_text(data = filter(trend_by_day,count != "active", value > 0), 
            aes(label = value, y = value + 0.3), 
            position = position_dodge(1), vjust = 0, size = 2.5) + 
  geom_line(data = filter(trend_by_day, count == "active"), size = .75, color = "grey39") +
  geom_text(data = filter(trend_by_day,count == "active"), aes(label = value, y = value + 3), size = 3) +
  geom_point(data = filter(trend_by_day, count == "active"), size = 1.5, color = "grey39") +
  labs(x = "Week Begining", y = "", title = "Active Incidents Trend") +
  guides(fill = guide_legend(title = "Legend")) + 
  theme(legend.position = "bottom")

#------------------------------------------------------------------------------------------
# Active Summaries
#------------------------------------------------------------------------------------------

# data for active summaries
active_sum <- hpsaw %>% 
  select(category, priority, impact, incident_location_name, status, create_date) %>% 
  filter(status == "Active", create_date >= "2017-07-22")

by_priority <- ggplot(active_sum, aes(priority)) +
  geom_bar(fill = "royalblue") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1) +
  labs(x = "", y = "", title = "Active Cases by Priority") + 
  
  ylim(0,15)
  

# Active incidents by category
by_category <- ggplot(active_sum, aes(category)) +
  geom_bar(aes(fill = priority)) + 
  geom_text(stat = 'count', aes(label = ..count..), hjust=-1) +
  labs(x = "", y = "", title = "Active Cases by Category") + 
  coord_flip() +
  ylim(0, 5) +
  theme(legend.position = "bottom")

# Active incidents by impact
by_impact <- ggplot(active_sum, aes(impact)) +
  geom_bar(fill = "royalblue") + 
  geom_text(stat='count',aes(label=..count..),vjust=-1) + 
  labs(x = "", y = "", title = "Active Cases by Impact") + 
  
  ylim(0, 20)

by_impact

# Active incidents by location
by_location <- ggplot(active_sum, aes(incident_location_name)) +
  geom_bar(fill = "royalblue") + 
  geom_text(stat='count', aes(label=..count..),hjust=-1) + 
  labs(x = "", y = "", title = "Active Cases by Location") + 
  coord_flip() +
  ylim(0, 4)

# Tidy environment
rm(list=c("hpsaw_active_extract", "hpsaw_extract", "t_active", "t_closed", 
          "t_created", "trend_tbl"))

rm(list=c("category_list", "date_seq", "start_date", "end_date" , "i"))
  
