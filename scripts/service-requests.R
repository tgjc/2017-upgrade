
#------------------------------------------------------------------------------------------
# Set-up and read data
#------------------------------------------------------------------------------------------

# Install / load packages
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
sr_extract <- 
  read_excel(path = "data/sr-hpsaw-extract-2017.07.25.xlsx",
             sheet = "Request List") %>%
  col_clean() %>% 
  mutate(create_date = dmy(format(created_time, '%d/%m/%Y')),
         create_time = format(created_time, '%H:%M:%S'),
         solved_dt = dmy(format(closed_time, '%d/%m/%Y')),
         solved_tm = format(closed_time, '%H:%M:%S'))

sr_active_extract <- 
  read_excel(path = "data/sr-hpsaw-active-extract-2017.07.25.xlsx",
             sheet = "Request List") %>%
  col_clean() %>% 
  mutate(create_date = dmy(format(created_time, '%d/%m/%Y')),
         create_time = format(created_time, '%H:%M:%S'),
         solved_time = as.POSIXct(closed_time))

## test for duplicates in report:
## hpsaw <- semi_join(hpsaw_active_extract, hpsaw_extract, by = "id")

sr_hpsaw <- bind_rows(sr_active_extract, sr_extract) %>% 
  rename(category = current_assignment_name)


#------------------------------------------------------------------------------------------
# create / closed / active trend
#------------------------------------------------------------------------------------------

# Created incidents 
sr_created <- sr_hpsaw %>% 
  select(create_date, category, solved_time) %>% 
  group_by(category, create_date) %>%
  summarise(created = n() ) %>% 
  ungroup() %>% 
  arrange(category, create_date)  # redundant?

# Closed incidents
sr_closed <- sr_hpsaw %>% 
  select(category, solved_dt) %>%
  filter(!is.na(solved_dt)) %>% 
  group_by(category, solved_dt) %>% 
  summarise(closed = n()) %>%
  ungroup() %>%                                            
  arrange(category, solved_dt) 


# Create blank trend table
sr_category_list <- sr_hpsaw %>%
  select(category) %>%
  distinct() %>%
  extract2(1)

start_date <- ymd( readline(prompt = "Please enter start date of report (YYYY/MM/DD): ") )
end_date <- ymd( readline(prompt = "Please enter end date of report (YYYY/MM/DD): ") )
date_seq <- as_date(start_date:end_date)

sr_trend_tbl <- tibble( dates = rep(date_seq, times = length(sr_category_list)),
                     category = rep(sr_category_list, each = length(date_seq)) )

# populate table with created / closed
sr_trend_tbl %<>% 
  left_join(y = sr_created, 
            by = c("dates" = "create_date", "category" = "category")) %>% 
  mutate(created = if_else(is.na(created), as.integer(0),created)) %>% 
  left_join(y = sr_closed, 
            by = c("dates" = "solved_dt", "category" = "category")) %>% 
  mutate(closed = if_else(is.na(closed), as.integer(0),closed))

# create active column
sr_active <- tibble(dates = as.character(), 
                   category = character(), 
                   active = double())

for(i in extract(date_seq)){
  sr_active <- sr_hpsaw %>% 
    select(category, status, create_date, solved_dt) %>% 
    group_by(category) %>% 
    mutate(active = case_when(create_date <= i & is.na(solved_dt) ~ 1,
                              create_date <= i & i < solved_dt ~ 1,
                              TRUE ~ 0) ) %>% 
    summarise(active = sum(active)) %>%
    mutate(dates = as.Date(i, origin = "1970-01-01")) %>% 
    bind_rows(sr_active)
}

# Join active to trend table
sr_trend_tbl %<>% 
  left_join(sr_active, by = c("dates", "category"))

# Summarise by day
sr_trend_by_day <- 
  sr_trend_tbl %>%
  select(dates:active) %>% 
  group_by(dates) %>% 
  summarise(created = sum(created),
            closed = sum(closed),
            active = sum(active)) %>% 
  gather(created, closed, active, key = count, value = value)

# Plot trend created, closed + active trend line
sr_t <- ggplot(data = sr_trend_by_day, aes(dates, value, group = count)) +
  geom_col(data = filter(sr_trend_by_day, count != "active"), aes(fill = count), position_dodge(.6)) +
  geom_text(data = filter(sr_trend_by_day,count != "active", value > 0), 
            aes(label = value, y = value + 0.3), 
            position = position_dodge(1), vjust = 0, size = 3) + 
  geom_line(data = filter(sr_trend_by_day, count == "active"), size = .75, color = "grey39") +
  geom_text(data = filter(sr_trend_by_day,count == "active"), aes(label = value, y = value + 4), size = 3) +
  geom_point(data = filter(sr_trend_by_day, count == "active"), size = 1.5, color = "grey39") +
  labs(x = "Week Begining", y = "Total", title = "Active Service Requests Trend") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5)) +
  guides(fill = guide_legend(title = "Legend"))


#------------------------------------------------------------------------------------------
# Active Summaries
#------------------------------------------------------------------------------------------

# data for active summaries
# sr_active_sum <- sr_hpsaw %>% 
#  select(category, priority, status, create_date) %>% 
#  filter(status == "Active", create_date >= "2017-07-22")  # <~ need to add create_date >= 2017-07-22

# Active incidents by priority
# sr_by_priority <- ggplot(sr_active_sum, aes(priority)) +
#  geom_bar(fill = "royalblue") + 
#  geom_text(stat='count',aes(label=..count..),vjust=-1) +
#  labs(x = "Priority", y = "Total", title = "Active Service Requests by Priority") + 
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5))


# Active incidents by category
# sr_by_category <- ggplot(sr_active_sum, aes(category)) +
#  geom_bar(aes(fill = priority)) + 
#  geom_text(stat = 'count', aes(label = ..count..), vjust=-1) +
#  labs(x = "Category", y = "Total", title = "Active Service Requests by Category") + 
#  theme_minimal() +
#  theme(plot.title = element_text(hjust = 0.5)) +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#  ggplot2::ylim(0,20)

# Active incidents by impact
# by_impact <- ggplot(active_sum, aes(impact)) +
#  geom_bar(fill = "royalblue") + 
#  geom_text(stat='count',aes(label=..count..),vjust=-1) + 
#  labs(x = "Impact", y = "Total", title = "Active Cases by Impact") + 
#  theme_minimal() + 
#  theme(plot.title = element_text(hjust = 0.5)) + 
#  ylim(0, 7)

# Active incidents by location
#by_location <- ggplot(active_sum, aes(incident_location_name)) +
#  geom_bar(fill = "royalblue") + 
#  geom_text(stat='count', aes(label=..count..),hjust=-1) + 
#  labs(x = "", y = "Total", title = "Active Cases by Location") + 
#  coord_flip() +
#  theme_minimal() + 
#  theme(plot.title = element_text(hjust = 0.5)) 

