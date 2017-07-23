
# Plot trend created, closed + active trend line
t <- ggplot(data = trend_by_day, aes(dates, value, group = count))

t + geom_col(data = filter(trend_by_day, count != "active"), aes(fill = count), position = "dodge") +
  geom_text(data = filter(trend_by_day,count != "active", value > 0), 
            aes(label = value, y = value + 0.3), 
            position = position_dodge(1), vjust = 0) + 
  geom_line(data = filter(trend_by_day, count == "active"), size = .75, color = "grey39") +
  geom_text(data = filter(trend_by_day,count == "active"), aes(label = value, y = value + 1.5)) +
  geom_point(data = filter(trend_by_day, count == "active"), size = 1.5, color = "grey39") +
  labs(x = "week begining", y = "count", title = "Total Incidents Trend") +
  theme(plot.title = element_text(hjust = 0.5))


# https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
# http://ggplot2.tidyverse.org/reference/position_dodge.html
# https://stackoverflow.com/questions/10327267/annotation-above-bars?noredirect=1&lq=1
# https://stackoverflow.com/questions/40048002/represent-geom-line-and-geom-bar-in-the-same-plot




  

