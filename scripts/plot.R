# Plot trend line
t_bar <- ggplot(by_day_open_closed, aes(dates, value))
t + geom_bar(aes(fill = ), position = "dodge", stat="identity")



t + theme(axis.text.x = element_text(angle = 45, hjust = 1))
t + theme(axis.text.x = element_text())


https://stackoverflow.com/questions/18158461/grouped-bar-plot-in-ggplot
http://ggplot2.tidyverse.org/reference/position_dodge.html
https://stackoverflow.com/questions/10327267/annotation-above-bars?noredirect=1&lq=1
https://stackoverflow.com/questions/40048002/represent-geom-line-and-geom-bar-in-the-same-plot
