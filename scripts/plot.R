# Plot trend line
t <- ggplot(hpsaw_trend, aes(create_date, solved_dt))
t + geom_bar(position = "dodge")

trend + theme(axis.text.x = element_text(angle = 45, hjust = 1))
trend + theme(axis.text.x = element_text())


g <- ggplot(mpg, aes(class))
g + geom_bar()
g + geom_bar(aes(fill = drv))


# First you need to get the counts for each category, i.e. how many Bads and Goods and so on are there for each group (Food, Music, People). 
# This would be done like so:
  
  raw <- read.csv("http://pastebin.com/raw.php?i=L8cEKcxS",sep=",")
raw[,2]<-factor(raw[,2],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)
raw[,3]<-factor(raw[,3],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)
raw[,4]<-factor(raw[,4],levels=c("Very Bad","Bad","Good","Very Good"),ordered=FALSE)

raw=raw[,c(2,3,4)] # getting rid of the "people" variable as I see no use for it

freq=table(col(raw), as.matrix(raw)) # get the counts of each factor level


# Then you need to create a data frame out of it, melt it and plot it:
  
  Names=c("Food","Music","People")     # create list of names
data=data.frame(cbind(freq),Names)   # combine them into a data frame
data=data[,c(5,3,1,2,4)]             # sort columns

# melt the data frame for plotting
data.m <- melt(data, id.vars='Names')

# plot everything
ggplot(data.m, aes(Names, value)) +   
  geom_bar(aes(fill = variable), position = "dodge", stat="identity")