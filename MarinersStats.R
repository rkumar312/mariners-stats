library(readr)
library(dplyr)
library(ggplot2)

df <- read_excel("BaseballStatistics.xlsx", sheet="MarinersBaseball")

print(head(df))
print(tail(df))
print(colnames(df))

summarise(df)

df %>% 
  summarise(mean_ba = mean(BA, na.rm = TRUE)) # method 1: calculating mean batting average of team


df %>%
  group_by(Name) %>%
  summarise(mean_obp = mean(OBP, na.rm = TRUE)) # method 2: displaying on base percentage for each player


var(df$Age, na.rm = TRUE) # calculate variance in age of team


IQR(df$SB, na.rm = TRUE) # calculate IQR of stolen bases

result <- t.test(df$BB, df$AB) # hypothesis test of walks and at-bats
print(result)


result2 <- t.test(df$`3B`) # hypothesis test of triples
print(result2)



plot(df$Rk, df$RBI) # very basic plot of RBI and Rank, not very descriptive or appealing

viz_dot <- ggplot(data = df, aes(x=AB, y=RBI)) + geom_point() + geom_smooth() + 
  labs(title = "Number of RBI vs Number of AB", y="Number of RBI", x="Number of AB")
print(viz_dot)
# basic scatter/line plot graphing number of RBI over AB


viz_bar <- ggplot(data = df, aes(x=HR)) + geom_bar() + 
  labs(title = "Number of players with certain Number of HR", y="Number of Players", x="Number of HR")
print(viz_bar)
# bar chart displaying number of people with a certain number of HR





