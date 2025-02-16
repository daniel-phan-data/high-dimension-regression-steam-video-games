library("tidyverse")
library("DataExplorer")
library(dplyr)

filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)
## quick rough cleaning ----
#remove games with 0 popularity
games <- games %>% filter(Average.playtime.forever>0 & Peak.CCU>0)
#factoring some variables just in case
games$Estimated.owners <- factor(games$Estimated.owners)
games <- games %>% filter(Estimated.owners != "0 - 20000")
games$Publishers <- factor(games$Publishers)
games$Developers <- factor(games$Developers)
# keep relevant variables
gamesc <- games %>% select(2,4,5,6,7,20,22,23,24,25,27,29,33,34,35,36,37) 
names(gamesc)
summary(gamesc)
# keep only numerical variables for now
gamesc <- gamesc %>% select(-Tags, -Genres, -Categories, -Publishers,
                            -Score.rank, -User.score, -Metacritic.score, -Developers)

## Peak.CCU density log10 ----
ggplot(gamesc, aes(x = Peak.CCU)) + 
  geom_density() + 
  scale_x_log10() + 
  theme_minimal()
## required age histogram ----
x <- gamesc$Required.age
hist(x,breaks=15,col="blue",
     xlab="Age restriction",
     ylab="Frequency",
     main="Age restriction distribution",tck=0.01, freq=FALSE)
box()
# superposing a density curve
densite <- density(x)
lines(densite, col = "purple",lwd=3)

## price density ----
ggplot(gamesc, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal()

##Positive density log10 ----
ggplot(gamesc, aes(x = Positive)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal()

## Negative density log10----
ggplot(gamesc, aes(x = Negative)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal()

## Recommendations density log10 ----
ggplot(gamesc, aes(x = Recommendations)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal()

## Average playtime forever----
ggplot(gamesc, aes(x = Average.playtime.forever)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal()

