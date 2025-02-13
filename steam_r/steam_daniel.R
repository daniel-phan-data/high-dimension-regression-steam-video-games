library("tidyverse")
library("DataExplorer")
library(dplyr)
games <- read.csv("../steam_data/games.csv")
str(games)
#remove games with 0 popularity
games <- games %>% filter(Average.playtime.forever>0 & Peak.CCU>0)
#factoring some variables just in case
games$Estimated.owners <- factor(games$Estimated.owners)
games <- games %>% filter(Estimated.owners != "0 - 20000")
games$Publishers <- factor(games$Publishers)
games$Developers <- factor(games$Developers)
games$Required.age <- factor(games$Required.age)
# keep relevant variables
gamesc <- games %>% select(2,4,5,6,7,20,22,23,24,25,27,29,33,34,35,36,37) 
names(gamesc)
summary(gamesc)
# keep only numerical variables for now
gamesc <- gamesc %>% select(-Tags, -Genres, -Categories, -Publishers,
                            -Score.rank, -User.score, -Metacritic.score, -Developers)
#quick boxplot to visualize the numerical variables
summary(gamesc)
names(gamesc)
label=c(names(gamesc[2:9]))
boxplot(gamesc[2:9], names=label)
#use of log on Peak.ccu to normalize deviation
ggplot(gamesc, aes(y = Peak.CCU)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal()
