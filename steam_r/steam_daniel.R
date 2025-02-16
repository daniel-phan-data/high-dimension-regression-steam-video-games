library("tidyverse")
library("DataExplorer")
library(dplyr)
library(ggplot2)
library(rlang)  # Pour utiliser sym()

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
games$Required.age <- factor(games$Required.age)
# keep relevant variables
gamesc <- games %>% select(2,4,5,6,7,20,22,23,24,25,27,29,33,34,35,36,37) 
names(gamesc)
summary(gamesc)
# keep only numerical variables for now
gamesc <- gamesc %>% select(-Tags, -Genres, -Categories, -Publishers,
                            -Score.rank, -User.score, -Metacritic.score, -Developers)
## quick boxplot to visualize the numerical variables ----
summary(gamesc)
names(gamesc)
label=c(names(gamesc[2:9]))
boxplot(gamesc[2:9], names=label)

## exemples of uni tests on Peak.CCU ----
#use of log to normalize deviation
ggplot(gamesc, aes(y = var)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal()
# use of sqrt to normalize
ggplot(gamesc, aes(y = Peak.CCU)) + 
  geom_boxplot() + 
  scale_y_sqrt() + 
  theme_minimal()
# density graph
ggplot(gamesc, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal()
# violin plot
ggplot(gamesc, aes(x = 1, y = Peak.CCU)) + 
  geom_violin(fill = "blue", alpha = 0.3) + 
  scale_y_log10() + 
  theme_minimal()
## uni tests but with a macro var ----

names(gamesc) # get names of variables
var <- 'Peak.CCU'  # choose a variable

# boxplot log10
ggplot(gamesc, aes_string(y = var)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal()

# boxplot sqrt
ggplot(gamesc, aes_string(y = var)) + 
  geom_boxplot() + 
  scale_y_sqrt() + 
  theme_minimal()

# density graph log10
ggplot(gamesc, aes_string(x = var)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal()

# violing plot log10
ggplot(gamesc, aes_string(x = "1", y = var)) + 
  geom_violin(fill = "blue", alpha = 0.3) + 
  scale_y_log10() + 
  theme_minimal()

## two variables tests (not finished) ----
# scatter plot
ggplot(gamesc, aes(x = Peak.CCU, y = Average.playtime.forever)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10() + 
  scale_x_log10() + 
  theme_minimal()

