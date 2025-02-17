#no installs in the code
#every library should be at the top
#filepath comment should be in english
#applying log10 on some variables make their distribution look a lot like normal laws
#could be interesting to also test spearson
## ton git a marche
#
library("tidyverse")
library("DataExplorer")
library(dplyr)

install.packages("here")  
library(here)

filepath <- here("steam_data", "games.csv")  # Chemin relatif propre
games <- read.csv(filepath)
#filepath <- "../steam_data/games.csv"
#games <- read.csv(filepath)
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

##  single variables tests on Peak.CCU ----
#use of log on Peak.ccu to normalize deviation
ggplot(gamesc, aes(y = Peak.CCU)) + 
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
  geom_density() + 
  scale_x_log10() + 
  theme_minimal()
# violin plot
ggplot(gamesc, aes(x = 1, y = Peak.CCU)) + 
  geom_violin() + 
  scale_y_log10() + 
  theme_minimal()
# outlier on another plot
gamesc <- gamesc %>%
  mutate(Outlier = ifelse(Peak.CCU > quantile(Peak.CCU, 0.99), "Outlier", "Normal"))

ggplot(gamesc, aes(x = Outlier, y = Peak.CCU)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  theme_minimal()
## two variables tests (not finished) ----
# scatter plot
ggplot(gamesc, aes(x = Peak.CCU, y = Average.playtime.forever)) + 
  geom_point(alpha = 0.5) + 
  scale_y_log10() + 
  scale_x_log10() + 
  theme_minimal()

#normality test
library(nortest)
lillie.test(gamesc$Positive)
lillie.test(gamesc$Price)
lillie.test(gamesc$Negative)
lillie.test(gamesc$Peak.CCU)
lillie.test(gamesc$Average.playtime.forever)
lillie.test(gamesc$Recommendations)
gamesc$Estimated.owners2 <- (as.numeric(sub("-.*", "", gamesc$Estimated.owners)) + as.numeric(sub(".*-", "",gamesc$Estimated.owners))) / 2
lillie.test(gamesc$Estimated.owners2)
#two variables tests : Spearman test
cor.test(gamesc$Price, gamesc$Positive, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Negative, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Peak.CCU, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Estimated.owners2, method = "spearman",exact=FALSE)
