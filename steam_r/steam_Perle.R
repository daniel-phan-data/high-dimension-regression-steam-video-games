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

filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)
#filepath <- here("steam_data", "games.csv")  # perle's import
#games <- read.csv(filepath)

##cleaning using steam_clean.R ----
source("steam_clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)

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
## normality + spearman (bivar) ----
#normality test
library(nortest)
lillie.test(gamesc$Positive)
lillie.test(gamesc$Price)
lillie.test(gamesc$Negative)
lillie.test(gamesc$Peak.CCU)
lillie.test(gamesc$Average.playtime.forever) # Y
lillie.test(gamesc$Recommendations)
gamesc$Estimated.owners2 <- (as.numeric(sub("-.*", "", gamesc$Estimated.owners)) + as.numeric(sub(".*-", "",gamesc$Estimated.owners))) / 2
lillie.test(gamesc$Estimated.owners2)
#two variables tests : Spearman test
cor.test(gamesc$Price, gamesc$Positive, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Negative, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Peak.CCU, method = "spearman",exact=FALSE)
cor.test(gamesc$Price, gamesc$Estimated.owners2, method = "spearman",exact=FALSE)

# matrice de correlation

#recodage des revues positives

total_reviews <- sum(gamesc$Positive,gamesc$Negative)
recoder_positive <- function(gamesc$Positive, total_reviews) {
  if (total_reviews >= 500) {
    if (gamesc$Positive >= 95) {
      return("Overwhelmingly Positive")
    } else if (gamesc$Positive >= 80) {
      return("Very Positive")
    }
  } else if (total_reviews >= 50) {
    if (gamesc$Positive >= 80) {
      return("Very Positive")
    } else if (gamesc$Positive >= 70) {
      return("Mostly Positive")
    } else if (gamesc$Positive >= 40) {
      return("Mixed")
    } else if (gamesc$Positive >= 20) {
      return("Mostly Negative")
    } else {
      return("Very Negative")
    }
  } else {
    if (gamesc$Positive >= 80) {
      return("Positive")
    } else if (gamesc$Positive >= 70) {
      return("Mostly Positive")
    } else if (gamesc$Positive >= 40) {
      return("Mixed")
    } else if (gamesc$Positive >= 20) {
      return("Mostly Negative")
    } else {
      return("Negative")
    }
  }
}


rating <- mapply(recoder_positive, gamesc$positive, gamesc$total_reviews)
print(rating)


