##library imports ----
library("tidyverse")
library("DataExplorer")
library(dplyr)
library(here)
library(ggplot2)
library(nortest)
library(ggcorrplot)
library(corrplot)

##data import ----
filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)
#filepath <- here("steam_data", "games.csv")  # perle's import
#games <- read.csv(filepath)

##cleaning using clean.R ----
source("clean.R")  # Source the cleaning file
gamesc <- clean_games(games)

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
