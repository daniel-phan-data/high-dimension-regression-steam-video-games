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
lillie.test(gamesc$Price)
lillie.test(gamesc$Peak.CCU)
lillie.test(gamesc$Average.playtime.forever) # Y
#two variables tests : Spearman test
cor.test(gamesc$Price, gamesc$Peak.CCU, method = "spearman",exact=FALSE)
