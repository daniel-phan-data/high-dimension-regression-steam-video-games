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

##cleaning using steam_clean.R -> cleaned_games ----
source("steam_clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)
##Univariate ----
names(cleaned_games)
summary(cleaned_games)

boxplot(cleaned_games$Average.playtime.forever, 
        main = "Distribution du temps de jeu moyen", 
        ylab = "Temps de jeu moyen (minutes)")

boxplot(cleaned_games$Recommendations, 
        main = "Distribution du nombre de recommendations",
        ylab = "Nombre de recommendations")

boxplot(cleaned_games$Negative, 
        main = "Distribution du nombre d'avis négatifs", 
        ylab = "Nombre d'avis négatifs")

boxplot(cleaned_games$Positive, 
        main = "Distribution du nombre d'avis positifs", 
        ylab = "Nombre d'avis positifs")

boxplot(cleaned_games$Price, 
        main = "Distribution du prix des jeux", 
        ylab = "Prix en dollars")

boxplot(cleaned_games$Peak.CCU, 
        main = "Distribution des maximums de joueurs simultanés atteint", 
        ylab = "Nombre de joueurs simultanés")
