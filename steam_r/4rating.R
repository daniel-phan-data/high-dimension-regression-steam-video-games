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

##cleaning using steam_clean.R ----
source("0clean.R")  # Source the cleaning file
gamesc <- clean_games(games)
##create rating variable ----
total_reviews <- sum(gamesc$Positive,gamesc$Negative)
create_rating <- function(Positive, total_reviews) {
  if (total_reviews >= 500) {
    if (Positive >= 95) {
      return("Overwhelmingly Positive")
    } else if (Positive >= 80) {
      return("Very Positive")
    }
  } else if (total_reviews >= 50) {
    if (Positive >= 80) {
      return("Very Positive")
    } else if (Positive >= 70) {
      return("Mostly Positive")
    } else if (Positive >= 40) {
      return("Mixed")
    } else if (Positive >= 20) {
      return("Mostly Negative")
    } else {
      return("Very Negative")
    }
  } else {
    if (Positive >= 80) {
      return("Positive")
    } else if (Positive >= 70) {
      return("Mostly Positive")
    } else if (Positive >= 40) {
      return("Mixed")
    } else if (Positive >= 20) {
      return("Mostly Negative")
    } else {
      return("Negative")
    }
  }
}

gamesc <- gamesc %>% mutate(rating = mapply(create_rating, Positive, total_reviews))
print(gamesc$rating)