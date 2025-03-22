##library imports ----
library("tidyverse")
library("DataExplorer")
library(dplyr)
##data import ----
filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)

##cleaning using steam_clean.R ----
source("0clean.R")  # Source the cleaning file
gamesc <- clean_games(games)
##create rating variable ----
create_rating <- function(Positive, Negative) {
  total_reviews <- Positive + Negative
  if (total_reviews >= 500) {
    if (Positive / total_reviews * 100 >= 95) {
      return("Overwhelmingly Positive")
    } 
    if (Positive / total_reviews * 100 >= 80 & Positive / total_reviews * 100 < 95) {
      return("Very Positive")
    }
  } else if (total_reviews >= 50 & total_reviews < 500) {
    if (Positive / total_reviews * 100 >= 80) {
      return("Very Positive")
    } 
    if (Positive / total_reviews * 100 >= 70 & Positive / total_reviews * 100 < 80) {
      return("Mostly Positive")
    } 
    if (Positive / total_reviews * 100 >= 40 & Positive / total_reviews * 100 < 70) {
      return("Mixed")
    } 
    if (Positive / total_reviews * 100 >= 20 & Positive / total_reviews * 100 < 40) {
      return("Mostly Negative")
    } 
    if (Positive / total_reviews * 100 < 20) {
      return("Very Negative")
    }
  } else {
    if (Positive / total_reviews * 100 >= 80) {
      return("Positive")
    } 
    if (Positive / total_reviews * 100 >= 70 & Positive / total_reviews * 100 < 80) {
      return("Mostly Positive")
    } 
    if (Positive / total_reviews * 100 >= 40 & Positive / total_reviews * 100 < 70) {
      return("Mixed")
    } 
    if (Positive / total_reviews * 100 >= 20 & Positive / total_reviews * 100 < 40) {
      return("Mostly Negative")
    } 
    if (Positive / total_reviews * 100 < 20) {
      return("Negative")
    }
  }
}

gamesc <- gamesc %>% mutate(rating = mapply(create_rating, Positive, total_reviews))
print(gamesc$rating)

