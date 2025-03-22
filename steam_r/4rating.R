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
  positive_ratio <- (Positive / total_reviews) * 100  # Ratio des avis positifs
  
  if (total_reviews >= 500) {  
    if (positive_ratio >= 95 && positive_ratio <= 100) {
      return("Overwhelmingly Positive")
    } else if (positive_ratio >= 80 && positive_ratio < 95) {
      return("Very Positive")
    } else if (positive_ratio >= 70 && positive_ratio < 80) {
      return("Mostly Positive")
    } else if (positive_ratio >= 40 && positive_ratio < 70) {
      return("Mixed")
    } else if (positive_ratio >= 20 && positive_ratio < 40) {
      return("Mostly Negative")
    } else if (positive_ratio >= 0 && positive_ratio < 20) {
      return("Overwhelmingly Negative")
    }
  } 
  
  else if (total_reviews >= 50 && total_reviews <= 499) {  
    if (positive_ratio >= 80 && positive_ratio <= 100) {
      return("Very Positive")
    } else if (positive_ratio >= 70 && positive_ratio < 80) {
      return("Mostly Positive")
    } else if (positive_ratio >= 40 && positive_ratio < 70) {
      return("Mixed")
    } else if (positive_ratio >= 20 && positive_ratio < 40) {
      return("Mostly Negative")
    } else if (positive_ratio >= 0 && positive_ratio < 20) {
      return("Very Negative")
    }
  } 
  
  else if (total_reviews >= 10 && total_reviews <= 49) {  # 10-49 reviews
    if (positive_ratio >= 80 && positive_ratio <= 100) {
      return("Positive")
    } else if (positive_ratio >= 70 && positive_ratio < 80) {
      return("Mostly Positive")
    } else if (positive_ratio >= 40 && positive_ratio < 70) {
      return("Mixed")
    } else if (positive_ratio >= 20 && positive_ratio < 40) {
      return("Mostly Negative")
    } else if (positive_ratio >= 0 && positive_ratio < 20) {
      return("Negative")
    }
  } 
  
  else {
    return("Not enough reviews")  
  }
}
gamesc <- gamesc %>% mutate(rating = mapply(create_rating, Positive, Negative))

# Définir l'ordre des niveaux du plus positif au plus négatif
rating_levels <- c("Overwhelmingly Positive", "Very Positive", "Positive", 
                   "Mostly Positive", "Mixed", 
                   "Mostly Negative", "Negative", "Very Negative", 
                   "Overwhelmingly Negative", "Not enough reviews")

# Convertir rating en facteur avec un ordre défini
gamesc <- gamesc %>%
  mutate(rating = factor(rating, levels = rating_levels, ordered = TRUE))

# Réorganiser les colonnes : Positive, Negative et rating en 2ᵉ, 3ᵉ et 4ᵉ position
gamesc <- gamesc %>%
  select(1, Positive, Negative, rating, everything())

effectifs <- gamesc %>% count(rating)
