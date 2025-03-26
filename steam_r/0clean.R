##library imports ----
library("tidyverse")
library("DataExplorer")
library(dplyr)

load_games <- function() {
  filepath <- "../steam_data/games.csv"
  games <- read.csv(filepath)
}

clean_games <- function() {
  games <- load_games()
  games <- games %>% filter(Average.playtime.forever>0 & Peak.CCU>0)
  #factoring some variables just in case
  games$Estimated.owners <- factor(games$Estimated.owners)
  games <- games %>% filter(Estimated.owners != "0 - 20000")
  games$Publishers <- factor(games$Publishers)
  games$Developers <- factor(games$Developers)
  # keep relevant variables
  gamesc <- games %>% select(2,4,5,6,7,20,22,23,24,25,27,29,33,34,35,36,37) 
  names(gamesc)
  summary(gamesc)
  # keep only numerical variables for now
  gamesc <- gamesc %>% select(-Tags, -Genres, -Categories, -Publishers,
                              -Score.rank, -User.score, -Metacritic.score, -Developers)
  gamesc[is.na(gamesc)] <- 0
  total_reviews <- gamesc$Positive + gamesc$Negative
  positive_ratio <- (gamesc$Positive / total_reviews) * 100  # Ratio des avis positifs
  
  # create rating
  gamesc <- gamesc %>% mutate(rating = mapply(create_rating, Positive, Negative))
  # define rating category order
  rating_levels <- c("Overwhelmingly Positive", "Very Positive", "Positive", 
                     "Mostly Positive", "Mixed", 
                     "Mostly Negative", "Negative", "Very Negative", 
                     "Overwhelmingly Negative", "Not enough reviews")
  # Convertir rating en facteur avec un ordre défini
  gamesc <- gamesc %>%
    mutate(Rating = factor(rating, levels = rating_levels, ordered = TRUE))
  
  # Réorganiser les colonnes
  gamesc <- gamesc %>%
    select(Name, Average.playtime.forever, Estimated.owners,
           Peak.CCU, Rating, Price,
           Recommendations, Required.age,
           Positive, Negative
           )
  return(gamesc)
}

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