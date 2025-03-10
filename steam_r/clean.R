# file to clean database before working on it
library(tidyverse)

clean_games <- function(games) {
  games <- games %>% filter(Average.playtime.forever>0 & Peak.CCU>0)
  #factoring some variables just in case
  games$Estimated.owners <- factor(games$Estimated.owners)
  games <- games %>% filter(Estimated.owners != "0 - 20000")
  games$Publishers <- factor(games$Publishers)
  games$Developers <- factor(games$Developers)
  # keep relevant variables
  cleaned_games <- games %>% select(2,4,5,6,7,20,22,23,24,25,27,29,33,34,35,36,37) 
  names(cleaned_games)
  summary(cleaned_games)
  # keep only numerical variables for now
  cleaned_games <- cleaned_games %>% select(-Tags, -Genres, -Categories, -Publishers,
                              -Score.rank, -User.score, -Metacritic.score, -Developers)
  cleaned_games[is.na(cleaned_games)] <- 0
  return(cleaned_games)
}
