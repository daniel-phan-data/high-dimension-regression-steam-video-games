## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)
gamesc <- games %>%
  select(Name, Publishers, Average.playtime.forever, Estimated.owners,
         Peak.CCU, rating, Price,
         Recommendations, Required.age,
         Positive, Negative,
         total_reviews, positive_ratio)

## tables ----

#Fréquences: pour voir combien de jeux appartiennent à chaque catégorie de rating

table(gamesc$rating)
#pourcentage des categories

prop.table(table(gamesc$rating))*100


## graph ----

ggplot(gamesc, aes(x = fct_rev(rating))) + 
    geom_bar(aes(y = ..prop.., group = 1), fill = "steelblue", color = "black") + 
    theme_minimal() + 
    labs(title = "Proportion of Game ratings",
         x = "rating",
         y = "Proportion") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
