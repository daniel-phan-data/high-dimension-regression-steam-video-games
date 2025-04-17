## IMPORTS ----
rm(list = ls()) #clean environment
graphics.off() #clean plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory
temp_env <- new.env() #temporary environment to avoid uneccessary variables after import
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env) #delete temporary environment after data has been loaded

#select variables for analysis
gamesc <- games %>%
  select(Average.playtime.forever, Estimated.owners,
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

