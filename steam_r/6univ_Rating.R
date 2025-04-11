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

#univariate rating
summary(gamesc)
names(gamesc)

#Fréquences: pour voir combien de jeux appartiennent à chaque catégorie de rating

table(gamesc$rating)
#pourcentage des categories

prop.table(table(gamesc$rating))*100

#Visualisation avec un graphique 
ggplot(gamesc, aes(x=rating))+
  geom_bar()+
  labs(title = "Distribution de rating", x="rating", y="Nombre de Participants")

ggplot(gamesc, aes(x = rating)) +
    geom_bar(fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of Game ratings on Steam",
         x = "rating",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


# #Trouver la catégorie la plus fréquente

names(which.max(table(gamesc$rating)))

#Test chi-deux
#influence du variable price sur rating
print(length(gamesc$rating))
print(length(gamesc$Price))

sum(is.na(gamesc$rating))
sum(is.na(gamesc$Price))


chisq.test(table(gamesc$rating, gamesc$Price))


##daniel ----

ggplot(gamesc, aes(x = fct_rev(rating))) + 
    geom_bar(aes(y = ..prop.., group = 1), fill = "steelblue", color = "black") + 
    theme_minimal() + 
    labs(title = "Proportion of Game ratings",
         x = "rating",
         y = "Proportion") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
