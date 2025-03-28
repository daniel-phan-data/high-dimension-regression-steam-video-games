##library imports ----
library("tidyverse")
library("DataExplorer")
library(dplyr)
library(ggplot2)
library(forcats)
rm(list = ls())

# source(~../model_lin_steam/steam_r/0clean.R)
##load cleaned data ----
source("0clean.R")  # Source the cleaning file
gamesc <- clean_games()

#univariate Rating
summary(gamesc)
names(gamesc)

#Fréquences: pour voir combien de jeux appartiennent à chaque catégorie de rating

table(gamesc$Rating)
#pourcentage des categories

prop.table(table(gamesc$Rating))*100

#Visualisation avec un graphique 
ggplot(gamesc, aes(x=Rating))+
  geom_bar()+
  labs(title = "Distribution de Rating", x="Rating", y="Nombre de Participants")

ggplot(gamesc, aes(x = Rating)) +
    geom_bar(fill = "steelblue", color = "black") +
    theme_minimal() +
    labs(title = "Distribution of Game Ratings on Steam",
         x = "Rating",
         y = "Count") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability


# #Trouver la catégorie la plus fréquente

names(which.max(table(gamesc$Rating)))

#Test chi-deux
#influence du variable price sur Rating
print(length(gamesc$Rating))
print(length(gamesc$Price))

sum(is.na(gamesc$Rating))
sum(is.na(gamesc$Price))


chisq.test(table(gamesc$Rating, gamesc$Price))


##daniel ----

ggplot(gamesc, aes(x = fct_rev(Rating))) + 
    geom_bar(aes(y = ..prop.., group = 1), fill = "steelblue", color = "black") + 
    theme_minimal() + 
    labs(title = "Proportion of Game Ratings",
         x = "Rating",
         y = "Proportion") +
    coord_flip() +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))
