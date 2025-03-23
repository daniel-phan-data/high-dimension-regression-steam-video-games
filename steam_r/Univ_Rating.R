##library imports ----
library("tidyverse")
library("DataExplorer")
library(dplyr)

source(~../model_lin_steam/steam_r/0clean.R)

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
  labs(title = "Distribution de Rating", x="Rating", y="Nombre de Paricipants")

#Trouver la catégorie la plus fréquente

names(which.max(table(gamesc$Rating)))

#Test chi-deux 
#influence du variable price sur Rating 

chisq.test(table(gamesc$cRating, gamesc$Price))
