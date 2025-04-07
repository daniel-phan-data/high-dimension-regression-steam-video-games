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
#filepath <- here("steam_data", "games.csv")  # perle's import
#games <- read.csv(filepath)

##cleaning using steam_clean.R -> cleaned_games ----
source("steam_clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)
##Univariate ----
names(cleaned_games)
summary(cleaned_games)

#Le temps de jeu moyen varie considérablement, avec 
#des valeurs allant de 0 à plusieurs centaines de milliers de minutes. On remarque une
#distribution asymétrique du temps de jeu moyen avec une médiane relativement basse,
#ce qui indique que la plupart des jeux ont un temps de jeu moyen modéré, tandis que quelques jeux ont des temps de jeu très élevés.

boxplot(cleaned_games$Average.playtime.forever, 
        main = "Distribution du temps de jeu moyen", 
        ylab = "Temps de jeu moyen (minutes)")

#La majeure partie des jeux possède un nombre de
#recommandations relativement faibles, concentrées autour de la médiane. Cependant, la 
#présence de nombreux valeurs extrêmes indique que certains jeux ont reçu un nombre 
#de recommandations extrêmement élevé par rapport aux autres. Cette distribution indique que seuls certains jeux particulièrement populaires 
#dominent le classement.

boxplot(cleaned_games$Recommendations, 
        main = "Distribution du nombre de recommendations",
        ylab = "Nombre de recommendations")

#Le nombre d’avis négatifs varie considérablement, avec des valeurs allant de 0 à plusieurs millions. 
#Certains jeux ont un nombre d’avis négatifs bien supérieur 
#à la majorité des autres d’où cet écart entre les valeurs. Cela pourrait refléter des différences significatives 
#dans la popularité ou la satisfaction des joueurs selon les jeux.

boxplot(cleaned_games$Negative, 
        main = "Distribution du nombre d'avis négatifs", 
        ylab = "Nombre d'avis négatifs")

#Tout comme la distribution du nombre d’avis négatifs,le nombre d’avis positifs varie considérablement, avec des valeurs allant de 0 à plusieurs millions. 
#Certains jeux ont un nombre d’avis positifs bien supérieur à la majorité des autres d’où cet écart entre les valeurs. 
#Cela pourrait refléter des différences significatives dans la popularité ou la satisfaction des joueurs selon les jeux.

boxplot(cleaned_games$Positive, 
        main = "Distribution du nombre d'avis positifs", 
        ylab = "Nombre d'avis positifs")

#Dans ce boxplot qui illustre la distribution des prix des jeux en dollars on observe que la majorité des jeux ont un prix situé dans une fourchette 
#relativement basse, et proches les uns des autres. La médiane, représentée par la ligne à l’intérieur de la boite, est proche 
#de la partie inférieure de l’échelle des prix, ce qui indique que plus de la moitié des jeux ont un prix relativement bas.

boxplot(cleaned_games$Price, 
        main = "Distribution du prix des jeux", 
        ylab = "Prix en dollars")

#Ce boxplot met en évidence une distribution asymétrique des maximums de joueurs simultanés, avec une concentration de jeux ayant des pics modérés
#et quelques jeux ayant des pics extrêmement élevés. La plupart des pics de connexion se produisent dans une plage d’horaire spécifique. 
#Les points extrêmes représentent des moments de la journée où les pics de connexion sont inhabituels.

boxplot(cleaned_games$Peak.CCU, 
        main = "Distribution des maximums de joueurs simultanés atteint", 
        ylab = "Nombre de joueurs simultanés")

#required.age
# Définir les catégories d'âge avec ifelse
cleaned_games$age_Category <- ifelse(cleaned_games$Required.age < 12 | cleaned_games$Required.age == "", "Tout public",
                                     ifelse(cleaned_games$Required.age >= 12 & cleaned_games$Required.age < 16, "+12",
                                            ifelse(cleaned_games$Required.age <= 16 & cleaned_games$Required.age < 18, "+16", "+18")))
#barplot
#Les jeux qui sont sans limite d'âge requis dominent largement le marché avec une offre importante de près de 8000 types de jeux. 
#En revanche, les jeux réservés aux plus de 18 ans sont nettement moins nombreux tout comme ceux destinés aux joueurs de 12 ans et ceux de 16 ans.
#Ces différences marquées pourraient refléter les préférences des développeurs et des consommateurs, ainsi que les contraintes liées aux réglementations.  

bar_plot_age <- ggplot(cleaned_games, aes(x = age_Category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Fréquence des catégories d'âge",
       x = "Âge requis",
       y = "Nombre de jeux") +
  theme_minimal()
print(bar_plot_age)

# estimated owner
#On observe que la majorité des jeux ont un nombre estimé de propriétaires relativement faible, avec un pic dans les catégories 20k-50k, 50k-100k,
#100k-200k et 200k-500k. Cela suggère que la plupart des jeux ont une audience modeste. 
#Plus le nombre de propriétaires augmente, moins il y a de nombre de jeux. Cela reflète probablement la nature compétitive de l’industrie du jeu vidéo,
#où seulement quelques jeux captent l’attention de la majorité des joueurs.

# sort categories and give them new names
new_labels <- c("0 - 20000" = "0-20k",
                "20000 - 50000" = "20k-50k",
                "50000 - 100000" = "50k-100k",
                "100000 - 200000" = "100k-200k",
                "200000 - 500000" = "200k-500k",
                "500000 - 1000000" = "500k-1M",
                "1000000 - 2000000" = "1M-2M",
                "2000000 - 5000000" = "2M-5M",
                "5000000 - 10000000" = "5M-10M",
                "10000000 - 20000000" = "10M-20M",
                "20000000 - 50000000" = "20M-50M",
                "50000000 - 100000000" = "50M-100M",
                "100000000 - 200000000" = "100M-200M")
# Rename factor levels
cleaned_games$Estimated.owners2 <- factor(cleaned_games$Estimated.owners, 
                                         levels = names(new_labels),  # Keep correct order
                                         labels = new_labels)  # Apply new labels

# Print new levels to verify
print(levels(cleaned_games$Estimated.owners2))
# time to plot
bar_plot_owner <- ggplot(cleaned_games, aes(x = Estimated.owners2)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Estimated Owners",
       x = "Estimated Owners",
       y = "Number of Games") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
print(bar_plot_owner)
