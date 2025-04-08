library("tidyverse")
library("DataExplorer")
library(dplyr)

filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)
#filepath <- here("steam_data", "games.csv")  # perle's import
#games <- read.csv(filepath)

##cleaning using steam_clean.R ----
source("steam_clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)

## Peak.CCU density log10 ----

#La majorité des jeux ont un nombre de joueurs simultanés relativement faible, 
#ce qui est typique pour des jeux moins populaire avec une concentration autour de 1e+01 à 1e+03. 
#Une petite proportion de jeux a atteint des pics très élevés ( jusqu’à 100000 joueurs simultanés),
#ce qui correspond généralement à des jeux très populaires.

ggplot(cleaned_games, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Peak CCU (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))
  

## price density ----
#La distribution ici est fortement asymétrique, avec une concentration des prix à gauche et une queue longue à droite.
#Cela montre que la majorité des jeux ont un des prix relativement bas, ce qui pourrait être cohérent avec 
#la popularité de certains jeux ou les promotions sur des plateformes, mais qu’il existe quelques jeux avec des prix beaucoup 
#élevés qui pourraient être des prix au moment de leur sortie ou des éditions limitées. 

ggplot(cleaned_games, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal() +
  ggtitle("Density Plot of Price (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

##Positive density log10 ----
#La distribution du nombre d’avis positifs montre que la plupart des jeux ont un nombre relativement faible d’avis positifs,
#avec une concentration autour de 1e+01 et 1e+03. Cependant, il existe une queue longue qui s’étend jusqu'à 100000 avis positifs,
#indiquant la présence de quelques jeux avec un nombre extrêmement élevés d’avis positifs. 

ggplot(cleaned_games, aes(x = Positive)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of positive reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Negative density log10----
#La distribution d’avis négatifs montre que la plupart des jeux ont un nombre relativement faible d’avis négatifs,
#avec une concentration autour de 1e+01 et 1e+03. Cependant la courbe s’étend jusqu’à 100000 avis négatifs.
#Ce qui laisse penser que la majorité des jeux est bien reçue de la part des joueurs. 

ggplot(cleaned_games, aes(x = Negative)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of negative reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Recommendations density log10 ----

#Le graphique montre une distribution fortement asymétrique, avec un pic autour de 100 recommandations (1e+02) et 
#la courbe qui s’étend jusqu’a 1e+06 de recommandations. Cela indique que la majorité des jeux reçoivent peu de 
#recommandations, tandis qu'une petite minorité est très populaire. Sur les plateformes comme steam, les recommandations
#sont probablement basées sur les avis positifs laissés par les joueurs.


ggplot(cleaned_games, aes(x = Recommendations)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of recommendations (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Average playtime forever----

#L’échelle logarithmique sur l’axe des abscisses (log10) permet de visualiser des données qui couvrent une large plage de valeurs.
# La courbe de densité montre que le pic se situe probablement autour de 1e+01 à 1e+03, ce qui suggère que la plupart des jeux 
#ont un temps de jeu moyen dans cette plage. Cependant il existe quelques jeux avec des temps extrêmement longs comme le montre la courbe qui s’étend jusqu’a 1e+05


ggplot(cleaned_games, aes(x = Average.playtime.forever)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of Average playtime forever (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))