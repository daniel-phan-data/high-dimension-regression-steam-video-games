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

##cleaning using steam_clean.R ----
source("steam_clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)

##univariate tests ----

#densité

#variable a expliqué
ggplot(cleaned_games, aes(x = Average.playtime.forever)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Average Playtime)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# L'on s'intéresse tout d'abord à la répartion des valeurs pour notre variable endogene : Average_playtime_forver
#La distribution du temps de jeu moyen révèle une forte concentration autour d’une valeur centrale, 
#la majorité des jeux captivent les joueurs pour une durée relativement similaire,
#l'on peut noter qu'une minorité se distingue par un temps de jeu plus élevé créant une traîne étendue vers les valeurs les plus élevées


## variables exogenes ##

#Peak.CCU density log10
ggplot(cleaned_games, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Peak CCU (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#une majorité de jeux ont une faible base de joueurs actifs en même temps,
#tandis qu'une minorité monopolise une grande partie du marché.

#L'échelle logarithmique est nécessaire pour mieux visualiser les différences entre les petits et grands jeux,
#sans quoi la concentration sur les faibles valeurs écraserait l’ensemble des données.


#required.age
# Définir les catégories d'âge avec ifelse
cleaned_games$age_Category <- ifelse(cleaned_games$Required.age < 12 | cleaned_games$Required.age == "", "Tout public",
                                     ifelse(cleaned_games$Required.age >= 12 & cleaned_games$Required.age < 16, "+12",
                                            ifelse(cleaned_games$Required.age <= 16 & cleaned_games$Required.age < 18, "+16", "+18")))

#barplot
bar_plot_age <- ggplot(cleaned_games, aes(x = age_Category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Fréquence des catégories d'âge",
       x = "Âge requis",
       y = "Nombre de jeux") +
  theme_minimal()

print(bar_plot_age)

# Boxplot
box_plot_age <- ggplot(cleaned_games, aes(x = age_Category, y = Average.playtime.forever)) +
  geom_boxplot(fill = "lightcoral") +
  labs(title = "Relation entre l'âge requis et le temps de jeu moyen",
       x = "Âge requis",
       y = "Temps de jeu moyen") +
  theme_minimal()

print(box_plot_age)

#"Tout public" est la catégorie de loin la plus représentée et également celle qui contient le plus d'outliers ,
#avec des temps de jeu très élevés dépassant 150 000 minutes.

# les jeux disposant d'un age requis  affichent peu de temps de jeu exceptionnellement élevés,
#ce qui pourrait indiquer qu'ils sont soit moins populaires

# hyptoheses : les joueurs adultes ont moins de temps pour jouer.

#price density


ggplot(cleaned_games, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal() +
  ggtitle("Density Plot of Price (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#Une grande partie des prix est concentrée dans les faibles valeurs.
#La densité diminue rapidement pour les valeurs plus élevées,
#suggérant une asymétrie à droite .

#L'utilisation de l'échelle log10 ici permet de mieux visualiser une distribution fortement asymétrique
#et d'atténuer l'effet des valeurs extrêmes. 

#Positive density log10 

ggplot(cleaned_games, aes(x = Positive + 1)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of positive reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))


#Negative density log10

ggplot(cleaned_games, aes(x = Negative + 1)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of negative reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

# Pour ces 2 variables l'on obtient 
#Distribution centrée et en forme de cloche
# la majorité des produits ont quelques centaines à quelques milliers d’avis positifs.

# idem pour l'utilisation du log10, Si on utilisait une échelle linéaire,
#la plupart des produits avec peu d’avis seraient difficiles à différencier,
#tandis que ceux avec beaucoup d’avis domineraient l’échelle.

#Recommendations density log10

ggplot(cleaned_games, aes(x = Recommendations + 1)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of recommendations (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#on distingue un premier pic
#Il y a un grand nombre de produits avec très peu de recommandations (1 à 10).
#Une majorité des produits semble avoir un nombre de recommandations situé autour de 1000 à 10 000.
#Après ce pic, la densité décroît lentement, suggérant que certains produits ont beaucoup plus de recommandations,
#mais ils sont rares.


##normality + spearman (bivariate tests) ----
#NOTE: need to replicate following tests on the other variables

#normality test

#log10 transformation first
cleaned_games$log_Peak_CCU <- log10(cleaned_games$Peak.CCU + 1)
cleaned_games$log_Average_playtime <- log10(cleaned_games$Average.playtime.forever + 1)
#normality test on transformed values
lillie.test(cleaned_games$log_Peak_CCU)
lillie.test(cleaned_games$log_Average_playtime)  # Y

# Aucune des deux variables  ne suit une distribution normale, malgré le log

#bivariate tests : Spearman test, trying different graphs


#relation bivariés entre la variable endogene et les Variables exogenes 
#scatter plot with LOESS smoothing
ggplot(cleaned_games, aes(x = Average.playtime.forever, y = Positive)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  scale_x_log10() +
  scale_y_log10() + 
  labs(title = "Spearman Correlation: Log-Scaled Playtime vs. Positive Ratings",
       x = "Log10(Average Playtime Forever)",
       y = "Log10(Positive Ratings)") +
  theme_minimal()

#Les jeux avec un bon engagement des joueurs (temps de jeu élevé) reçoivent plus d’évaluations positives.
#Il y a une saturation pour les jeux à très haut engagement,
#où l’augmentation du temps de jeu ne se traduit plus forcément par plus d’avis

#hexbin plot
ggplot(cleaned_games, aes(x = Average.playtime.forever, y = Peak.CCU)) +
  geom_hex(alpha = 0.6) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Spearman Correlation: Log-Scaled Playtime vs. Peak CCU",
       x = "Log10(Average Playtime Forever)",
       y = "Log10(Peak CCU)") +
  theme_minimal() +
  scale_fill_viridis_c()

#2D density plot with contours
ggplot(cleaned_games, aes(x = Average.playtime.forever, y = Peak.CCU)) +
  geom_density_2d(aes(color = ..level..), size = 1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Spearman Correlation: Log-Scaled Playtime vs. Peak CCU",
       x = "Log10(Average Playtime Forever)",
       y = "Log10(Peak CCU)") +
  theme_minimal() +
  scale_color_viridis_c()

#corrélation positive, les jeux ayant un temps de jeu moyen plus élevé
#ont tendance à avoir un plus grand pic de joueurs simultanés
#cependant pas mal de variabilité
#beaucoup de jeux ont un faible temps de jeu moyen et un pic de joueurs relativement bas


##correlation matrix next ----

#keep numerical variables
numeric_vars <- cleaned_games[, sapply(cleaned_games, is.numeric)]
names(numeric_vars)
numeric_vars <- numeric_vars[1:7] # delete log variable
names(numeric_vars)

#correlation matrix
cor_matrix <- cor(numeric_vars, method = "spearman", use = "pairwise.complete.obs")
print(cor_matrix)
#illustration
x11()
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), tl.col = "black")
#heatmap
x11()
ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE)

#strong correlations : Positive - Recommendations (0.83) / Negative - Positive (0.80) 
#Moderate correlations: Peak.CCU - Recommendations (0.59) / Negative - Recommendations (0.67) / Peak.CCU - Positive (0.66)
#Weaker correlations: Price - Average.playtime.forever (0.30) / Price - Positive (0.27)

