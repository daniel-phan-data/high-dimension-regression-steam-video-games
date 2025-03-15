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

boxplot(cleaned_games$Average.playtime.forever, 
        main = "Distribution du temps de jeu moyen", 
        ylab = "Temps de jeu moyen (minutes)")

boxplot(cleaned_games$Recommendations, 
        main = "Distribution du nombre de recommendations",
        ylab = "Nombre de recommendations")

boxplot(cleaned_games$Negative, 
        main = "Distribution du nombre d'avis négatifs", 
        ylab = "Nombre d'avis négatifs")

boxplot(cleaned_games$Positive, 
        main = "Distribution du nombre d'avis positifs", 
        ylab = "Nombre d'avis positifs")

boxplot(cleaned_games$Price, 
        main = "Distribution du prix des jeux", 
        ylab = "Prix en dollars")

boxplot(cleaned_games$Peak.CCU, 
        main = "Distribution des maximums de joueurs simultanés atteint", 
        ylab = "Nombre de joueurs simultanés")

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

# estimated owner
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
