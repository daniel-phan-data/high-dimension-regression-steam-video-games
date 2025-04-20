## IMPORTS ----
rm(list = ls()) #clean environment
graphics.off() #clean plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory
temp_env <- new.env() #temporary environment to avoid uneccessary variables after import
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env) #delete temporary environment after data has been loaded

# select variables for analysis
cleaned_games <- games %>%
  select(Average.playtime.forever, Estimated.owners,
         Peak.CCU, rating, Price,
         Recommendations, Required.age,
         Positive, Negative,
         total_reviews, positive_ratio)

## average playtime forever
boxplot(cleaned_games$Average.playtime.forever, 
        main = "Distribution du temps de jeu moyen", 
        ylab = "Temps de jeu moyen (minutes)")

## recommendations
boxplot(cleaned_games$Recommendations, 
        main = "Distribution du nombre de recommendations",
        ylab = "Nombre de recommendations")

## negative
boxplot(cleaned_games$Negative, 
        main = "Distribution du nombre d'avis négatifs", 
        ylab = "Nombre d'avis négatifs")

## positive
boxplot(cleaned_games$Positive, 
        main = "Distribution du nombre d'avis positifs", 
        ylab = "Nombre d'avis positifs")

## price
boxplot(cleaned_games$Price, 
        main = "Distribution du prix des jeux", 
        ylab = "Prix en dollars")

## peak ccu
boxplot(cleaned_games$Peak.CCU, 
        main = "Distribution des maximums de joueurs simultanés atteint", 
        ylab = "Nombre de joueurs simultanés")

## required.age
# define age category
cleaned_games$age_Category <- ifelse(cleaned_games$Required.age < 12 | cleaned_games$Required.age == "", "Tout public",
                                     ifelse(cleaned_games$Required.age >= 12 & cleaned_games$Required.age < 16, "+12",
                                            ifelse(cleaned_games$Required.age <= 16 & cleaned_games$Required.age < 18, "+16", "+18")))
# barplot
bar_plot_age <- ggplot(cleaned_games, aes(x = age_Category)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Fréquence des catégories d'âge",
       x = "Âge requis",
       y = "Nombre de jeux") +
  theme_minimal()
print(bar_plot_age)

## estimated owner

bar_plot_owner <- ggplot(cleaned_games, aes(x = Estimated.owners)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribution of Estimated Owners",
       x = "Estimated Owners",
       y = "Number of Games") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability
print(bar_plot_owner)
