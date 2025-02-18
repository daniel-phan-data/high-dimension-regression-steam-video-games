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
ggplot(gamesc, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Peak CCU (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## required age histogram ----
x <- gamesc$Required.age
hist(x,breaks=15,col="blue",
     xlab="Age restriction",
     ylab="Frequency",
     main="Age restriction distribution",tck=0.01, freq=FALSE)
box()
# superposing a density curve
densite <- density(x)
lines(densite, col = "purple",lwd=3)

## price density ----
ggplot(gamesc, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal() +
  ggtitle("Density Plot of Price (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

##Positive density log10 ----
ggplot(gamesc, aes(x = Positive)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of positive reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Negative density log10----
ggplot(gamesc, aes(x = Negative)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of negative reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Recommendations density log10 ----
ggplot(gamesc, aes(x = Recommendations)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of recommendations (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Average playtime forever----
ggplot(gamesc, aes(x = Average.playtime.forever)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of Average playtime forever (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

