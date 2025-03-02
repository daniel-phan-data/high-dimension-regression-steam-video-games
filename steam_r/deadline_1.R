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

#Peak.CCU density log10
ggplot(cleaned_games, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Peak CCU (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#required age histogram

x <- cleaned_games$Required.age
hist(x,breaks=15,col="blue",
     xlab="Age restriction",
     ylab="Frequency",
     main="Age restriction distribution",tck=0.01, freq=FALSE)
box()
densite <- density(x)
lines(densite, col = "purple",lwd=3) # superposing a density curve

#price density

ggplot(cleaned_games, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal() +
  ggtitle("Density Plot of Price (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

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

#Recommendations density log10

ggplot(cleaned_games, aes(x = Recommendations + 1)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of recommendations (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

#Average playtime forever

ggplot(cleaned_games, aes(x = Average.playtime.forever)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of Average playtime forever (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

##normality + spearman (bivariate tests) ----
#NOTE: need to replicate following tests on the other variables

#normality test

#log10 transformation first
cleaned_games$log_Peak_CCU <- log10(cleaned_games$Peak.CCU + 1)
cleaned_games$log_Average_playtime <- log10(cleaned_games$Average.playtime.forever + 1)
#normality test on transformed values
lillie.test(cleaned_games$log_Peak_CCU)
lillie.test(cleaned_games$log_Average_playtime)  # Y

#bivariate tests : Spearman test, trying different graphs

#scatter plot with LOESS smoothing
ggplot(cleaned_games, aes(x = Average.playtime.forever, y = Peak.CCU)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "loess", color = "red") +
  scale_x_log10() +
  scale_y_log10() + 
  labs(title = "Spearman Correlation: Log-Scaled Playtime vs. Positive Ratings",
       x = "Log10(Average Playtime Forever)",
       y = "Log10(Positive Ratings)") +
  theme_minimal()

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

##correlation matrix next ----

#keep numerical variables
numeric_vars <- cleaned_games[, sapply(cleaned_games, is.numeric)]
#correlation matrix
cor_matrix <- cor(numeric_vars, method = "spearman", use = "pairwise.complete.obs")
print(cor_matrix)
#illustration
corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), tl.col = "black")
#heatmap
x11()
ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE)

#strong correlations : Positive - Recommendations (0.83) / Negative - Positive (0.80) 
#Moderate correlations: Peak.CCU - Recommendations (0.59) / Negative - Recommendations (0.67) / Peak.CCU - Positive (0.66)
#Weaker correlations: Price - Average.playtime.forever (0.30) / Price - Positive (0.27)

