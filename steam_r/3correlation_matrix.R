##library and data import ----
library("tidyverse")
library("DataExplorer")
library(dplyr)
library(here)
library(ggplot2)
library(nortest)
library(corrplot)

filepath <- "../steam_data/games.csv"
games <- read.csv(filepath)
source("clean.R")  # Source the cleaning file
cleaned_games <- clean_games(games)

####correlation matrix ####

# keep numerical variables
numeric_vars <- cleaned_games[, sapply(cleaned_games, is.numeric)]

#correlation matrix
cor_matrix <- cor(numeric_vars, method = "spearman", use = "pairwise.complete.obs")
print(cor_matrix)

#illustration


corrplot(cor_matrix, method = "color", type = "upper", order = "hclust",
         col = colorRampPalette(c("blue", "white", "red"))(200), tl.col = "black")

#heatmap
library(ggcorrplot)
x11()
ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE)


#strong correlations : Positive - Recommendations (0.83) / Negative - Positive (0.80) 
#Moderate correlations: Peak.CCU - Recommendations (0.59) / Negative - Recommendations (0.67) / Peak.CCU - Positive (0.66)
#Weaker correlations: Price - Average.playtime.forever (0.30) / Price - Positive (0.27)