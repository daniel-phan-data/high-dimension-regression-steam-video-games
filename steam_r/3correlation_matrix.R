## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)

cleaned_games <- games %>% select(Average.playtime.forever, Estimated.owners,
                           Peak.CCU,Price, Recommendations,
                           Required.age, Positive, Negative)

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

x11()
ggcorrplot(cor_matrix, method = "square", type = "lower", lab = TRUE)


#strong correlations : Positive - Recommendations (0.83) / Negative - Positive (0.80) 
#Moderate correlations: Peak.CCU - Recommendations (0.59) / Negative - Recommendations (0.67) / Peak.CCU - Positive (0.66)
#Weaker correlations: Price - Average.playtime.forever (0.30) / Price - Positive (0.27)



## dani ----
# keep numerical variables

numeric_vars <- cleaned_games[, sapply(cleaned_games, is.numeric)]

# Create the correlation matrix
correlation_matrix <- cor(numeric_vars)
corrplot(correlation_matrix, method = "circle")

# Plot the correlation matrix without the mirror image
corrplot(correlation_matrix, method = "circle", type = "upper")

