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

## Peak.CCU density log10 ----
ggplot(cleaned_games, aes(x = Peak.CCU)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  scale_x_log10() + 
  theme_minimal() +
  ggtitle("Density Plot of Peak CCU (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## price density ----
ggplot(cleaned_games, aes(x = Price)) + 
  geom_density(fill = "blue", alpha = 0.3) + 
  theme_minimal() +
  ggtitle("Density Plot of Price (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

##Positive density log10 ----
ggplot(cleaned_games, aes(x = Positive)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of positive reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Negative density log10----
ggplot(cleaned_games, aes(x = Negative)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of negative reviews (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Recommendations density log10 ----
ggplot(cleaned_games, aes(x = Recommendations)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of number of recommendations (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))

## Average playtime forever----
ggplot(cleaned_games, aes(x = Average.playtime.forever)) +
  geom_density(fill = "blue", alpha = 0.3) +
  scale_x_log10() +
  theme_minimal() +
  ggtitle("Density Plot of Average playtime forever (log10 scale)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14))