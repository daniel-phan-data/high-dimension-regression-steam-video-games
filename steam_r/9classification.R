## IMPORTS ----
rm(list = ls()) #clean environment
graphics.off() #clean plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory
temp_env <- new.env() #temporary environment to avoid uneccessary variables after import
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env) #delete temporary environment after data has been loaded

#select variables for analysis
gamesc <- games %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)

# setting class reference to 0-20k
gamesc$Estimated.owners <- relevel(gamesc$Estimated.owners, ref = "0-20k")

# log transformation
gamesc$Average.playtime.forever <- log1p(gamesc$Average.playtime.forever)
gamesc$Peak.CCU <- log1p(gamesc$Peak.CCU)
gamesc$Positive <- log1p(gamesc$Positive)
gamesc$Negative <- log1p(gamesc$Negative)
gamesc$Recommendations <- log1p(gamesc$Recommendations)
gamesc$Price <- log1p(gamesc$Price)

# standardisation
X <- c("Average.playtime.forever", "Peak.CCU", "Positive", "Negative", 
       "Recommendations", "Price", "Required.age")
gamesc_scaled <- as.data.frame(scale(gamesc[, X]))
gamesc_scaled$Estimated.owners <- gamesc$Estimated.owners

# create logit model
model_logit <- multinom(Estimated.owners ~ ., data = gamesc_scaled)

# testing coefficient significance
z <- summary(model_logit)$coefficients / summary(model_logit)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
cat("\n--- P-values des coefficients ---\n")
print(round(p_values, 4))

# AIC of model
cat("\n--- AIC du modèle ---\n")
print(AIC(model_logit))

# pseudo R²
cat("\n--- Pseudo R² ---\n")
print(pR2(model_logit))

# VIF for multicolinearity
mod_lineaire_temp <- lm(
    as.numeric(as.factor(Estimated.owners)) ~ Peak.CCU + Positive + Negative + Recommendations + Price + Required.age,
    data = gamesc_scaled
)
cat("\n--- VIF (multicolinéarité) ---\n")
print(vif(mod_lineaire_temp))

# prediction quality, confusion matrix
pred <- predict(model_logit)
cat("\n--- Matrice de confusion ---\n")
print(table(Predicted = pred, Actual = gamesc_scaled$Estimated.owners))

# accuracy calculation
accuracy <- mean(pred == gamesc_scaled$Estimated.owners)
cat("\n--- Taux de bonnes prédictions ---\n")
print(round(accuracy, 4))

