## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)

# Sélection des variables et nettoyage des colonnes
gamesc <- games %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)

# Recode de la variable 'Estimated.owners' pour correspondre au script Python
gamesc$Estimated.owners <- as.factor(gamesc$Estimated.owners)
gamesc$Estimated.owners <- 
    fct_recode(gamesc$Estimated.owners,
               "0-20k" = "0 - 20000",
               "20k-50k" = "20000 - 50000",
               "50k-100k" = "50000 - 100000",
               "100k-200k" = "100000 - 200000",
               "200k-500k" = "200000 - 500000",
               "500k-1M" = "500000 - 1000000",
               "1M-2M" = "1000000 - 2000000",
               "2M-5M" = "2000000 - 5000000",
               "5M-10M" = "5000000 - 10000000",
               "10M-20M" = "10000000 - 20000000",
               "20M-50M" = "20000000 - 50000000",
               "50M-100M" = "50000000 - 100000000",
               "100M-200M" = "100000000 - 200000000"
    )

# Fixer la classe de référence comme dans Python (0-20k)
gamesc$Estimated.owners <- relevel(gamesc$Estimated.owners, ref = "0-20k")

# Transformation log1p pour les variables continues (comme dans Python)
gamesc$Average.playtime.forever <- log1p(gamesc$Average.playtime.forever)
gamesc$Peak.CCU <- log1p(gamesc$Peak.CCU)
gamesc$Positive <- log1p(gamesc$Positive)
gamesc$Negative <- log1p(gamesc$Negative)
gamesc$Recommendations <- log1p(gamesc$Recommendations)
gamesc$Price <- log1p(gamesc$Price)

# Standardisation des variables continues
X <- c("Average.playtime.forever", "Peak.CCU", "Positive", "Negative", 
       "Recommendations", "Price", "Required.age")

# Standardisation (équivalent de StandardScaler en Python)
gamesc_scaled <- as.data.frame(scale(gamesc[, X]))
gamesc_scaled$Estimated.owners <- gamesc$Estimated.owners

# Créer un modèle multinomial
library(nnet)
modele_logit <- multinom(Estimated.owners ~ ., data = gamesc_scaled)

# 3. Test de significativité des coefficients (z, p-value)
z <- summary(modele_logit)$coefficients / summary(modele_logit)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))

# Affichage lisible des p-values
cat("\n--- P-values des coefficients ---\n")
print(round(p_values, 4))

# 5. AIC du modèle
cat("\n--- AIC du modèle ---\n")
print(AIC(modele_logit))

# 6. Pseudo R² (comme R² en régression linéaire)
cat("\n--- Pseudo R² ---\n")
print(pR2(modele_logit))

# 7. VIF pour détecter la multicolinéarité
# On doit repasser par une régression linéaire avec les mêmes variables
# (car car::vif() ne marche pas sur multinom())
mod_lineaire_temp <- lm(
    as.numeric(as.factor(Estimated.owners)) ~ Peak.CCU + Positive + Negative + Recommendations + Price + Required.age,
    data = gamesc_scaled
)
cat("\n--- VIF (multicolinéarité) ---\n")
print(vif(mod_lineaire_temp))

# 8. Qualité de prédiction : Matrice de confusion
# pas tres visible
pred <- predict(modele_logit)
cat("\n--- Matrice de confusion ---\n")
print(table(Predicted = pred, Actual = gamesc_scaled$Estimated.owners))

# 9. Taux de bonne classification
accuracy <- mean(pred == gamesc_scaled$Estimated.owners)
cat("\n--- Taux de bonnes prédictions ---\n")
print(round(accuracy, 4))
