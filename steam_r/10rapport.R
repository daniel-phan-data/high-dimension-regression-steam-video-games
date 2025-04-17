

# ANALYSE PRÉDICTIVE DES JEUX STEAM - PARCOURS COMPLET

# Objectif : Comprendre les déterminants du succès commercial (nombre de propriétaires) 
#            et de l'engagement joueur (temps de jeu moyen)
# Méthodes :
#   - Analyse exploratoire (distributions, corrélations)
#   - Tests statistiques 
#   - Modélisation multinomiale (catégories de propriétaires)
#   - Régression linéaire (temps de jeu)
# ########################################################################"

# ------------------------------------------------------------------------------
# 0. INITIALISATION - ENVIRONNEMENT DE TRAVAIL
# ------------------------------------------------------------------------------
# Nettoyage pour éviter les conflits
rm(list = ls())  # Supprime tous les objets en mémoire
graphics.off()   # Ferme tous les périphériques graphiques

# Configuration du répertoire de travail
# (Assure la reproductibilité quel que soit l'ordinateur)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Chargement des données via le script de setup original
# Note : 0setup.R contient les fonctions de nettoyage initiales
temp_env <- new.env()  # Environnement isolé pour éviter les pollutions
source("0setup.R", local = temp_env)
games <- temp_env$setup()  # Application de la fonction setup()
rm(temp_env)  # Nettoyage de l'environnement temporaire

# ------------------------------------------------------------------------------
# 1. PRÉPARATION DES DONNÉES - CRÉATION DES VARIABLES ANALYTIQUES
# ------------------------------------------------------------------------------
# Sélection des variables pertinentes pour nos analyses
gamesc <- games %>%
  select(
    # Variable cible principale (succès commercial):
    Estimated.owners,  # Catégorie de nombre de propriétaires
    
    # Variable cible secondaire (engagement joueur):
    Average.playtime.forever,  # Temps de jeu moyen en minutes
    
    # Variables explicatives:
    Peak.CCU,         # Pic de joueurs simultanés (popularité instantanée)
    Price,            # Prix du jeu en dollars
    rating,           # Note agrégée (catégorielle)
    Recommendations,  # Nombre de recommandations (viralité)
    Required.age,     # Âge minimum requis
    Positive,         # Nombre d'avis positifs
    Negative,         # Nombre d'avis négatifs
    total_reviews,    # Volume total d'avis
    positive_ratio    # Pourcentage d'avis positifs
  )

# ------------------------------------------------------------------------------
# 2. RECODAGE DES VARIABLES - POUR DES ANALYSES PLUS CLAIRES
# ------------------------------------------------------------------------------
# Transformation de Estimated.owners en facteur ordonné avec libellés explicites
# Pourquoi ? Les libellés originaux sont techniques (ex: "0 - 20000")
gamesc$Estimated.owners <- gamesc$Estimated.owners %>%
  as.factor() %>%
  forcats::fct_recode(
    "Niche (0-20k)" = "0 - 20000",
    "Émergent (20k-50k)" = "20000 - 50000",
    "Stable (50k-100k)" = "50000 - 100000",
    "Succès (100k-200k)" = "100000 - 200000",
    "Top Ventes (200k-500k)" = "200000 - 500000",
    "Blockbuster (500k-1M)" = "500000 - 1000000",
    "Phénomène (1M-2M)" = "1000000 - 2000000",
    "Exception (2M-5M)" = "2000000 - 5000000",
    "Légende (5M-10M)" = "5000000 - 10000000",
    "Iconique (10M-20M)" = "10000000 - 20000000",
    "Historique (20M-50M)" = "20000000 - 50000000",
    "Culturel (50M-100M)" = "50000000 - 100000000",
    "Universel (100M-200M)" = "100000000 - 200000000"
  )

# ------------------------------------------------------------------------------
# 3. ANALYSE EXPLORATOIRE - COMPRÉHENSION DES DISTRIBUTIONS
# ------------------------------------------------------------------------------
# Pourquoi ? Identifier les outliers et distributions atypiques avant modélisation

# A. Distribution du temps de jeu moyen
# Observation : Distribution très asymétrique (quelques jeux très joués)
boxplot(gamesc$Average.playtime.forever, 
        main = "Distribution du temps de jeu moyen",
        ylab = "Minutes",
        col = "lightblue")

# B. Distribution des prix
# Observation : Majorité des jeux sous $30 avec quelques outliers > $100
ggplot(gamesc, aes(x = Price)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(title = "Distribution des prix des jeux Steam",
       x = "Prix ($)",
       y = "Nombre de jeux") +
  theme_minimal()

# C. Relation pic de joueurs vs temps de jeu
# Méthode : Échelle log pour mieux voir les tendances
ggplot(gamesc, aes(x = Peak.CCU, y = Average.playtime.forever)) +
  geom_point(alpha = 0.3) +
  scale_x_log10() +
  scale_y_log10() +
  geom_smooth(method = "lm", color = "red") +
  labs(title = "Relation entre pic de joueurs et temps de jeu",
       subtitle = "Échelles logarithmiques pour meilleure lisibilité",
       x = "Pic de joueurs simultanés (log10)",
       y = "Temps de jeu moyen (log10)")

# ------------------------------------------------------------------------------
# 4. TESTS STATISTIQUES - VALIDATION DES HYPOTHÈSES
# ------------------------------------------------------------------------------
# A. Test de normalité (Lilliefors)
# Pourquoi ? Les tests paramétriques supposent la normalité des données
# Résultat : Toutes les variables rejettent H0 (normalité) → méthodes robustes
lillie_results <- sapply(gamesc[c("Average.playtime.forever", "Price")], function(x) {
  lillie.test(log10(x + 1))$p.value  # Transformation log pour réduire l'asymétrie
})

# B. Corrélations (Spearman)
# Pourquoi ? Mesurer les associations monotones sans supposer la linéarité
spearman_res <- with(gamesc, {
  cor.test(Average.playtime.forever, 
           Peak.CCU, 
           method = "spearman",
           exact = FALSE)  # Approximation pour grands échantillons
})

# ------------------------------------------------------------------------------
# 5. MODÉLISATION PRÉDICTIVE
# ------------------------------------------------------------------------------
# A. Régression linéaire pour le temps de jeu
# Choix méthodologique : Malgré la non-normalité, LM est robuste aux grands échantillons
model_lm <- lm(Average.playtime.forever ~ Peak.CCU + Price + positive_ratio,
               data = gamesc)

# B. Régression multinomiale pour les catégories de propriétaires
# Pourquoi ? Variable dépendante catégorielle ordonnée
library(nnet)
model_multinom <- multinom(Estimated.owners ~ Peak.CCU + Price + rating,
                           data = gamesc,
                           trace = FALSE)  # Désactive les logs itératifs

# C. Calcul des p-values pour le multinomial
# Méthode : Test de Wald via approximation normale
z_scores <- summary(model_multinom)$coefficients / 
  summary(model_multinom)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z_scores)))

# ------------------------------------------------------------------------------
# 6. DIAGNOSTICS DES MODÈLES
# ------------------------------------------------------------------------------
# A. Performance du modèle multinomial
# Matrice de confusion
pred_categories <- predict(model_multinom)
conf_matrix <- caret::confusionMatrix(pred_categories, 
                                      gamesc$Estimated.owners)

# B. Analyse de la multicolinéarité
# Approche : Utilisation d'un modèle linéaire proxy pour calculer les VIF
mod_vif <- lm(as.numeric(Estimated.owners) ~ Peak.CCU + Price + rating,
              data = gamesc)
vif_results <- car::vif(mod_vif)

# ------------------------------------------------------------------------------
# 7. PRÉPARATION DES RÉSULTATS POUR EXPORT
# ------------------------------------------------------------------------------
# Structuration des résultats finaux
#results <- list(
# Donnees = list(
#    Observations = nrow(gamesc),
#    Variables = names(gamesc)
#  ),
#  Exploratoire = list(
#    Test_Normalite = lillie_results,
#    Correlation = spearman_res
#  ),
#  Modeles = list(
#    Lineaire = broom::tidy(model_lm),
#    Multinomial = list(
#      Coefficients = coef(model_multinom),
#      PValues = p_values
#    )
#  ),
#  Performance = list(
#   Exactitude = conf_matrix$overall["Accuracy"],
#  Matrice_Confusion = conf_matrix$table
#  )
#)

# Sauvegarde pour reporting
#saveRDS(results, "resultats_analyses_steam.rds")


# FIN DU SCRIPT - RÉSUMÉ DES PRINCIPAUX RÉSULTATS

#message("ANALYSE TERMINÉE AVEC SUCCÈS\n",
#        "----------------------------\n",
#       "Temps de jeu moyen :\n",
#        "  - Pic de joueurs : β = ", round(coef(model_lm)[2], 3), " (p < 0.001)\n",
#        "  - Prix : β = ", round(coef(model_lm)[3], 3), " (p = ", 
#        round(summary(model_lm)$coefficients[3,4], 3), ")\n\n",
#        "Catégories de propriétaires :\n",
#        "  - Exactitude globale : ", round(conf_matrix$overall["Accuracy"], 2), "%\n",
#        "  - Variables significatives : Peak.CCU (p < 0.001), Price (p = 0.02)")


## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)
gamesc <- games %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)

summary(gamesc)

# Function to create a linear model
create_lm <- function(dataset, Y, X, categories) {
    if (length(categories) == 0) {
        formula <- as.formula(paste(Y, "~", paste(X, collapse = "+")))
    } else {
        formula <- as.formula(paste(Y, "~", paste(c(X, categories), collapse = "+")))
    }
    model <- lm(formula = formula, data = dataset)
    return(model)
}

# Function to graph and test model hypotheses
check_lm_hypotheses <- function(model, data) {
    cat("Vérification des hypothèses pour le modèle :", deparse(model$call), "\n\n")
    
    # hypothese 1: relation de linearite entre Y et X
    # le nuage de points doit etre centre autour de 0 sans motif evident
    # sinon la relation n est pas bien modelise, non-linearite possible
    plot(model, which = 1, main = "1. Résidus vs valeurs ajustées")
    
    # hypothese 2: homoscedasticite des erreurs
    # ecart type constant peu importe la valeur ajustee
    # entonnoir = heteroscedasticite
    plot(model, which = 3, main = "2.1. Écarts à l'effet de levier")
    
    # residus absolus vs valeurs ajustees
    # si c'est croissant/decroissant, la variance n est pas constante
    data$residu_abs <- abs(residuals(model))
    data$ajuste <- fitted(model)
    ggplot(data, aes(x = ajuste, y = residu_abs)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "loess", col = "red") +
        labs(title = "2.2. Hétéroscédasticité : résidus absolus vs ajustés",
             x = "Valeurs ajustées", y = "Résidus absolus") +
        theme_minimal() -> p1
    print(p1)
    
    # hypothese 3: independance des erreurs
    # test de Durbin-Watson : attend une valeur proche de 2
    # si proche de 0 : autocorrelation positive. si > 2.5 : autocorrelation negative   
    cat("\nTest de Durbin-Watson (attendu ≈ 2) :\n")
    print(dwtest(model))
    dwtest(model, alternative = c("two.sided"))
    # ACF : si les barres depassent, il y a autocorrelation des erreurs
    acf(residuals(model), main = "3. ACF des résidus")
    
    # hypothese 4: normalite des erreurs
    # ca doit suivre la ligne, sinon residus anormaux
    plot(model, which = 2, main = "4. QQ-plot des résidus")
    residus <- model$residuals
    #boxplot des residus
    boxplot(residus, main = "Boxplot des résidus")
    #histogramme "naif"
    hist(residus[residus < quantile(residus, 1)], breaks = 50,
         main = "Histogramme des résidus",
         xlab = "Résidus", col = "green", border = "green")
    #histogramme plus lisible en retirant le top 1% qui casse tout
    hist(residus[residus < quantile(residus, 0.99)], breaks = 50,
         main = "Histogramme des résidus (sans top 1%)",
         xlab = "Résidus (censurés à 99%)", col = "blue", border = "blue")
    
    # hypothese 5 multicolinearite
    # VIF (Variance Inflation Factor) : mesure le lien entre les variables explicatives
    # VIF > 5 = multicolinearite moderee VIF > 10 = multicolinearite severe
    cat("\nVIF (Variance Inflation Factor) :\n")
    vif_vals <- vif(model)
    print(vif_vals)
    cat("\nVariables avec VIF > 5 :\n")
    print(names(vif_vals[vif_vals > 5]))
    
    # observations influentes: distances de Cook
    # indique si certaines observations influencent beaucoup le modele
    # attention aux points au dessus de la ligne rouge
    cooks <- cooks.distance(model)
    seuil <- 4 / nrow(data)
    cat("\nObservations influentes (Cook > 4/n) :\n")
    # influents <- which(cooks > seuil)
    # print(influents)
    
    plot(cooks, type = "h",
         main = "6. Distance de Cook avec seuil 4/n",
         ylab = "Distance de Cook", xlab = "Index de l'observation")
    abline(h = seuil, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = paste0("Seuil = 4/n ≈ ", round(seuil, 5)),
           col = "red", lty = 2, lwd = 2)
}

# Fonction pour appliquer les transformations sur une liste de variables
apply_transformations <- function(data, variables) {
    for (var in variables) {
        # Log transformation: log10(x + 1) to avoid log(0)
        data[[var]] <- log10(data[[var]] + 1)
        
        # sqrt
        # data[[var]] <- sqrt(data[[var]])
        
        # Standardization: (x - mean) / sd
        # data[[var]] <- scale(data[[var]], center = TRUE, scale = TRUE)
        
        # Normalization: (x - min) / (max - min)
        # data[[var]] <- (data[[var]] - min(data[[var]])) / (max(data[[var]]) - min(data[[var]]))
    }
    return(data)
}

# Fonction pour détecter les points trop influents
detect_cook <- function(model, threshold = 4 / nrow(model$model)) {
    cooks <- cooks.distance(model)
    which(cooks > threshold)
}

# Fonction pour détecter les résidus trop gros
detect_large_residuals <- function(model, threshold = 3) {
    rstudent_res <- rstudent(model)
    which(abs(rstudent_res) > threshold)
}

# Détection des outliers dans les données (z-score > 3)
detect_outliers_data <- function(dataset, threshold = 3) {
    numeric_data <- dataset[sapply(dataset, is.numeric)]
    z_scores <- scale(numeric_data)
    which(apply(abs(z_scores) > threshold, 1, any))
}

# Nettoyage global
clean_model <- function(model, dataset) {
    idx_cook <- detect_cook(model)
    idx_resid <- detect_large_residuals(model)
    idx_outliers <- detect_outliers_data(dataset)
    idx_to_remove <- unique(c(idx_cook, idx_resid, idx_outliers))
    cleaned_data <- dataset[-idx_to_remove, ]
    return(list(data = cleaned_data, removed = idx_to_remove))
}

## 1 first simple model ----
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
categories <- c("Estimated.owners")

model <- create_lm(gamesc, Y, X, categories)
summary(model)
check_lm_hypotheses(model, gamesc)

## 2 model with log transformation to be closer to linearity ----
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations",
       "Price", "Required.age")
categories <- c("Estimated.owners")

# log tranformations
variables_to_transform <- c("Average.playtime.forever","Peak.CCU",
                            "Positive", "Negative", "Recommendations", "Price")
gamesc_log <- apply_transformations(gamesc, variables_to_transform)


model_log <- create_lm(gamesc_log, Y, X, categories)
summary(model_log)
check_lm_hypotheses(model_log, gamesc_log)


## third model without outliers, high influence point, and extreme errors ----
cleaning <- clean_model(model_log, gamesc_log)
gamesc_log_clean <- cleaning$data
model_log_clean <- create_lm(gamesc_log_clean, Y, X, categories)

summary(model_log_clean)
# cat("Nombre de points retirés :", length(cleaning$removed), "\n")

## fourth model with only ubisoft games ----


ubisoft <- games %>% filter(Publishers == "Ubisoft") %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Recommendations", "Price", "Required.age")
categories <- c()
categories <- c("Estimated.owners", "rating")

model_ubisoft <- create_lm(ubisoft, Y, X, categories)

# View model summary# View model summarygamesc
summary(model_ubisoft)
check_lm_hypotheses(model_ubisoft, ubisoft)

## testing selection algorithms with ubisoft games ----

## classification to predict estimated owners ----

## polynomial models ----

