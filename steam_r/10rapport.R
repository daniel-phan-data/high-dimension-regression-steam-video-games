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