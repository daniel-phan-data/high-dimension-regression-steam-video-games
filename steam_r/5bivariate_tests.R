## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

## normality tests for quantitative variables (bivar tests) ----
#normality test
# Sélectionner les colonnes a tester pour la normalite
names(gamesc)
numeric_cols <- gamesc %>%
  select(Average.playtime.forever, Peak.CCU, Price, Recommendations, Required.age,
         Positive, Negative, total_reviews, positive_ratio)

# Appliquer log10 (+1 pour éviter log10(0))
numeric_cols_log <- numeric_cols %>%
  mutate(across(everything(), ~ log10(. + 1)))

# Appliquer le test de Lilliefors et extraire les p-values
lillie_results <- lapply(numeric_cols_log, lillie.test)

# Créer un tableau avec les noms des variables et leurs p-values
lillie_table <- data.frame(
  "p-value lillie tests" = sapply(lillie_results, function(x) x$p.value)
)
print(lillie_table)

## spearman because there is no normality ----
names(gamesc)
numeric_cols <- gamesc %>%
  select(Average.playtime.forever, Peak.CCU, Price, Recommendations, Required.age,
         Positive, Negative, total_reviews, positive_ratio)
names(numeric_cols)

# Effectuer le test de Spearman pour toutes les combinaisons avec "Average.playtime.forever"
spearman_results <- list()
rho_values <- list()  # Liste pour stocker les valeurs de rho

for (var in names(numeric_cols)[-1]) {  # Exclure "Average.playtime.forever" de la liste
  test_result <- suppressWarnings(cor.test(numeric_cols$Average.playtime.forever, numeric_cols[[var]], method = "spearman"))
  
  # Stocker les p-values et rho dans les listes
  spearman_results[[var]] <- test_result$p.value  # Stocker les p-values
  rho_values[[var]] <- test_result$estimate  # Stocker les valeurs de rho
}

# Créer un tableau avec les résultats
spearman_table <- data.frame(
  "p-value spearman" = unlist(spearman_results),
  "rho (Spearman)" = unlist(rho_values)
)

# Afficher les résultats
print(spearman_table)


## kruskal wallis because no normality and data is not paired ----
names(gamesc)
# Définir la variable y a expliquer
y <- gamesc$Average.playtime.forever

# Sélectionner les variables qualitatives
quali <- gamesc %>%
  select(Estimated.owners, rating)

# Créer une liste vide pour stocker les résultats
kruskal_table <- data.frame(
  Test = character(),
  H_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# Appliquer le test Kruskal-Wallis sur chaque variable qualitative
for (var in names(quali)) {
  test_result <- kruskal.test(y ~ gamesc[[var]], data = gamesc)
  
  # Ajouter les résultats au tableau
  kruskal_table <- rbind(kruskal_table, data.frame(
    Test = var,  # Remplacer par le nom de la variable quali
    H_statistic = test_result$statistic,
    p_value = test_result$p.value
  ))
}

# Afficher le tableau complet avec la colonne Test
print(kruskal_table)

## Afficher tout les resultats des tests bivaries----
print(lillie_table)
print(spearman_table)
print(kruskal_table)
