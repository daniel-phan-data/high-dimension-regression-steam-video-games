
# Installer et charger le package nnet si ce n'est pas encore fait
# install.packages("nnet")  # À décommenter si le package n'est pas installé
library(nnet)

# Exemple de jeu de données (à remplacer par tes propres données)
# Imaginons que dataset soit ta base de données avec les colonnes nécessaires
set.seed(42)  # Pour la reproductibilité
dataset <- data.frame(
    Estimated.owners = factor(sample(c("1000000 - 2000000", "10000000 - 20000000", "20000000 - 50000000", 
                                       "50000000 - 100000000", "2000000 - 5000000", "5000000 - 10000000"), 
                                     100, replace = TRUE)),
    Peak.CCU = rnorm(100, mean = 1000, sd = 300),
    Positive = rnorm(100, mean = 10000, sd = 2000),
    Negative = rnorm(100, mean = 500, sd = 100),
    Recommendations = rnorm(100, mean = 300, sd = 50),
    Price = rnorm(100, mean = 20, sd = 5),
    Required.age = rnorm(100, mean = 25, sd = 3)
)

# Ajuster un modèle multinomial
modele_multinom <- multinom(Estimated.owners ~ Peak.CCU + Positive + Negative + Recommendations + Price + Required.age, data = dataset)

# Résumé du modèle
summary(modele_multinom)

# Prédire la catégorie la plus probable
predictions <- predict(modele_multinom, newdata = dataset)

# Afficher les premières prédictions
head(predictions)

# Prédire les probabilités associées à chaque catégorie
probabilites <- predict(modele_multinom, newdata = dataset, type = "probs")

# Afficher les premières probabilités pour les 5 premières observations
head(probabilites)

# Matrice de confusion pour évaluer la performance du modèle
confusion_matrix <- table(Predicted = predictions, Actual = dataset$Estimated.owners)

# Afficher la matrice de confusion
print(confusion_matrix)

# Calculer l'exactitude globale du modèle
accuracy <- sum(predictions == dataset$Estimated.owners) / nrow(dataset)
cat("L'exactitude du modèle est de : ", accuracy, "\n")
