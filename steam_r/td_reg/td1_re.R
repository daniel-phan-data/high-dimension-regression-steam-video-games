
# Modèle de régression linéaire multiple
# Tests d'hypothèses

# Vider la mémoire
rm(list = ls())

# Installer le package "ISLR"
# et charger le package "ISLR"
library(ISLR)

# Charger le package "lmtest"
library(lmtest)

# Afficher la structure du jeu de données "Carseats"
str(Carseats)
View(Carseats)
? Carseats

## Exercice 
#1
modele.RLM <- lm(formula = Sales ~ ., data = Carseats)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
sort(summary(modele.RLM)$coefficients[,"Pr(>|t|)"])

#3.i
les_residus <- modele.RLM$residuals
acf(modele.RLM$residuals)

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)

#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

#4
# Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
dwtest(modele.RLM, alternative = c("two.sided"))  
# on obtient P_value = 0.8982, on accepte donc H_0, i.e., les erreurs sont non corrélées

#5
# Test d'homoscedasticité de Breusch-Pagan
bptest(modele.RLM, studentize = FALSE) 
#on obtient P_value = 0.7737, on accepte donc H_0, 
#Le terme d'erreur est homoscedasticique

#6
#### vérifier la normalité des erreurs ####
# Graphiquement : normal Q-Q plot, et histogramme versus densité normale
# Normal Q-Q plot
plot(modele.RLM, 2)
# Histogramme versus densité normale
residus <- modele.RLM$residuals
residus
hist(residus, freq = FALSE, ylim = c(0,0.48), 
     main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE)

#7
# Test de Shapiro-Wilk pour tester l'hypothèse de normalité du terme d'erreur
shapiro.test(residuals(modele.RLM)) 
# on obtient une P_value = 0.8337, on accepte donc H_0, i.e., la loi du terme d'erreur est normale 

#8
modele.RLM <- lm(formula = Sales ~ ., data = Carseats)
summary(modele.RLM)
# Ordonner les variables explicatives numériques selon les valeurs 
# des p_values croissantes (du test de Student)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]

# On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 

# Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 

#9
# Ordonner les variables explicatives numériques/qualitatives 
# selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] # Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher) # Attention pour les variables explicatives qualitatives ayant
# plus de deux modalités (on compare à dimension égale)
# dans ce cas il faudrait faire comme suit
XX <- model.matrix(Sales ~., data = Carseats)[,-1] # Cette fonction construit la matrice de design en remplaçant 
# chacune des variables qualitatives par les indicatrices 
# de ses modalités (la première modalité est supprimée)
# on supprime la première colonne correspondant à l'intercept

View(XX)
Carseats.num.data <- cbind(Sales = Carseats[,"Sales"],XX)
Carseats.num.data <- as.data.frame(Carseats.num.data) # Bd constituée que de variables numériques
View(Carseats.num.data)
tests.Fisher2 <- anova(lm(Sales~., data = Carseats.num.data))
tests.Fisher2
m <- nrow(tests.Fisher2)
vect.pvalues.Fisher2 <- tests.Fisher2[1:m-1,"Pr(>F)"] # Extrait le vecteur des p_values
names(vect.pvalues.Fisher2) <- rownames(tests.Fisher2[1:m-1,])
sort(vect.pvalues.Fisher2)

#10
# Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher2)
sort(vect.pvalues.Student) # On n'obtient pas le même classement; 

#11
# il est recommandé de retenir celui de Fisher

