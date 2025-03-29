
rm(list = ls())

library(ISLR)

str(Auto) #structure du jeu de données "Auto"
?Auto
n <- nrow(Auto) #nombre d'observations
n
View(Auto)

#### Régression de `mpg` en fonction de `horsepower`, et estimation de l'erreur théorique de prévision par VC ####
#Estimation de l'erreur théorique de prévision, par 3 méthodes différentes de VC, de trois modèles différents : 
# modele1 : linéaire simple;
# modele2 :  polynomiale de degré 2; 
# modele3 : polynomiale de degré 3. 

#### Méthode 1 : estimation de l'erreur de prévision par l'approche de l'ensemble de validation ####
indices <- sample(x=n, size=trunc((2/3)*n), replace=FALSE) #vecteur d'indices, de taille `partie entière de (2/3)*n', 
#les indices sont tirés de manière aléatoire sans remise dans l'ensemble {1,2, ...,n} 
indices

#on choisit ici de prendre 2/3 *n observations pour l'apprentissage, et le reste pour le test (ou la validation)
#1
ensemble_apprentissage <- Auto[indices, ] #la partie de la base de données `Auto` qui va servir pour l'apprentissage
str(ensemble_apprentissage)

ensemble_validation <- Auto[ - indices, ] #la base de validation
str(ensemble_validation)

#2
modele1 <- glm(formula = mpg ~ horsepower, data = ensemble_apprentissage) 
modele1

#estimation de l'erreur du modele1
#3
valeurs_predites <- predict(object = modele1, newdata = ensemble_validation) 
str(valeurs_predites)
valeurs_predites

#4
estimation_erreur_modele1 <- mean((ensemble_validation$mpg - valeurs_predites)^2)  
estimation_erreur_modele1

# 5
indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
ensemble_apprentissage <- Auto[indices, ]
ensemble_validation <- Auto[ - indices, ]
modele1 <- lm(formula = mpg ~ horsepower, data = ensemble_apprentissage)
valeurs_predites <- predict(object = modele1, newdata = ensemble_validation)
estimation_erreur_modele1 <- mean((ensemble_validation$mpg - valeurs_predites)^2)  
estimation_erreur_modele1

EAR_modele1 <- mean(abs(ensemble_validation$mpg - valeurs_predites))/mean(abs(ensemble_validation$mpg))*100
EAR_modele1

##### comment améliorer la méthode ####
M <- 1500
erreur_modele1 = NULL
for (i in 1:M)
{
    indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
    ensemble_apprentissage <- Auto[indices, ]
    ensemble_validation <- Auto[ - indices, ]
    modele1 <- lm(formula = mpg ~ horsepower, data = ensemble_apprentissage)
    valeurs_predites <- predict(object = modele1, newdata = ensemble_validation)
    erreur_modele1[i] <- mean((ensemble_validation$mpg - valeurs_predites)^2)
}
Err = NULL
for (m in 1:M)
{Err[m] = mean(erreur_modele1[1:m])
}
plot(Err, type = 'l')
Erreur_prevision_modele1 = Err[M] #meilleur estimation de l'erreur de prévision du modele1
Erreur_prevision_modele1
##############################################################################################

#6
modele2 <- glm(formula = mpg ~ horsepower+I(horsepower^2), data = ensemble_apprentissage) #modele polynomial de degré 2
modele2

modele3 <- glm(formula = mpg ~ horsepower+I(horsepower^2)+I(horsepower^3), data = ensemble_apprentissage) #modele polynomial de degré 3
modele3

#7
#estimation de l'erreur du modele2
valeurs_predites <- predict(object = modele2, newdata = ensemble_validation) 
estimation_erreur_modele2 <- mean((ensemble_validation$mpg - valeurs_predites)^2)  
estimation_erreur_modele2

#estimation de l'erreur du modele3
valeurs_predites <- predict(object = modele3, newdata = ensemble_validation) 
estimation_erreur_modele3 <- mean((ensemble_validation$mpg - valeurs_predites)^2)  
estimation_erreur_modele3

print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 


##### comment améliorer la méthode   ####
# On répète la méthode M fois, et on prend les moyennes des estimations
M <- 1000
erreur_modele1 = NULL
erreur_modele2 = NULL
erreur_modele3 = NULL
for (i in 1:M)
{
    indices <- sample(x = n, size = trunc((2/3)*n), replace = FALSE)
    ensemble_apprentissage <- Auto[indices, ]
    ensemble_validation <- Auto[ - indices, ]
    modele1 <- lm(formula = mpg ~ horsepower, data = ensemble_apprentissage)
    modele2 <- lm(formula = mpg ~ horsepower+I(horsepower^2), data = ensemble_apprentissage) 
    modele3 <- lm(formula = mpg ~ horsepower+I(horsepower^2)+I(horsepower^3), data = ensemble_apprentissage)
    valeurs_predites1 <- predict(object = modele1, newdata = ensemble_validation)
    erreur_modele1[i] <- mean((ensemble_validation$mpg - valeurs_predites1)^2)
    valeurs_predites2 <- predict(object = modele2, newdata = ensemble_validation)
    erreur_modele2[i] <- mean((ensemble_validation$mpg - valeurs_predites2)^2)
    valeurs_predites3 <- predict(object = modele3, newdata = ensemble_validation)
    erreur_modele3[i] <- mean((ensemble_validation$mpg - valeurs_predites3)^2)
}
Err1 = NULL
Err2 = NULL
Err3 = NULL
for (m in 1:M)
{Err1[m] = mean(erreur_modele1[1:m])
Err2[m] = mean(erreur_modele2[1:m])
Err3[m] = mean(erreur_modele3[1:m])
}
ecart_y <- range(Err1,Err2,Err3)
plot(Err1, ylim=ecart_y, type = 'l')
lines(Err2, type = 'l', col="red")
lines(Err3, type = 'l', col="blue")
Erreur_prevision_modele1 = Err1[M] 
Erreur_prevision_modele2 = Err2[M]
Erreur_prevision_modele3 = Err3[M]
print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(Erreur_prevision_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(Erreur_prevision_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(Erreur_prevision_modele3)))) 

#### Méthode 2 : estimation de l'erreur par leave-one-out VC (LOOCV), i.e., la K-fold CV avec K = n, le nombre d'observations ####
# 1
modele1 <- glm(formula = mpg ~ horsepower, data = Auto)
modele1

# 2
library(boot) #on charge le package `boot` pour pouvoir utiliser la fonction cv.glm()
estimation_erreur_modele1 <- cv.glm(data = Auto, glmfit = modele1, K = n)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele1

# 3
estimation_erreur_modele1 <- cv.glm(data = Auto, glmfit = modele1, K = n)$delta[1] #estimation de l'erreur du modele1 par la méthode LOOCV
estimation_erreur_modele1

# 4
modele2 <- glm(formula = mpg ~ horsepower+I(horsepower^2), data = Auto)
modele2

modele3 <- glm(formula = mpg ~ horsepower+I(horsepower^2)+I(horsepower^3), data = Auto)
modele3

# 5
estimation_erreur_modele2 <- cv.glm(data = Auto, glmfit = modele2, K = n)$delta[1] #estimation de l'erreur du modele2 par la méthode LOOCV

estimation_erreur_modele3 <- cv.glm(data = Auto, glmfit = modele3, K = n)$delta[1] #estimation de l'erreur du modele3 par la méthode LOOCV

# 6
print(c("Résultats des estimations par LOOCV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 

#### Méthode 3 : estimation de l'erreur des trois modèles précédents par K-fold CV, avec K = 10 ####
# 1
estimation_erreur_modele1 <- cv.glm(data = Auto, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode  K-fold CV
estimation_erreur_modele2 <- cv.glm(data = Auto, glmfit =  modele2, K = 10)$delta[1] #estimation de l'erreur du modele2 par la méthode K-fold CV
estimation_erreur_modele3 <- cv.glm(data = Auto, glmfit = modele3, K = 10)$delta[1] #estimation de l'erreur du modele3 par la méthode K-fold CV

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 

# 2
estimation_erreur_modele1 <- cv.glm(data = Auto, glmfit = modele1, K = 10)$delta[1] #estimation de l'erreur du modele1 par la méthode  K-fold CV
estimation_erreur_modele2 <- cv.glm(data = Auto, glmfit =  modele2, K = 10)$delta[1] #estimation de l'erreur du modele2 par la méthode K-fold CV
estimation_erreur_modele3 <- cv.glm(data = Auto, glmfit = modele3, K = 10)$delta[1] #estimation de l'erreur du modele3 par la méthode K-fold CV

print(c("Résultats des estimations par 10-fold CV : ",
        paste("Estimation de l'erreur du modele1 = ", as.character(estimation_erreur_modele1)),
        paste("Estimation de l'erreur du modele2 = ", as.character(estimation_erreur_modele2)),
        paste("Estimation de l'erreur du modele3 = ", as.character(estimation_erreur_modele3)))) 


#### Bootstrap ####
f_estimateurs_w <- function(data, index){return(coef(lm(formula = mpg ~ horsepower, data = data, 
                                                        subset = index)))}
f_estimateurs_w(data = Auto, index = 1:392)

f_estimateurs_w(data = Auto, index = sample(x=392, size=392, replace = TRUE))

f_estimateurs_w(data = Auto, index = sample(392, 392, replace = TRUE))

boot(data = Auto, statistic = f_estimateurs_w, R = 2000)

#comparaison avec les estimations des écarts-type données par la lm() 
modele <- lm(formula = mpg ~ horsepower, data = Auto)
summary(modele)



