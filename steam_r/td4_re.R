
#Vider la mémoire
rm(list = ls())
library(ISLR)
library(boot)
str(Carseats)
?Carseats

#### Exercice 1 ####
#1
XX <- model.matrix(Sales ~., data = Carseats)[,-1] #matrice de design sans l'intercept
View(XX)
require(glmnet)
reg.ridge <- glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 0)
par(mfrow = c(1,2))
plot(reg.ridge, label = TRUE)
plot(reg.ridge, xvar = "lambda", label = TRUE, lwd = 2)

n <- nrow(Carseats)
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 0, 
                         nfolds=n)
bestlam <- reg.cvridge$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvridge)
min(reg.cvridge$cvm) #erreur de prévision du modèle ridge optimal 
coef(reg.cvridge)

#2
#Comparaison des modèles en terme d'erreur (de prévision) estimée (par VC)
#erreur du modèle ridge optimal
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], 
                         alpha = 0, nfolds = n)
erreur.modele.ridge.opt <- min(reg.cvridge$cvm) #erreur de prévision du modèle ridge optimal 
erreur.modele.ridge.opt

#erreur du modèle de RLM complet
require(boot)
erreur.modele.RLM.complet <- cv.glm(data = Carseats, 
                                    glmfit =  glm(formula = Sales ~., 
                                                  data = Carseats), K = n)$delta[1]
erreur.modele.RLM.complet

#erreur du modèle optimal selon le critère BIC
Carseats.num.data <- cbind(Sales = Carseats[,"Sales"],XX)
Carseats.num.data <- as.data.frame(Carseats.num.data) #Bd constituée que de variables numériques
View(Carseats.num.data)
require(glmulti)
select.modele.bic <- glmulti(Sales ~., data = Carseats.num.data, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
erreur.modele.bic.opt <- cv.glm(data = Carseats.num.data, 
                                glmfit =  glm(formula = modele.opt.bic, 
                                              data = Carseats.num.data), K = n)$delta[1]
erreur.modele.bic.opt

#On affiche l'erreur de prévision (estimée) de chacun des modèles
erreur.modele.RLM.complet
erreur.modele.ridge.opt
erreur.modele.bic.opt

#### Exercice 2 ####
#1 le Lasso 
reg.lasso <- glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 1)
par(mfrow = c(1,2))
plot(reg.lasso, label = TRUE)
plot(reg.lasso, xvar = "lambda", label = TRUE, lwd = 2)

reg.cvlasso <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], 
                         nflods = n, alpha = 1)
bestlam <- reg.cvlasso$lambda.min
bestlam
par(mfrow = c(1,1))
plot(reg.cvlasso)
coef(reg.cvlasso)
min(reg.cvlasso$cvm)

#2 Comparaison des modèles en terme d'erreur de prévision
## i 
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], nfolds = n, alpha = 0)
erreur.modele.ridge.opt <- min(reg.cvridge$cvm) #erreur de prévision du modèle ridge optimal 
erreur.modele.ridge.opt

## ii
reg.cvlasso <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], nfolds=n, alpha = 1)
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm) #erreur de prévision du modèle lasso optimal 
erreur.modele.lasso.opt

## iii
erreur.modele.RLM.complet <- cv.glm(data = Carseats, 
                                    glmfit =  glm(formula = Sales ~., 
                                                  data = Carseats), K = n)$delta[1]
erreur.modele.RLM.complet

## iv
erreur.modele.bic.opt <- cv.glm(data = Carseats.num.data, 
                                glmfit =  glm(formula = modele.opt.bic, 
                                              data = Carseats.num.data), K = n)$delta[1]
erreur.modele.bic.opt

#on affiche l'erreur de prévision (estimée) de chacun des modèles
erreur.modele.ridge.opt
erreur.modele.lasso.opt
erreur.modele.RLM.complet
erreur.modele.bic.opt

#### Exercice 3 ####
# 1 le Lasso 
reg.lasso <- glmnet(x = scale(XX), y = Carseats[,"Sales"], alpha = 1)
reg.cvlasso <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], nflods = n, alpha = 1)
coef(reg.cvlasso)
erreur.modele.lasso.opt <- min(reg.cvlasso$cvm)
erreur.modele.lasso.opt

## le group Lasso
## On définit les groupes de variables
groupe <- c(1,2,3,4,5,6,6,7,8,9,10)
require(gglasso)
reg.group.lasso <- gglasso(x = scale(XX), y = Carseats$Sales, group = groupe)
reg.cv.group.lasso <- cv.gglasso(x = scale(XX), y = Carseats$Sales,
                                 nfolds = n, group = groupe)  
coef(reg.cv.group.lasso)
erreur.modele.group.lasso.opt <- min(reg.cv.group.lasso$cvm)
erreur.modele.group.lasso.opt

#2 Comparaison des modèles
reg.cvridge <- cv.glmnet(x = scale(XX), y = Carseats[,"Sales"], nfolds = n, alpha = 0)
erreur.modele.ridge.opt <- min(reg.cvridge$cvm) #erreur de prévision du modèle ridge optimal 
erreur.modele.ridge.opt

erreur.modele.RLM.complet <- cv.glm(data = Carseats, 
                                    glmfit =  glm(formula = Sales ~., 
                                                  data = Carseats), K = n)$delta[1]
erreur.modele.RLM.complet

erreur.modele.bic.opt <- cv.glm(data = Carseats.num.data, 
                                glmfit =  glm(formula = modele.opt.bic, 
                                              data = Carseats.num.data), K = n)$delta[1]
erreur.modele.bic.opt

## on affiche l'erreur de chacun des modèles
erreur.modele.ridge.opt
erreur.modele.lasso.opt
erreur.modele.group.lasso.opt
erreur.modele.RLM.complet
erreur.modele.bic.opt

