## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

gamesc <- gamesc %>% select(-Name)
modele.RLM <- lm(formula = Average.playtime.forever ~ ., data = gamesc)
summary(modele.RLM)
sort(summary(modele.RLM)$coefficients[,"Pr(>|t|)"])

#hétéroscédasticité, ce qui signifie que le modèle ne capture pas bien la structure des erreurs.

XX <- model.matrix(Average.playtime.forever ~ ., data = gamesc) #Matrice de design
View(XX)
p <- ncol(XX)-1 #Nombre de variables numériques explicatives dans le modèle de RLM complet
p
require(leaps)  
require(glmulti)
select.modele.aic <- glmulti(Average.playtime.forever ~ ., data = gamesc, level = 1, 
                             fitfunction = lm, crit = "aic", plotty = FALSE, method = "h")
modele.opt.aic <- summary(select.modele.aic)$bestmodel
modele.opt.aic
summary(lm(modele.opt.aic, data = gamesc))
anova(lm(modele.opt.aic, data = gamesc))
#best model AIC 

select.modele.bic <- glmulti(Average.playtime.forever ~ ., data = gamesc, level = 1,
                             fitfunction = lm, crit = "bic", plotty = FALSE, method = "h")
modele.opt.bic <- summary(select.modele.bic)$bestmodel
modele.opt.bic
summary(lm(modele.opt.bic, data = gamesc))
anova(lm(modele.opt.bic, data = gamesc))
#Best model BIC : Average.playtime.forever~1+Estimated.owners+Price+Recommendations
#### Exercice 2 ####
#Forward selection, version 1 : fonction regsubsets()
select.mod.for <- regsubsets(Average.playtime.forever ~ ., data = gamesc, 
                             nbest = 1, nvmax = p, method = "forward")
#Backward elimination, version 1 : fonction regsubsets()
select.mod.bac <- regsubsets(Average.playtime.forever ~ ., data = gamesc, 
                             nbest = 1, nvmax = p, method = "backward")
#Résultats pour le critère BIC des deux algorithmes
par(mfrow = c(1,2))
plot(select.mod.for, scale = "bic", main = "Forward selection")
plot(select.mod.bac, scale = "bic", main = "Backward elimination")
#Résultas des deux algorithmes pour le critère Cp de Mallow 
par(mfrow = c(1,2))
plot(select.mod.for, scale = "Cp", main = "Forward selection")
plot(select.mod.bac, scale = "Cp", main = "Backward elimination")
#Résultats des deux algorithmes pour le critère du R2 ajusté 
par(mfrow = c(1,2))
plot(select.mod.for, scale = "adjr2", main = "Forward selection")
plot(select.mod.bac, scale = "adjr2", main = "Backward elimination")

#### Exercice 3 ####
##Forward selection, version 2 : fonction step()
modele.trivial <- lm(Average.playtime.forever ~ 1, data = gamesc)
modele.complet <- lm(Average.playtime.forever ~ ., data = gamesc)
#selon AIC (k = 10)
res.select.AIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = gamesc, direction = "forward", k = 10)
#selon BIC (k = log(n))
n <- nrow(gamesc)
res.select.BIC.for <- step(modele.trivial, 
                           scope = list(lower = modele.trivial, upper = modele.complet),
                           data = gamesc, direction = "forward", k = log(n))
#selon le critère de la statistique de Fisher
n <- nrow(gamesc)
res.select.F.for <- step(modele.trivial, 
                         scope = list(lower = modele.trivial, upper = modele.complet),
                         data = gamesc, direction = "forward", test = "F")

##Backward elimination, version 2 : fonction step()
modele.complet <- lm(Average.playtime.forever ~ ., data = gamesc)
#selon AIC (k = 2)
res.select.AIC.bac <- step(modele.complet, data = Carseats, direction = "backward", k = 10)
#selon BIC (k = log(n))
n <- nrow(gamesc)
res.select.BIC.bac <- step(modele.complet, data = Carseats, direction = "backward", k = log(n))
#selon le critère de Fisher
n <- nrow(gamesc)
res.select.F.bac <- step(modele.complet, data = Carseats, direction = "backward", test = "F")

#### Exercice 4 ####
# Méthode ascendante bidirectionnelle (pour les trois critères AIC, BIC et F)
modele.trivial <- lm(Average.playtime.forever ~ 1, data = gamesc)
modele.complet <- lm(Average.playtime.forever ~ ., data = gamesc)
res.select.AIC.for.both <- step(modele.trivial, scope = list(lower = modele.trivial, 
                                                             upper = modele.complet), 
                                data = gamesc, direction = "both", k = 2)  
res.select.BIC.for.both <- step(modele.trivial, scope = list(lower = modele.trivial, 
                                                             upper = modele.complet), 
                                data = gamesc, direction = "both", k = log(n))  
res.select.F.for.both <- step(modele.trivial, scope = list(lower = modele.trivial, 
                                                           upper = modele.complet), 
                              data = gamesc, direction = "both", test = "F")  


# Méthode descendante bidirectionnelle
modele.complet <- lm(Average.playtime.forever ~ ., data = gamesc)
res.select.AIC.bac.both <- step(modele.complet, data = gamesc, 
                                direction = "both", k = 2)  
res.select.BIC.bac.both <- step(modele.complet, data = gamesc, 
                                direction = "both", k = log(n))  
res.select.F.bac.both <- step(modele.complet, data = gamesc, 
                              direction = "both", test = "F")  

#### Exercice 5 ####
# Sélection par algorithme génétique 
#selon AIC
select.mod.gen <- glmulti(Average.playtime.forever ~ ., data = gamesc.num.data, level = 1, method = "g", 
                          fitfunction = lm, crit = 'aic', plotty = F)
aic.best.model <- summary(select.mod.gen)$bestmodel
aic.best.model

#selon BIC
select.mod.gen <- glmulti(Average.playtime.forever ~ ., data = gamesc.num.data, level = 1, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
bic.best.model <- summary(select.mod.gen)$bestmodel
bic.best.model


#### l'algorithme génétique permet la sélection en un temps raisonnable, même en grande dimension ####
#Sélection par algorithme génétique parmi tous les modèles de RLM + tous les modèles avec interactions
#selon AIC
select.mod.gen <- glmulti(Average.playtime.forever ~ ., data = gamesc.num.data, level = 2, method = "g", 
                          fitfunction = lm, crit = 'aic', plotty = F)
aic.best.model <- summary(select.mod.gen)$bestmodel
aic.best.model

#selon BIC
select.mod.gen <- glmulti(Average.playtime.forever ~ ., data = gamesc.num.data, level = 2, method = "g", 
                          fitfunction = lm, crit = 'bic', plotty = F)
bic.best.model <- summary(select.mod.gen)$bestmodel
bic.best.model

model2 <- lm(bic.best.model, data=gamesc.num.data)
summary(model2)
anova(model2)


#### Exercice 6 ####
# ...

gamesc_opt<- Carseats.num.data[, c("Sales","CompPrice","Income","Advertising","Price","ShelveLocGood",
                                     "ShelveLocMedium","Age")]####cahnger par best model
str(gamesc_opt)

par(mfrow = c(1,1))

#1
modele.RLM <- lm(Average.playtime.forever ~ ., data = gamesc_opt)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

#3.i
acf(modele.RLM$residuals)

#3.ii
#Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
#graphiquement :
plot(modele.RLM, 1)

#3.iii
#Vérifier l'hypothèse d'homoscedasticité des erreurs;
#graphiquement :
plot(modele.RLM, 3)

#4
#Tester la non-corrélation (d'ordre 1) des erreurs : test de Durbin-Watson
require(lmtest)
dwtest(modele.RLM, alternative = c("two.sided"))

#5
#Test d'homoscedasticité de Breusch-Pagan
require(lmtest)
bptest(modele.RLM, studentize = FALSE)

#6
#### vérifier la normalité des erreurs ####
#Graphiquement : normal Q-Q plot, et histogramme versus densité normale
#normal Q-Q plot
plot(modele.RLM, 2)
#histogramme versus densité normale
residus <- modele.RLM$residuals
hist(residus, freq = FALSE, ylim = c(0,0.48), 
     main = "Histogramme des résidus")
curve(dnorm(x, mean = mean(residus), sd = sd(residus)), 
      col = 2, lty = 2, lwd = 2, add = TRUE)
#Test de Shapiro-Wilk pour tester l'hypothèse de normalité du terme d'erreur
shapiro.test(residuals(modele.RLM))

#7
modele.RLM <- lm(formula = Sales ~ ., data = Carseats_opt)
summary(modele.RLM)
#Ordonner les variables explicatives numériques selon les valeurs 
#des p_values croissantes (du test de Student)
vect.pvalues.Student <- summary(modele.RLM)$coefficients[,"Pr(>|t|)"]#On supprime la p_value de l'intercept 
vect.pvalues.Student <-vect.pvalues.Student[2:length(vect.pvalues.Student)] 
#Variables explicatives ordonnées, de la plus significative à la moins significative
sort(vect.pvalues.Student) 

#8
#Ordonner les variables explicatives numériques/qualitatives 
#selon les valeurs des P_values croissantes du test de Fisher
tests.Fisher <- anova(modele.RLM)
tests.Fisher
str(tests.Fisher)
m <- nrow(tests.Fisher)
vect.pvalues.Fisher <- tests.Fisher[1:m-1,"Pr(>F)"] #Extrait le vecteur des p_values
names(vect.pvalues.Fisher) <- rownames(tests.Fisher[1:m-1,])
sort(vect.pvalues.Fisher) #Attention pour les variables explicatives qualitatives ayant

#9
#Comparaison avec le classement selon le test de Student
sort(vect.pvalues.Fisher)
sort(vect.pvalues.Student) #On n'obtient pas le même classement; 
#il est recommandé de retenir celui de Fisher

