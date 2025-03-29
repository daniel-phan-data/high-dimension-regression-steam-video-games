## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

#need to remove names for model building
gamesc <- gamesc %>% select(-Name)

#### no transformations ----
names(gamesc)
modele.RLM <- lm(formula = Average.playtime.forever ~ 
                     Peak.CCU,
                 data = gamesc)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
sort(summary(modele.RLM)$coefficients[,"Pr(>|t|)"])

#3.i
les_residus <- modele.RLM$residuals
acf(modele.RLM$residuals)

## 

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)
#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

## too many values stacked on the left, STOP

#### with log10 applied ----
log_gamesc <- gamesc
log_gamesc[, sapply(log_gamesc, is.numeric)] <- log10(1+log_gamesc[,sapply(log_gamesc, is.numeric)])
names(log_gamesc)
modele.RLM <- lm(formula = Average.playtime.forever ~ 
                     Peak.CCU,
                 data = log_gamesc)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
sort(summary(modele.RLM)$coefficients[,"Pr(>|t|)"])

#3.i
les_residus <- modele.RLM$residuals
acf(modele.RLM$residuals)

## auto correlated residuals, STOP
#### without outliers ----
remove_outliers <- function(df) {
    # Sélectionner les colonnes numériques
    numeric_columns <- df[, sapply(df, is.numeric)]
    
    # Identifier et supprimer les outliers pour chaque variable
    for (col in colnames(numeric_columns)) {
        Q1 <- quantile(numeric_columns[[col]], 0.25)
        Q3 <- quantile(numeric_columns[[col]], 0.75)
        IQR_value <- Q3 - Q1
        
        # Définir les limites (inférieure et supérieure)
        lower_limit <- Q1 - 1.5 * IQR_value
        upper_limit <- Q3 + 1.5 * IQR_value
        
        # Supprimer les lignes où la valeur est en dehors de ces limites
        df <- df[df[[col]] >= lower_limit & df[[col]] <= upper_limit, ]
    }
    return(df)
}

# Appliquer la fonction sur ton jeu de données 'gamesc'
no_outliers_gamesc <- remove_outliers(gamesc)

names(no_outliers_gamesc)
modele.RLM <- lm(formula = Average.playtime.forever ~ 
                     Peak.CCU,
                 data = no_outliers_gamesc)
attributes(modele.RLM)
summary(modele.RLM)
summary(modele.RLM)$coefficients

summary(modele.RLM)$coefficients[,"Pr(>|t|)"]
sort(summary(modele.RLM)$coefficients[,"Pr(>|t|)"])

#3.i
les_residus <- modele.RLM$residuals
acf(modele.RLM$residuals)

## 

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)
#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

## values spread too much, STOP
## give up on linearity, let's test polynomials models