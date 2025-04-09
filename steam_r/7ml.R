## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

gamesc <- gamesc %>% select(Average.playtime.forever, Estimated.owners, Peak.CCU,
                            rating, Price, Recommendations, Required.age,
                            Positive, Negative, total_reviews, positive_ratio)

# Function to create a linear model
create_ml <- function(dataset, Y, X, categories) {
    if (length(categories) == 0) {
        formula <- as.formula(paste(Y, "~", paste(X, collapse = "+")))
    } else {
        formula <- as.formula(paste(Y, "~", paste(c(X, categories), collapse = "+")))
    }
    modele.RLM <- lm(formula = formula, data = dataset)
    return(modele.RLM)
}

# Function to create a GLS model
create_gls <- function(dataset, Y, X, categories = c(), correlation_struct = corCompSymm()) {
    # Build the formula for the model
    predictors <- if (length(categories) == 0) X else c(X, categories)
    formula_str <- paste(Y, "~", paste(predictors, collapse = "+"))
    
    # Convert the formula string to a formula object
    formula <- as.formula(formula_str)
    
    # Remove rows with NA to avoid fitting issues
    dataset_clean <- na.omit(dataset)
    
    # Fit GLS model with optional correlation structure
    modele.gls <- gls(formula, 
                      data = dataset_clean, 
                      correlation = correlation_struct)
    
    return(modele.gls)
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

## first model with only numeric variables ----
names(gamesc)
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
# categories <- c("Estimated.owners")
categories <- c()
variables <- c(Y, X, categories)  # Combined variables list
print(variables)
modele.RLM <- create_ml(gamesc, Y, X, categories)

summary(modele.RLM)

#3.i
les_residus <- modele.RLM$residuals
# test autocorrelation des erreur
acf(modele.RLM$residuals)

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)

#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

# premier modele avec que les quantitatives peu concluant, R2 tres faible
# les plots montrent qu ils y a trop de valeurs faibles et aucune linearite




## first model but transforming some of the numeric values ----
names(gamesc)
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations",
       "Price", "Required.age")
categories <- c()
variables <- c(Y, X, categories)  # Combined variables list
print(variables)
variables_to_transform <- c("Average.playtime.forever","Peak.CCU",
                            "Positive", "Negative", "Recommendations", "Price")
#application log
gamesc1 <- apply_transformations(gamesc, variables_to_transform)
modele.RLM <- create_ml(gamesc1, Y, X, categories)

summary(modele.RLM)
#3.i
les_residus <- modele.RLM$residuals
# test autocorrelation des erreur
acf(modele.RLM$residuals)

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)

#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

# plots differents, mais meme probleme de coefficient de determination trop bas
# expliquer l utilite des 3 transformations

## too much error auto correlation, try gls ----

model.GLS <- create_gls(gamesc1, Y, X, categories)
summary(model.GLS)

les_residus <- model.GLS$residuals
# test autocorrelation des erreur
acf(model.GLS$residuals)

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(model.GLS, 1)

#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(model.GLS, 3)


## XXXXX adding qualitative predictors ----
names(gamesc)
Y <- "Average.playtime.forever"
# removed positive and negative because rating was calculated from them
X <- c("Peak.CCU", "Positive", "Negative","Recommendations", "Price", "Required.age")
categories <- c("rating")
variables <- c(Y, X, categories)  # Combined variables list
print(variables)
variables_to_transform <- c("Average.playtime.forever","Peak.CCU", "Recommendations", "Price")
#log, normalisation, standardization
gamesc2 <- apply_transformations(gamesc, variables_to_transform)
modele.RLM <- create_ml(gamesc2, Y, X, categories)

summary(modele.RLM)
#3.i
les_residus <- modele.RLM$residuals
# test autocorrelation des erreur
acf(modele.RLM$residuals)

#3.ii
# Vérifier l'hypothèse de linéarité entre la variable réponse et les variables explicatives;
# graphiquement :
plot(modele.RLM, 1)

#3.iii
# Vérifier l'hypothèse d'homoscedasticité des erreurs;
# graphiquement :
plot(modele.RLM, 3)

# plots differents, mais meme probleme de coefficient de determination trop bas
# expliquer l utilite des 3 transformations
# attentions p values des categories d estimated owners

