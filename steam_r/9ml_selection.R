## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)

gamesc <- games %>% select(Average.playtime.forever, Estimated.owners, Peak.CCU,
                           Price, Recommendations, Required.age,
                           Positive, Negative)

select_model_glmulti <- function(data, criterion = "aic", level = 1) {
    result <- glmulti(Average.playtime.forever ~ ., data = data, level = level,
                      fitfunction = lm, crit = criterion, plotty = FALSE, method = "h")
    best_model_formula <- summary(result)$bestmodel
    best_model <- lm(best_model_formula, data = data)
    return(best_model)
}

run_stepwise <- function(data, direction = "forward", criterion = "aic") {
    modele.trivial <- lm(Average.playtime.forever ~ 1, data = data)
    modele.complet <- lm(Average.playtime.forever ~ ., data = data)
    
    # Définir la pénalité k selon le critère
    k <- switch(criterion,
                "aic" = 2,
                "bic" = log(nrow(data)),
                "F"   = NULL)
    
    # Effectuer la sélection pas à pas en fonction du critère
    if (criterion == "F") {
        result <- step(modele.trivial, scope = list(lower = modele.trivial, upper = modele.complet),
                       data = data, direction = direction, test = "F")
    } else {
        result <- step(modele.trivial, scope = list(lower = modele.trivial, upper = modele.complet),
                       data = data, direction = direction, k = k)
    }
    
    # Retourner le résultat du modèle sélectionné
    return(result)
}

turn_data_to_num <- function(gamesc) {
    XX <- model.matrix(Average.playtime.forever ~ ., data = gamesc)[,-1]
    gamesc_num <- cbind(Avreage.playtime.forever = dataset[,"Average.playtime.forever"], XX) 
    gamesc_num <- as.data.frame(gamesc_num)
    return(gamesc_num)
}


