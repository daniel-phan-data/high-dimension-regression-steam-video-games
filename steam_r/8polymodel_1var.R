#### replication td3 modeles polynomiaux avec une var explicative

## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

# Define the function ----
validate_model_errors <- function(dataset, response_var, predictor_var, M = 1000) {
    # Preparation steps
    n <- nrow(dataset)
    erreur_modele1 = NULL
    erreur_modele2 = NULL
    erreur_modele3 = NULL
    
    # Loop for M iterations to calculate the error for each model
    for (i in 1:M) {
        indices <- sample(x = n, size = trunc((2/3) * n), replace = FALSE)
        ensemble_apprentissage <- dataset[indices, ]
        ensemble_validation <- dataset[-indices, ]
        
        modele1 <- lm(formula = paste(response_var, "~", predictor_var), data = ensemble_apprentissage)
        modele2 <- lm(formula = paste(response_var, "~", predictor_var, "+ I(", predictor_var, "^2)"), data = ensemble_apprentissage) 
        modele3 <- lm(formula = paste(response_var, "~", predictor_var, "+ I(", predictor_var, "^2) + I(", predictor_var, "^3)"), data = ensemble_apprentissage)
        
        valeurs_predites1 <- predict(object = modele1, newdata = ensemble_validation)
        erreur_modele1[i] <- mean((ensemble_validation[[response_var]] - valeurs_predites1)^2)
        
        valeurs_predites2 <- predict(object = modele2, newdata = ensemble_validation)
        erreur_modele2[i] <- mean((ensemble_validation[[response_var]] - valeurs_predites2)^2)
        
        valeurs_predites3 <- predict(object = modele3, newdata = ensemble_validation)
        erreur_modele3[i] <- mean((ensemble_validation[[response_var]] - valeurs_predites3)^2)
    }
    
    # Compute the mean error over M iterations
    Err1 = NULL
    Err2 = NULL
    Err3 = NULL
    for (m in 1:M) {
        Err1[m] = mean(erreur_modele1[1:m])
        Err2[m] = mean(erreur_modele2[1:m])
        Err3[m] = mean(erreur_modele3[1:m])
    }
    
    # Plot the error curves
    ecart_y <- range(Err1, Err2, Err3)
    plot(Err1, ylim = ecart_y, type = 'l', main = "Model Error Estimations", xlab = "Iterations", ylab = "Mean Squared Error")
    lines(Err2, type = 'l', col = "red")
    lines(Err3, type = 'l', col = "blue")
    
    # Return the final error estimates for each model
    Erreur_prevision_modele1 = Err1[M]
    Erreur_prevision_modele2 = Err2[M]
    Erreur_prevision_modele3 = Err3[M]
    
    print(c("Résultats des estimations par la méthode de l'ensemble de validation : ",
            paste("Estimation de l'erreur du modele1 = ", as.character(Erreur_prevision_modele1)),
            paste("Estimation de l'erreur du modele2 = ", as.character(Erreur_prevision_modele2)),
            paste("Estimation de l'erreur du modele3 = ", as.character(Erreur_prevision_modele3))))
}

# Example usage of the function ----

# Fonction pour appliquer les transformations sur une liste de variables
apply_transformations <- function(data, variables) {
    for (var in variables) {
        # Log transformation: log10(x + 1) to avoid log(0)
        data[[var]] <- log10(data[[var]] + 1)
        
        # Standardization: (x - mean) / sd
        data[[var]] <- scale(data[[var]], center = TRUE, scale = TRUE)
        
        # Normalization: (x - min) / (max - min)
        data[[var]] <- (data[[var]] - min(data[[var]])) / (max(data[[var]]) - min(data[[var]]))
    }
    return(data)
}

variables <- c("Average.playtime.forever", "Peak.CCU")
gamesc_transformed <- apply_transformations(gamesc, variables)

validate_model_errors(gamesc_transformed, "Average.playtime.forever", "Peak.CCU")
