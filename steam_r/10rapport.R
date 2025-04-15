## IMPORTS ----
rm(list = ls())
graphics.off()
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env)
gamesc <- games %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)

summary(gamesc)

# Function to create a linear model
create_lm <- function(dataset, Y, X, categories) {
    if (length(categories) == 0) {
        formula <- as.formula(paste(Y, "~", paste(X, collapse = "+")))
    } else {
        formula <- as.formula(paste(Y, "~", paste(c(X, categories), collapse = "+")))
    }
    model <- lm(formula = formula, data = dataset)
    return(model)
}

# Function to graph and test model hypotheses
check_lm_hypotheses <- function(model, data) {
    cat("Vérification des hypothèses pour le modèle :", deparse(model$call), "\n\n")
    
    # hypothese 1: relation de linearite entre Y et X
    # le nuage de points doit etre centre autour de 0 sans motif evident
    # sinon la relation n est pas bien modelise, non-linearite possible
    plot(model, which = 1, main = "1. Résidus vs valeurs ajustées")
    
    # hypothese 2: homoscedasticite des erreurs
    # ecart type constant peu importe la valeur ajustee
    # entonnoir = heteroscedasticite
    plot(model, which = 3, main = "2.1. Écarts à l'effet de levier")
    
    # residus absolus vs valeurs ajustees
    # si c'est croissant/decroissant, la variance n est pas constante
    data$residu_abs <- abs(residuals(model))
    data$ajuste <- fitted(model)
    ggplot(data, aes(x = ajuste, y = residu_abs)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "loess", col = "red") +
        labs(title = "2.2. Hétéroscédasticité : résidus absolus vs ajustés",
             x = "Valeurs ajustées", y = "Résidus absolus") +
        theme_minimal() -> p1
    print(p1)
    
    # hypothese 3: independance des erreurs
    # test de Durbin-Watson : attend une valeur proche de 2
    # si proche de 0 : autocorrelation positive. si > 2.5 : autocorrelation negative   
    cat("\nTest de Durbin-Watson (attendu ≈ 2) :\n")
    print(dwtest(model))
    dwtest(model, alternative = c("two.sided"))
    # ACF : si les barres depassent, il y a autocorrelation des erreurs
    acf(residuals(model), main = "3. ACF des résidus")
    
    # hypothese 4: normalite des erreurs
    # ca doit suivre la ligne, sinon residus anormaux
    plot(model, which = 2, main = "4. QQ-plot des résidus")
    residus <- model$residuals
    #boxplot des residus
    boxplot(residus, main = "Boxplot des résidus")
    #histogramme "naif"
    hist(residus[residus < quantile(residus, 1)], breaks = 50,
         main = "Histogramme des résidus",
         xlab = "Résidus", col = "green", border = "green")
    #histogramme plus lisible en retirant le top 1% qui casse tout
    hist(residus[residus < quantile(residus, 0.99)], breaks = 50,
         main = "Histogramme des résidus (sans top 1%)",
         xlab = "Résidus (censurés à 99%)", col = "blue", border = "blue")
    
    # hypothese 5 multicolinearite
    # VIF (Variance Inflation Factor) : mesure le lien entre les variables explicatives
    # VIF > 5 = multicolinearite moderee VIF > 10 = multicolinearite severe
    cat("\nVIF (Variance Inflation Factor) :\n")
    vif_vals <- vif(model)
    print(vif_vals)
    cat("\nVariables avec VIF > 5 :\n")
    print(names(vif_vals[vif_vals > 5]))
    
    # observations influentes: distances de Cook
    # indique si certaines observations influencent beaucoup le modele
    # attention aux points au dessus de la ligne rouge
    cooks <- cooks.distance(model)
    seuil <- 4 / nrow(data)
    cat("\nObservations influentes (Cook > 4/n) :\n")
    # influents <- which(cooks > seuil)
    # print(influents)
    
    plot(cooks, type = "h",
         main = "6. Distance de Cook avec seuil 4/n",
         ylab = "Distance de Cook", xlab = "Index de l'observation")
    abline(h = seuil, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = paste0("Seuil = 4/n ≈ ", round(seuil, 5)),
           col = "red", lty = 2, lwd = 2)
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

# Fonction pour détecter les points trop influents
detect_cook <- function(model, threshold = 4 / nrow(model$model)) {
    cooks <- cooks.distance(model)
    which(cooks > threshold)
}

# Fonction pour détecter les résidus trop gros
detect_large_residuals <- function(model, threshold = 3) {
    rstudent_res <- rstudent(model)
    which(abs(rstudent_res) > threshold)
}

# Détection des outliers dans les données (z-score > 3)
detect_outliers_data <- function(dataset, threshold = 3) {
    numeric_data <- dataset[sapply(dataset, is.numeric)]
    z_scores <- scale(numeric_data)
    which(apply(abs(z_scores) > threshold, 1, any))
}

# Nettoyage global
clean_model <- function(model, dataset) {
    idx_cook <- detect_cook(model)
    idx_resid <- detect_large_residuals(model)
    idx_outliers <- detect_outliers_data(dataset)
    idx_to_remove <- unique(c(idx_cook, idx_resid, idx_outliers))
    cleaned_data <- dataset[-idx_to_remove, ]
    return(list(data = cleaned_data, removed = idx_to_remove))
}

## 1 first simple model ----
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
categories <- c("Estimated.owners")

model <- create_lm(gamesc, Y, X, categories)
summary(model)
check_lm_hypotheses(model, gamesc)

## 2 model with log transformation to be closer to linearity ----
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations",
       "Price", "Required.age")
categories <- c("Estimated.owners")
variables_to_transform <- c("Average.playtime.forever","Peak.CCU",
                            "Positive", "Negative", "Recommendations", "Price")
# log tranformations
gamesc_log <- apply_transformations(gamesc, variables_to_transform)
model_log <- create_lm(gamesc_log, Y, X, categories)

summary(model_log)
check_lm_hypotheses(model_log, gamesc_log)


## third model without outliers, high influence point, and extreme errors ----
cleaning <- clean_model(model_log, gamesc_log)
gamesc_log_clean <- cleaning$data
model_log_clean <- create_lm(gamesc_log_clean, Y, X, categories)

summary(model_log_clean)
# cat("Nombre de points retirés :", length(cleaning$removed), "\n")

## fourth model with only ubisoft games ----

## testing selection algorithms with ubisoft games ----

## classification to predict estimated owners ----

## polynomial models ----