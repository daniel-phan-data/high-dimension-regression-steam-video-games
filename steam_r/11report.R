# this script will generate all the graphs and data in our annex
# starting from our first linear model attempt
# refer to files 1, 2, 3, 5, and 6 for the rest of the annex

## IMPORTS ----
rm(list = ls()) #clean environment
graphics.off() #clean plots
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set working directory
temp_env <- new.env() #temporary environment to avoid uneccessary variables after import
source("0setup.R", local = temp_env)
games <- temp_env$setup()
rm(temp_env) #delete temporary environment after data has been loaded

#select variables for analysis
gamesc <- games %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)

## functions to create models and test hypotheses ----
# function to create a linear model
create_lm <- function(dataset, Y, X, categories) {
    if (length(categories) == 0) {
        formula <- as.formula(paste(Y, "~", paste(X, collapse = "+")))
    } else {
        formula <- as.formula(paste(Y, "~", paste(c(X, categories), collapse = "+")))
    }
    model <- lm(formula = formula, data = dataset)
    return(model)
}

# function to graph and test model hypotheses
check_lm_hypotheses <- function(model, data) {
    cat("Vérification des hypothèses pour le modèle :", deparse(model$call), "\n\n")
    
    # hypothese 1: relation de linearite entre Y et X
    # le nuage de points doit etre centre autour de 0 sans motif evident
    # sinon la relation n est pas bien modelise, non-linearite possible
    plot(model, which = 1, main = "1. Résidus vs valeurs ajustées")
    
    # hypothese 2: homoscedasticite des erreurs
    # on veut un ecart type constant peu importe la valeur ajustee
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
    #histogramme plus lisible en retirant le top 1%
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
    # attention aux lignes au dessus de la ligne rouge
    cooks <- cooks.distance(model)
    seuil <- 4 / nrow(data)
    # cat("\nObservations influentes (Cook > 4/n) :\n")
    # influents <- which(cooks > seuil)
    # print(influents)

    plot(cooks, type = "h",
         main = "6. Distance de Cook avec seuil 4/n",
         ylab = "Distance de Cook", xlab = "Index de l'observation")
    abline(h = seuil, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = paste0("Seuil = 4/n ≈ ", round(seuil, 5)),
           col = "red", lty = 2, lwd = 2)
}

# function to transform data with log10
apply_transformations <- function(data, variables) {
    for (var in variables) {
        # log transformation: log10(x + 1) to avoid log(0)
        data[[var]] <- log10(data[[var]] + 1)
        
    }
    return(data)
}

# function to detect high influence point with Cook's distance
detect_cook <- function(model, threshold = 4 / nrow(model$model)) {
    cooks <- cooks.distance(model)
    which(cooks > threshold)
}

# function to detect abnormally large residuals
detect_large_residuals <- function(model, threshold = 3) {
    rstudent_res <- rstudent(model)
    which(abs(rstudent_res) > threshold)
}

# function to detect outliers
detect_outliers_data <- function(dataset, threshold = 3) {
    numeric_data <- dataset[sapply(dataset, is.numeric)]
    z_scores <- scale(numeric_data)
    which(apply(abs(z_scores) > threshold, 1, any))
}

# remove outliers, high influence observations and extreme residuals
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

# log tranformations
variables_to_transform <- c("Average.playtime.forever","Peak.CCU",
                            "Positive", "Negative", "Recommendations", "Price")
gamesc_log <- apply_transformations(gamesc, variables_to_transform)

model_log <- create_lm(gamesc_log, Y, X, categories)
summary(model_log)
check_lm_hypotheses(model_log, gamesc_log)

## 3 model with log but without outliers, high influence point, and extreme errors ----
cleaning <- clean_model(model_log, gamesc_log)
gamesc_log_clean <- cleaning$data

model_log_clean <- create_lm(gamesc_log_clean, Y, X, categories)
summary(model_log_clean)
# cat("Nombre de points retirés :", length(cleaning$removed), "\n")

## 4 model with only ubisoft games, no transformation ----

ubisoft <- games %>% filter(Publishers == "Ubisoft") %>%
    select(Average.playtime.forever, Estimated.owners,
           Peak.CCU, rating, Price,
           Recommendations, Required.age,
           Positive, Negative,
           total_reviews, positive_ratio)
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Recommendations", "Price", "Required.age")
categories <- c()
categories <- c("Estimated.owners", "rating")

model_ubisoft <- create_lm(ubisoft, Y, X, categories)
summary(model_ubisoft)
check_lm_hypotheses(model_ubisoft, ubisoft)

## functions to test selection algorithms ----

# function that finds the best linear model for given criteria
select_model_glmulti <- function(data, crit = "aic", level = 1) {
    formula <- as.formula("Average.playtime.forever ~ .")
    result <- glmulti(formula, data = data, level = level,
                      fitfunction = "lm", crit = crit,
                      plotty = FALSE, method = "h")
    return(summary(result)$bestmodel)
    
}

# function that runs step by step algorithm for given criteria and direction
run_stepwise <- function(data, direction = "forward", crit = "aic") {
    modele.trivial <- lm(Average.playtime.forever ~ 1, data = data)
    modele.complet <- lm(Average.playtime.forever ~ ., data = data)
    # penality k according to criteria
    k <- switch(crit,
                "aic" = 2,
                "bic" = log(nrow(data)),
                "F"   = NULL)
    # run algorithm
    if (crit == "F") {
        result <- step(modele.trivial, scope = list(lower = modele.trivial, upper = modele.complet),
                       data = data, direction = direction, test = "F")
    } else {
        result <- step(modele.trivial, scope = list(lower = modele.trivial, upper = modele.complet),
                       data = data, direction = direction, k = k)
    }
    return(result)
}

# turn qualitative variables to numerical data, using indicator function
turn_data_to_num <- function(data) {
    y <- data[["Average.playtime.forever"]]
    XX <- model.matrix(~ ., data = data %>% select(-Average.playtime.forever))[, -1]
    colnames(XX) <- make.names(colnames(XX))
    data_num <- as.data.frame(cbind(Average.playtime.forever = y, XX))
    return(data_num)
}

# compare models by displaying key values in a table
compare_models <- function(model_list, model_names = NULL) {
    if (is.null(model_names)) {
        model_names <- paste0("Model_", seq_along(model_list))
    }
    
    results <- lapply(seq_along(model_list), function(i) {
        model <- model_list[[i]]
        name <- model_names[i]
        
        # Extract basic stats
        aic_val <- AIC(model)
        bic_val <- BIC(model)
        adj_r2 <- summary(model)$adj.r.squared
        rse <- sigma(model)  # residual standard error
        
        # Assumptions
        sw_test <- shapiro.test(residuals(model))$p.value
        bp_test <- bptest(model)$p.value
        vif_vals <- tryCatch({
            vif(model)
        }, error = function(e) rep(NA, length(coefficients(model))))
        
        mean_vif <- if (is.numeric(vif_vals)) mean(vif_vals, na.rm = TRUE) else NA
        
        # Return a row
        data.frame(
            Model = name,
            AIC = round(aic_val, 2),
            BIC = round(bic_val, 2),
            Adj_R2 = round(adj_r2, 3),
            RSE = round(rse, 2),
            Shapiro.p = round(sw_test, 4),
            BP.p = round(bp_test, 4),
            Mean_VIF = round(mean_vif, 2),
            stringsAsFactors = FALSE
        )
    })
    
    do.call(rbind, results)
}


## testing algorithms selection on ubisoft games ----

## glmulti

ubisoft_aic <- select_model_glmulti(ubisoft, crit = "aic")
ubisoft_aic <- lm(ubisoft_aic, ubisoft)

ubisoft_bic <- select_model_glmulti(ubisoft, crit = "bic")
ubisoft_bic <- lm(ubisoft_bic, ubisoft)

## forward selection
ubisoft_aic_for <- run_stepwise(ubisoft, direction = "forward", crit = "aic")
ubisoft_bic_for <- run_stepwise(ubisoft, direction = "forward", crit = "bic")
ubisoft_F_for <- run_stepwise(ubisoft, direction = "forward", crit = "F")

## backward selection
ubisoft_aic_back <- run_stepwise(ubisoft, direction = "backward", crit = "aic")
ubisoft_bic_back <- run_stepwise(ubisoft, direction = "backward", crit = "bic")
ubisoft_F_back <- run_stepwise(ubisoft, direction = "backward", crit = "F")

## both directions
ubisoft_aic_both <- run_stepwise(ubisoft, direction = "both", crit = "aic")
ubisoft_bic_both <- run_stepwise(ubisoft, direction = "both", crit = "bic")
ubisoft_F_both <- run_stepwise(ubisoft, direction = "both", crit = "F")

## comparison
models_to_compare <- list(ubisoft_aic, ubisoft_bic,
                          ubisoft_aic_for, ubisoft_bic_for, ubisoft_F_for,
                          ubisoft_aic_both, ubisoft_bic_both, ubisoft_F_both)
names_to_use <- c("AIC", "BIC",
                  "AIC forward", "BIC forward", "Fischer forward",
                  "AIC both", "BIC both", "Fischer both")
# backward direction returns trivial model, so we are not comparing them
compare_models(models_to_compare, names_to_use)

## show variables used in each model
for (i in seq_along(models_to_compare)) {
    cat("\n---", names_to_use[i], "---\n")
    print(attr(terms(models_to_compare[[i]]), "term.labels"))
}

## classification to predict estimated owners ----

# setting class reference to 0-20k
gamesc$Estimated.owners <- relevel(gamesc$Estimated.owners, ref = "0-20k")

# log transformation
gamesc$Average.playtime.forever <- log1p(gamesc$Average.playtime.forever)
gamesc$Peak.CCU <- log1p(gamesc$Peak.CCU)
gamesc$Positive <- log1p(gamesc$Positive)
gamesc$Negative <- log1p(gamesc$Negative)
gamesc$Recommendations <- log1p(gamesc$Recommendations)
gamesc$Price <- log1p(gamesc$Price)

# standardisation
X <- c("Average.playtime.forever", "Peak.CCU", "Positive", "Negative", 
       "Recommendations", "Price", "Required.age")
gamesc_scaled <- as.data.frame(scale(gamesc[, X]))
gamesc_scaled$Estimated.owners <- gamesc$Estimated.owners

# create logit model
model_logit <- multinom(Estimated.owners ~ ., data = gamesc_scaled)

# testing coefficient significance
z <- summary(model_logit)$coefficients / summary(model_logit)$standard.errors
p_values <- 2 * (1 - pnorm(abs(z)))
cat("\n--- P-values des coefficients ---\n")
print(round(p_values, 4))

# AIC of model
cat("\n--- AIC du modèle ---\n")
print(AIC(model_logit))

# pseudo R²
cat("\n--- Pseudo R² ---\n")
print(pR2(model_logit))

# VIF for multicolinearity
mod_lineaire_temp <- lm(
    as.numeric(as.factor(Estimated.owners)) ~ Peak.CCU + Positive + Negative + Recommendations + Price + Required.age,
    data = gamesc_scaled
)
cat("\n--- VIF (multicolinéarité) ---\n")
print(vif(mod_lineaire_temp))

# prediction quality, confusion matrix
pred <- predict(model_logit)
cat("\n--- Matrice de confusion ---\n")
print(table(Predicted = pred, Actual = gamesc_scaled$Estimated.owners))

# accuracy calculation
accuracy <- mean(pred == gamesc_scaled$Estimated.owners)
cat("\n--- Taux de bonnes prédictions ---\n")
print(round(accuracy, 4))

# trivial classification, always predict most common class
majority_class <- names(which.max(table(gamesc_scaled$Estimated.owners)))
trivial_pred <- rep(majority_class, nrow(gamesc_scaled))

# accuracy of trivial classification
trivial_accuracy <- mean(trivial_pred == gamesc_scaled$Estimated.owners)
cat("\n--- Taux de bonnes prédictions (modèle trivial) ---\n")
print(round(trivial_accuracy, 4))

## polynomial models tests ----
check_polynomial_models <- function(data, response_var, predictors) {
    for (var in predictors) {
        cat("\n", strrep("-", 60), "\n")
        cat("Analyse pour la variable explicative :", var, "\n")
        
        # creating models with deg 1, 2 and 3
        f1 <- as.formula(paste(response_var, "~", var))
        f2 <- as.formula(paste(response_var, "~", var, "+ I(", var, "^2)"))
        f3 <- as.formula(paste(response_var, "~", var, "+ I(", var, "^2) + I(", var, "^3)"))
        m1 <- lm(f1, data = data)
        m2 <- lm(f2, data = data)
        m3 <- lm(f3, data = data)
        
        # anova
        cat("\n> Résultat ANOVA entre les modèles de degré 1 à 3 :\n")
        print(anova(m1, m2, m3))
        
        # R2 adjusted
        r2_adj <- c(
            round(summary(m1)$adj.r.squared, 4),
            round(summary(m2)$adj.r.squared, 4),
            round(summary(m3)$adj.r.squared, 4)
        )
        names(r2_adj) <- c("Degré 1", "Degré 2", "Degré 3")
        cat("\n> R² ajusté :\n")
        print(r2_adj)
        
        # AIC
        cat("\n> AIC des modèles :\n")
        print(AIC(m1, m2, m3))
        
        # graph to compare models performances
        print(
            ggplot(data, aes(x = !!sym(var), y = !!sym(response_var))) +
                geom_point(alpha = 0.5) +
                geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black") +
                geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "red") +
                geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE, color = "blue") +
                labs(title = paste("Ajustements polynomiaux pour", var),
                     subtitle = "Noir = linéaire, rouge = degré 2, bleu = degré 3")
        )
    }
}

# function to transform data with log10
apply_transformations <- function(data, variables) {
    for (var in variables) {
        # log transformation: log10(x + 1) to avoid log(0)
        data[[var]] <- log10(data[[var]] + 1)
    }
    return(data)
}

# log tranformations
variables_to_transform <- c("Average.playtime.forever","Peak.CCU",
                            "Positive", "Negative", "Recommendations", "Price")
gamesc_log <- apply_transformations(gamesc, variables_to_transform)

X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
check_polynomial_models(gamesc, response_var = "Average.playtime.forever", predictors = X)
