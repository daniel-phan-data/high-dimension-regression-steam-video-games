# tests polynomial models up to degree 3 for each given variable
# variable to predict is Average.playtime.forever

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

