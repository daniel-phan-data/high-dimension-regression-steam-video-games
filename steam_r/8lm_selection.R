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

turn_data_to_num <- function(data) {
  y <- data[["Average.playtime.forever"]]
  XX <- model.matrix(~ ., data = data %>% select(-Average.playtime.forever))[, -1]
  colnames(XX) <- make.names(colnames(XX))
  data_num <- as.data.frame(cbind(Average.playtime.forever = y, XX))
  return(data_num)
}

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

ubisoft <- games %>% filter(Publishers == "Ubisoft") %>%
  select(Average.playtime.forever, Estimated.owners,
         Peak.CCU, rating, Price,
         Recommendations, Required.age,
         Positive, Negative,
         total_reviews, positive_ratio)

## aic bic ----

ubisoft_aic <- select_model_glmulti(ubisoft, crit = "aic")
ubisoft_aic <- lm(ubisoft_aic, ubisoft)
summary(ubisoft_aic)

ubisoft_bic <- select_model_glmulti(ubisoft, crit = "bic")
ubisoft_bic <- lm(ubisoft_bic, ubisoft)
summary(ubisoft_bic)

## aic bic after turning whole data into numeric ----
# (too long, and owners is good on its own)

# ubisoft_num <- turn_data_to_num(ubisoft)
# ubisoft_aic_num <- select_model_glmulti(ubisoft_num, crit = "aic")
# ubisoft_aic_num <- lm(ubisoft_aic, ubisoft_num)
# summary(ubisoft_aic_num)


## forward selection ----

# aic 
ubisoft_aic_for <- run_stepwise(ubisoft, direction = "forward", crit = "aic")
# bic
ubisoft_bic_for <- run_stepwise(ubisoft, direction = "forward", crit = "bic")
# Fischer
ubisoft_F_for <- run_stepwise(ubisoft, direction = "forward", crit = "F")

## backward selection ----

# aic
ubisoft_aic_back <- run_stepwise(ubisoft, direction = "backward", crit = "aic")
# bic
ubisoft_bic_back <- run_stepwise(ubisoft, direction = "backward", crit = "bic")
#s Fischer
ubisoft_F_back <- run_stepwise(ubisoft, direction = "backward", crit = "F")

## both directions ----

# aic
ubisoft_aic_both <- run_stepwise(ubisoft, direction = "both", crit = "aic")
# bic
ubisoft_bic_both <- run_stepwise(ubisoft, direction = "both", crit = "bic")
# Fischer
ubisoft_F_both <- run_stepwise(ubisoft, direction = "both", crit = "F")

## comparison ----

models_to_compare <- list(ubisoft_aic_for, ubisoft_bic_for, ubisoft_F_for,
                          ubisoft_aic_both, ubisoft_bic_both, ubisoft_F_both)
names_to_use <- c("AIC forward", "BIC forward", "Fischer forward",
                  "AIC both", "BIC both", "Fischer both")

compare_models(models_to_compare, names_to_use)

