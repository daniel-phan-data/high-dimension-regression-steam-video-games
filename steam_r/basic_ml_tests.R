## IMPORTS ----
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
temp_env <- new.env()
source("0setup.R", local = temp_env)
gamesc <- temp_env$setup()
rm(temp_env)

gamesc <- gamesc %>% select(-Name)

# Function to create a linear model
create_ml <- function(dataset, Y, X, categories) {
    if (length(Category) == 0) {
        formula <- as.formula(paste(Y, "~", paste(X, collapse = "+")))
    } else {
        formula <- as.formula(paste(Y, "~", paste(c(X, categories), collapse = "+")))
    }
    modele.RLM <- lm(formula = formula, data = dataset)
    return(modele.RLM)
}


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

#replace below Q1 with Q1, and anything above Q3+1.5IQR with that
#basically cap extreme values, because very high outliers and too many small values
winsorize_dataset <- function(data, variables) {
    for (var in variables) {
        Q1 <- quantile(data[[var]], 0.25, na.rm = TRUE)
        Q3 <- quantile(data[[var]], 0.75, na.rm = TRUE)
        IQR_value <- Q3 - Q1
        lower_bound <- Q1
        upper_bound <- Q3 + 1.5 * IQR_value
        
        data[[var]] <- ifelse(data[[var]] < lower_bound, lower_bound, data[[var]])
        data[[var]] <- ifelse(data[[var]] > upper_bound, upper_bound, data[[var]])
    }
    return(data)
}


## on essaie de trouver un modele correct ----
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")

categories <- c()
categories <- c("Estimated.owners")
names(gamesc)
variables <- c(Y, X, categories)  # Combined variables list
print(variables)
# log, normalize and standardize
gamesc_transformed <- apply_transformations(gamesc, X)
variables_to_winsorize <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
gamesc_w <- winsorize_dataset(gamesc, variables_to_winsorize)
# Create the model
# modele.RLM <- create_ml(gamesc, Y, X, categories)
modele.RLM <- lm(formula = Average.playtime.forever ~ ., data = gamesc_w)

# View model summary# View model summarygamesc
summary(modele.RLM)
