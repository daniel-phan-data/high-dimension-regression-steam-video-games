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

## normality tests for quantitative variables (bivar tests) ----
# select variables to test
names(gamesc)
numeric_cols <- gamesc %>%
  select(Average.playtime.forever, Peak.CCU, Price, Recommendations, Required.age,
         Positive, Negative, total_reviews, positive_ratio)

# log transformation
numeric_cols_log <- numeric_cols %>%
  mutate(across(everything(), ~ log10(. + 1)))

# lillie tests
lillie_results <- lapply(numeric_cols_log, lillie.test)

# store results in a dataframe
lillie_table <- data.frame(
  "p-value lillie tests" = sapply(lillie_results, function(x) x$p.value)
)
print(lillie_table)

## spearman because there is no normality ----
names(gamesc)
numeric_cols <- gamesc %>%
  select(Average.playtime.forever, Peak.CCU, Price, Recommendations, Required.age,
         Positive, Negative, total_reviews, positive_ratio)
names(numeric_cols)

# spearman for every combination with average.playtime.forever
spearman_results <- list()
rho_values <- list()  # store rho values in a list

for (var in names(numeric_cols)[-1]) {  # exclude average playtime 
  test_result <- suppressWarnings(cor.test(numeric_cols$Average.playtime.forever, numeric_cols[[var]], method = "spearman"))
  
  # store rho and p values in lists
  spearman_results[[var]] <- test_result$p.value
  rho_values[[var]] <- test_result$estimate
}

# store tests results in dataframe
spearman_table <- data.frame(
  "p-value spearman" = unlist(spearman_results),
  "rho (Spearman)" = unlist(rho_values)
)

print(spearman_table)


## kruskal wallis because no normality and data is not paired ----
names(gamesc)
# target variable
y <- gamesc$Average.playtime.forever

# categorical predictors
quali <- gamesc %>%
  select(Estimated.owners, rating)

# empty table to store results
kruskal_table <- data.frame(
  Test = character(),
  H_statistic = numeric(),
  p_value = numeric(),
  stringsAsFactors = FALSE
)

# kruskal test for all the variables of the list
for (var in names(quali)) {
  test_result <- kruskal.test(y ~ gamesc[[var]], data = gamesc)
  
  # add results to table
  kruskal_table <- rbind(kruskal_table, data.frame(
    Test = var,  # display variable name instead of test name
    H_statistic = test_result$statistic,
    p_value = test_result$p.value
  ))
}

print(kruskal_table)

## display all bivariate tests results ----
print(lillie_table)
print(spearman_table)
print(kruskal_table)

