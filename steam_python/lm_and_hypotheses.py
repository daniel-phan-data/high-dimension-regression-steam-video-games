import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy.stats import zscore
import statsmodels.formula.api as smf
from statsmodels.stats.stattools import durbin_watson
from statsmodels.stats.outliers_influence import variance_inflation_factor
import statsmodels.api as sm

from setup import load_and_clean_games, clean_column_names

# set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

# create linear model with given dataset, Y, quantitative variables X and qualitative variables
def create_lm(dataset, Y, X, categories):
    if len(categories) == 0:
        formula = f"{Y} ~ {' + '.join(X)}"
    else:
        formula = f"{Y} ~ {' + '.join(X + categories)}"
    
    model = smf.ols(formula=formula, data=dataset).fit()
    return model

# check essential hypotheses for given linear model and data
def check_lm_hypotheses(model, data):
    print("Vérification des hypothèses pour le modèle :")
    print(model.model.formula)
    print("\n")

    # 1. residuals vs fitted values for linearity check
    fitted_vals = model.fittedvalues
    residuals = model.resid
    plt.figure(figsize=(6, 4))
    sns.residplot(x=fitted_vals, y=residuals, lowess=True, line_kws={'color': 'red'})
    plt.xlabel("Valeurs ajustées")
    plt.ylabel("Résidus")
    plt.title("1. Résidus vs valeurs ajustées")
    plt.axhline(0, color='gray', linestyle='--')
    plt.show()

    # 2. residuals normality
    sm.qqplot(residuals, line='45', fit=True)
    plt.title("2. residuals QQ-plot")
    plt.show()

    # 3. residuals independance
    standardized_residuals = residuals / np.std(residuals)
    sqrt_std_resid = np.sqrt(np.abs(standardized_residuals))
    plt.figure(figsize=(6, 4))
    sns.scatterplot(x=fitted_vals, y=sqrt_std_resid, alpha=0.4)
    sns.regplot(x=fitted_vals, y=sqrt_std_resid, scatter=False, lowess=True, line_kws={'color': 'red'})
    plt.title("3. Écarts à l'effet de levier")
    plt.xlabel("Valeurs ajustées")
    plt.ylabel("√|Résidus standardisés|")
    plt.show()

    # 4. homoscedasticity
    abs_resid = np.abs(residuals)
    plt.figure(figsize=(6, 4))
    sns.scatterplot(x=fitted_vals, y=abs_resid, alpha=0.4)
    sns.regplot(x=fitted_vals, y=abs_resid, scatter=False, lowess=True, line_kws={'color': 'red'})
    plt.title("4. Hétéroscédasticité : résidus absolus vs ajustés")
    plt.xlabel("Valeurs ajustées")
    plt.ylabel("Résidus absolus")
    plt.show()

    # 5. autocorrelation with durbin-watson and acf
    dw_stat = durbin_watson(residuals)
    print(f"\n5. Durbin-Watson : {dw_stat:.3f} (attendu ≈ 2)\n")

    # ACF
    sm.graphics.tsa.plot_acf(residuals, lags=40)
    plt.title("5. ACF des résidus")
    plt.show()

    # 6. histogram and boxplot of residuals
    plt.figure(figsize=(4, 4))
    sns.boxplot(y=residuals)
    plt.title("Boxplot des résidus")
    plt.show()

    plt.figure(figsize=(6, 4))
    sns.histplot(residuals[residuals < np.quantile(residuals, 1)], bins=50, kde=True, color="blue")
    plt.title("Histogramme des résidus")
    plt.xlabel("Résidus")
    plt.show()

    plt.figure(figsize=(6, 4))
    sns.histplot(residuals[residuals < np.quantile(residuals, 0.99)], bins=50, kde=True, color="blue")
    plt.title("Histogramme des résidus (sans top 1%)")
    plt.xlabel("Résidus")
    plt.show()

    # 7. multicolinearity with VIF
    print("7. VIF (Variance Inflation Factor) :")
    X = model.model.exog
    vif_data = pd.DataFrame()
    vif_data["Variable"] = model.model.exog_names
    vif_data["VIF"] = [variance_inflation_factor(X, i) for i in range(X.shape[1])]
    print(vif_data)
    print("\nVariables avec VIF > 5 :")
    print(vif_data[vif_data["VIF"] > 5]["Variable"].tolist())

    # 8. Cook's distance
    cooks_d = model.get_influence().cooks_distance[0]
    seuil = 4 / len(data)
    plt.figure(figsize=(8, 4))
    plt.stem(np.arange(len(cooks_d)), cooks_d, markerfmt=",")
    plt.axhline(y=seuil, color="red", linestyle="--", linewidth=2)
    plt.title("8. Distance de Cook avec seuil 4/n")
    plt.xlabel("Index de l'observation")
    plt.ylabel("Distance de Cook")
    plt.legend([f"Seuil = 4/n ≈ {seuil:.5f}"], loc="upper right")
    plt.tight_layout()
    plt.show()

# apply transformation according to the specified method
def apply_transformations(data, variables, method="log"):
    data = data.copy()
    for var in variables:
        if method == "log":
            data[var] = np.log10(data[var] + 1)
        elif method == "sqrt":
            data[var] = np.sqrt(data[var])
        elif method == "standardize":
            data[var] = (data[var] - data[var].mean()) / data[var].std()
        elif method == "normalize":
            data[var] = (data[var] - data[var].min()) / (data[var].max() - data[var].min())
    return data

# detects high influence point according to Cook's distance
def detect_cook(model, threshold=None):
    influence = model.get_influence()
    cooks_d = influence.cooks_distance[0]
    if threshold is None:
        threshold = 4 / model.nobs
    return np.where(cooks_d > threshold)[0]

# detects large residuals
def detect_large_residuals(model, threshold=3):
    influence = model.get_influence()
    student_resid = influence.resid_studentized_external
    return np.where(np.abs(student_resid) > threshold)[0]

# detects outliers according to zscore=3
def detect_outliers_data(dataset, threshold=3):
    numeric_data = dataset.select_dtypes(include=[np.number])
    z_scores = zscore(numeric_data, nan_policy='omit')
    outlier_rows = np.where(np.abs(z_scores) > threshold)
    return np.unique(outlier_rows[0])

# removes large residuals, outliers and high influence point according to Cook's distance
def clean_model(model, dataset):
    idx_cook = detect_cook(model)
    idx_resid = detect_large_residuals(model)
    idx_outliers = detect_outliers_data(dataset)
    idx_to_remove = np.unique(np.concatenate([idx_cook, idx_resid, idx_outliers]))
    cleaned_data = dataset.drop(index=idx_to_remove)
    return {
        'data': cleaned_data,
        'removed': idx_to_remove
    }

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    gamesc = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    gamesc = clean_column_names(gamesc)
    Y = "average_playtime_forever"
    X = ["peak_ccu", "positive", "negative", "recommendations", "price", "required_age"]
    categories = ["estimated_owners"]

    model = create_lm(gamesc, Y, X, categories)
    print(model.summary())
    check_lm_hypotheses(model, gamesc)

    variables_to_transform = ["peak_ccu", "positive", "negative", "recommendations", "price"]
    gamesc_log = apply_transformations(gamesc, variables_to_transform, "log")
    model_log = create_lm(gamesc_log, Y, X, categories)
    print(model_log.summary())
    check_lm_hypotheses(model_log, gamesc_log)

    cleaned_result = clean_model(model, gamesc_log)
    gamesc_log_clean = cleaned_result['data']
    model_log_clean = create_lm(gamesc_log_clean, Y, X, categories)
    print(model_log_clean.summary())
    check_lm_hypotheses(model_log_clean, gamesc_log_clean)