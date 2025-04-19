import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
from sklearn.preprocessing import LabelEncoder, StandardScaler
from statsmodels.stats.outliers_influence import variance_inflation_factor
from scipy.stats import norm

from setup import load_and_clean_games, clean_column_names


def preprocess_data(filepath):
    games = load_and_clean_games(filepath)
    gamesc = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    gamesc = clean_column_names(gamesc).dropna()

    # Recode 'Estimated owners'
    categories = {
        "0 - 20000": "0-20k", "20000 - 50000": "20k-50k", "50000 - 100000": "50k-100k",
        "100000 - 200000": "100k-200k", "200000 - 500000": "200k-500k", "500000 - 1000000": "500k-1M",
        "1000000 - 2000000": "1M-2M", "2000000 - 5000000": "2M-5M", "5000000 - 10000000": "5M-10M",
        "10000000 - 20000000": "10M-20M", "20000000 - 50000000": "20M-50M",
        "50000000 - 100000000": "50M-100M", "100000000 - 200000000": "100M-200M"
    }
    gamesc['estimated_owners'] = gamesc['estimated_owners'].map(categories)
    le = LabelEncoder()
    gamesc['estimated_owners'] = le.fit_transform(gamesc['estimated_owners'])

    return gamesc


def prepare_features(gamesc, X):
    X_data = gamesc[X]
    X_data_log = np.log1p(X_data).clip(lower=0, upper=100)
    X_data_log = sm.add_constant(X_data_log)

    # Standardize
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X_data_log.drop(columns='const'))
    X_data_scaled = np.column_stack([np.ones(X_scaled.shape[0]), X_scaled])
    return X_data_log, X_data_scaled


def fit_multinomial_logit(Y, X_scaled):
    model = sm.MNLogit(Y, X_scaled)
    return model.fit()


def print_model_summary(model_fit):
    print(model_fit.summary())

    # P-values
    z = model_fit.params / model_fit.bse
    p_values = 2 * (1 - norm.cdf(np.abs(z)))
    print("\n--- P-values of coefficients ---")
    print(np.round(p_values, 4))

    # AIC and Pseudo R²
    print("\n--- AIC ---")
    print(model_fit.aic)
    print("\n--- Pseudo R² ---")
    print(model_fit.prsquared)


def compute_vif(X_data_log, X_vars):
    X_no_const = X_data_log.drop(columns='const')
    vif_data = pd.DataFrame()
    vif_data["Variable"] = X_vars
    vif_data["VIF"] = [variance_inflation_factor(X_no_const.values, i)
                       for i in range(X_no_const.shape[1])]
    print("\n--- VIF (multicollinearity) ---")
    print(vif_data)


def evaluate_model(model_fit, X_scaled, Y_true):
    pred = model_fit.predict(X_scaled)
    pred_class = np.argmax(pred, axis=1)
    conf_matrix = pd.crosstab(pred_class, Y_true)
    print("\n--- Confusion Matrix ---")
    print(conf_matrix)

    accuracy = np.mean(pred_class == Y_true)
    print("\n--- Accuracy ---")
    print(np.round(accuracy, 4))


def run_analysis(filepath, X_vars):
    gamesc = preprocess_data(filepath)
    X_data_log, X_scaled = prepare_features(gamesc, X_vars)
    model_fit = fit_multinomial_logit(gamesc['estimated_owners'], X_scaled)
    print_model_summary(model_fit)
    compute_vif(X_data_log, X_vars)
    evaluate_model(model_fit, X_scaled, gamesc['estimated_owners'])


if __name__ == "__main__":
    os.chdir(os.path.dirname(os.path.abspath(__file__)))
    filepath = "../steam_data/games.csv"
    variables = ['positive', 'peak_ccu']
    run_analysis(filepath, variables)
