import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from scipy.stats import shapiro, spearmanr, kruskal
import warnings
from setup import load_and_clean_games

# Set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def print_bivariate_tests(cleaned_games):
    warnings.filterwarnings("ignore")  # Supprimer les warnings pour les tests de corr

    # Colonnes quantitatives à tester
    numeric_cols = cleaned_games[[
        "Average playtime forever", "Peak CCU", "Price", "Recommendations",
        "Required age", "Positive", "Negative", "total_reviews", "positive_ratio"
    ]].copy()

    # Appliquer log10(x+1) pour éviter log(0)
    numeric_cols_log = np.log10(numeric_cols + 1)

    # --- Normality tests (Shapiro-Wilk) ---
    lillie_table = pd.DataFrame({
        "p-value Shapiro-Wilk": [
            shapiro(numeric_cols_log[col].dropna())[1]
            for col in numeric_cols_log.columns
        ]
    }, index=numeric_cols_log.columns)
    print("Shapiro-Wilk Test (log10 transformed variables):")
    print(lillie_table)

    # --- Spearman correlation with Average.playtime.forever ---
    spearman_results = {}
    rho_values = {}

    for col in numeric_cols.columns:
        if col != "Average playtime forever":
            rho, pval = spearmanr(numeric_cols["Average playtime forever"], numeric_cols[col], nan_policy="omit")
            spearman_results[col] = pval
            rho_values[col] = rho

    spearman_table = pd.DataFrame({
        "p-value spearman": pd.Series(spearman_results),
        "rho (Spearman)": pd.Series(rho_values)
    })
    print("\nSpearman Correlations with Average playtime forever:")
    print(spearman_table)

    # --- Kruskal-Wallis tests (variables qualitatives) ---
    quali_cols = ["Estimated owners", "rating"]
    kruskal_results = []

    for col in quali_cols:
        if col in cleaned_games.columns:
            groups = [group["Average playtime forever"].dropna() for _, group in cleaned_games.groupby(col)]
            if len(groups) > 1:
                stat, pval = kruskal(*groups)
                kruskal_results.append({
                    "Test": col,
                    "H_statistic": stat,
                    "p_value": pval
                })

    kruskal_table = pd.DataFrame(kruskal_results)
    print("\nKruskal-Wallis Test Results:")
    print(kruskal_table)

    return lillie_table, spearman_table, kruskal_table

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    # print(games.head())
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative", "total_reviews", "positive_ratio"
    ]]
    print_bivariate_tests(cleaned_games)