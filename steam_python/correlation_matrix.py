import os
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from setup import load_and_clean_games

# set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def print_correlation_matrix(cleaned_games):
    # keep numeric variables
    numeric_vars = cleaned_games.select_dtypes(include=[np.number])

    # spearman correlation (because no linearity)
    cor_matrix = numeric_vars.corr(method='spearman')
    print("Spearman Correlation Matrix:")
    print(cor_matrix)

    # Heatmap type corrplot
    plt.figure(figsize=(10, 8))
    mask = np.triu(np.ones_like(cor_matrix, dtype=bool))  # hide lower triangle
    cmap = sns.diverging_palette(240, 10, as_cmap=True)
    sns.heatmap(cor_matrix, mask=mask, cmap=cmap, center=0, annot=True,
                fmt=".2f", square=True, linewidths=.5, cbar_kws={"shrink": .8})
    plt.title("Spearman Correlation Matrix (upper triangle)")
    plt.show()

    # other heatmap style
    plt.figure(figsize=(10, 8))
    sns.heatmap(cor_matrix, cmap="coolwarm", annot=True, fmt=".2f",
                square=True, cbar=True, linewidths=.5)
    plt.title("Spearman Correlation Heatmap (full)")
    plt.show()

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    print_correlation_matrix(cleaned_games)