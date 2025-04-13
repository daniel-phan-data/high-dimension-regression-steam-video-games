import os
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from setup import load_and_clean_games

# Set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def print_correlation_matrix(cleaned_games):
    # Garder uniquement les colonnes numériques
    numeric_vars = cleaned_games.select_dtypes(include=[np.number])

    # Matrice de corrélation (méthode de Spearman pour capter les liens non linéaires)
    cor_matrix = numeric_vars.corr(method='spearman')
    print("Spearman Correlation Matrix:")
    print(cor_matrix)

    # Heatmap type corrplot (triangle supérieur, clustering par similarité)
    plt.figure(figsize=(10, 8))
    mask = np.triu(np.ones_like(cor_matrix, dtype=bool))  # Masquer le triangle inférieur
    cmap = sns.diverging_palette(240, 10, as_cmap=True)
    sns.heatmap(cor_matrix, mask=mask, cmap=cmap, center=0, annot=True,
                fmt=".2f", square=True, linewidths=.5, cbar_kws={"shrink": .8})
    plt.title("Spearman Correlation Matrix (upper triangle)")
    plt.show()

    # Heatmap carrée style ggcorrplot
    plt.figure(figsize=(10, 8))
    sns.heatmap(cor_matrix, cmap="coolwarm", annot=True, fmt=".2f",
                square=True, cbar=True, linewidths=.5)
    plt.title("Spearman Correlation Heatmap (full)")
    plt.show()

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    # print(games.head())
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    print_correlation_matrix(cleaned_games)