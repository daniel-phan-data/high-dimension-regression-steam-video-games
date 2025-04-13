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

def plot_rating_distribution(gamesc):
    # Fréquences
    rating_counts = gamesc["rating"].value_counts()
    print("Fréquences absolues :")
    print(rating_counts)

    # Pourcentages
    rating_percent = rating_counts / rating_counts.sum() * 100
    print("\nPourcentages (%):")
    print(rating_percent.round(2))

    # DataFrame pour le plot
    rating_df = pd.DataFrame({
        "rating": rating_counts.index,
        "percentage": rating_percent.values
    })

    # Graphique horizontal
    plt.figure(figsize=(8, 5))
    sns.barplot(data=rating_df, y="rating", x="percentage", color="steelblue", edgecolor="black")
    plt.title("Proportion of Game Ratings", fontsize=14, weight='bold')
    plt.xlabel("Proportion (%)")
    plt.ylabel("Rating")
    plt.xlim(0, rating_df["percentage"].max() * 1.1)
    plt.grid(axis="x", linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    # print(games.head())
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative", "rating"
    ]]
    plot_rating_distribution(cleaned_games)