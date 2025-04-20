import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from setup import load_and_clean_games

# set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def plot_rating_distribution(gamesc):
    # frequency
    rating_counts = gamesc["rating"].value_counts()
    print("Fréquences absolues :")
    print(rating_counts)

    # precentages
    rating_percent = rating_counts / rating_counts.sum() * 100
    print("\nPourcentages (%):")
    print(rating_percent.round(2))

    # df for plot
    rating_df = pd.DataFrame({
        "rating": rating_counts.index,
        "percentage": rating_percent.values
    })

    # horizontal graph
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
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative", "rating"
    ]]
    plot_rating_distribution(cleaned_games)