import os
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from setup import load_and_clean_games

os.chdir(os.path.dirname(os.path.abspath(__file__)))

def plot_density(data, column, title, log=False):
    plt.figure(figsize=(8, 5))
    if log:
        # avoid log(0)
        data = data[data[column] > 0]
        x = np.log10(data[column])
        xlabel = f"log10({column})"
    else:
        x = data[column]
        xlabel = column
    sns.kdeplot(x, fill=True, color="blue", alpha=0.3)
    plt.title(title, fontsize=14, fontweight='bold')
    plt.xlabel(xlabel)
    plt.ylabel("Density")
    plt.grid(True, linestyle="--", alpha=0.5)
    plt.tight_layout()
    plt.show()

def print_log_tests(cleaned_games):
    plot_density(cleaned_games, "Peak CCU", "Density Plot of Peak CCU (log10 scale)", log=True)
    plot_density(cleaned_games, "Price", "Density Plot of Price", log=False)
    plot_density(cleaned_games, "Positive", "Density Plot of number of positive reviews (log10 scale)", log=True)
    plot_density(cleaned_games, "Negative", "Density Plot of number of negative reviews (log10 scale)", log=True)
    plot_density(cleaned_games, "Recommendations", "Density Plot of number of recommendations (log10 scale)", log=True)
    plot_density(cleaned_games, "Average playtime forever", "Density Plot of Average playtime forever (log10 scale)", log=True)

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    print_log_tests(cleaned_games)