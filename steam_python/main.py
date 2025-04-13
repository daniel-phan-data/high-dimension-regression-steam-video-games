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

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    # print(games.head())
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]