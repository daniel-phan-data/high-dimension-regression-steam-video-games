import os
import pandas as pd
from setup import load_and_clean_games

# Set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    gamesc = load_and_clean_games(filepath)
    # gamesc = pd.read_csv(filepath)
    print(gamesc.head())