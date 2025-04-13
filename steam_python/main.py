import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import numpy as np
from setup import load_and_clean_games

# Set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    gamesc = load_and_clean_games(filepath)
    # gamesc = pd.read_csv(filepath)
    print(gamesc.head())