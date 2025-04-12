import os
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from setup import load_and_clean_games

os.chdir(os.path.dirname(os.path.abspath(__file__)))

def print_boxplots(cleaned_games: pd.DataFrame) -> None:

    plt.boxplot(cleaned_games['Average playtime forever'].dropna())
    plt.title("Distribution du temps de jeu moyen")
    plt.ylabel("Temps de jeu moyen (minutes)")
    plt.show()

    plt.boxplot(cleaned_games['Recommendations'].dropna())
    plt.title("Distribution du nombre de recommandations")
    plt.ylabel("Nombre de recommandations")
    plt.show()

    plt.boxplot(cleaned_games['Negative'].dropna())
    plt.title("Distribution du nombre d'avis négatifs")
    plt.ylabel("Nombre d'avis négatifs")
    plt.show()

    plt.boxplot(cleaned_games['Positive'].dropna())
    plt.title("Distribution du nombre d'avis positifs")
    plt.ylabel("Nombre d'avis positifs")
    plt.show()

    plt.boxplot(cleaned_games['Price'].dropna())
    plt.title("Distribution du prix des jeux")
    plt.ylabel("Prix en dollars")
    plt.show()

    plt.boxplot(cleaned_games['Peak CCU'].dropna())
    plt.title("Distribution des maximums de joueurs simultanés atteints")
    plt.ylabel("Nombre de joueurs simultanés")
    plt.show()

def categorize_age(age: int) -> str:
    if pd.isna(age) or age == "" or age < 12:
        return "Tout public"
    elif 12 <= age < 16:
        return "+12"
    elif 16 <= age < 18:
        return "+16"
    else:
        return "+18"

def print_required_age(cleaned_games: pd.DataFrame) -> None:
    cleaned_games['age_Category'] = cleaned_games['Required age'].apply(categorize_age)
    # Barplot avec Seaborn
    sns.countplot(data=cleaned_games, x='age_Category', order=["Tout public", "+12", "+16", "+18"], color="steelblue")
    plt.title("Fréquence des catégories d'âge")
    plt.xlabel("Âge requis")
    plt.ylabel("Nombre de jeux")
    plt.tight_layout()
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
    # print_boxplots(cleaned_games)
    print_required_age(cleaned_games)