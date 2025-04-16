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

def print_owners(cleaned_games):
    # Dictionnaire de renommage
    new_labels = {
        "0 - 20000": "0-20k",
        "20000 - 50000": "20k-50k",
        "50000 - 100000": "50k-100k",
        "100000 - 200000": "100k-200k",
        "200000 - 500000": "200k-500k",
        "500000 - 1000000": "500k-1M",
        "1000000 - 2000000": "1M-2M",
        "2000000 - 5000000": "2M-5M",
        "5000000 - 10000000": "5M-10M",
        "10000000 - 20000000": "10M-20M",
        "20000000 - 50000000": "20M-50M",
        "50000000 - 100000000": "50M-100M",
        "100000000 - 200000000": "100M-200M"
    }

    # Appliquer le renommage
    cleaned_games["Estimated owners2"] = cleaned_games["Estimated owners"].map(new_labels)

    # Définir l'ordre des catégories
    ordered_categories = list(new_labels.values())
    cleaned_games["Estimated owners2"] = pd.Categorical(
        cleaned_games["Estimated owners2"],
        categories=ordered_categories,
        ordered=True
    )

    # Afficher les niveaux
    print("Catégories ordonnées :", cleaned_games["Estimated owners2"].cat.categories)

    # Tracer le graphique
    plt.figure(figsize=(10, 6))
    sns.countplot(data=cleaned_games, x="Estimated owners2", color="steelblue")
    plt.title("Distribution of Estimated Owners")
    plt.xlabel("Estimated Owners")
    plt.ylabel("Number of Games")
    plt.xticks(rotation=45, ha="right")
    plt.tight_layout()
    plt.show()

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    cleaned_games = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    print_boxplots(cleaned_games)
    print_required_age(cleaned_games)
    print_owners(cleaned_games)