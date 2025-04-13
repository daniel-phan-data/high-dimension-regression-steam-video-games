import pandas as pd
import os
import re

os.chdir(os.path.dirname(os.path.abspath(__file__)))

def create_total_reviews(positive, negative):
    return positive + negative

def create_positive_ratio(positive, negative):
    total = positive + negative
    if total == 0:
        return 0
    return (positive / total) * 100

def create_rating(positive, negative):
    total = positive + negative
    if total == 0:
        return "Not enough reviews"

    ratio = (positive / total) * 100

    if total >= 500:
        if 95 <= ratio <= 100:
            return "Overwhelmingly Positive"
        elif 80 <= ratio < 95:
            return "Very Positive"
        elif 70 <= ratio < 80:
            return "Mostly Positive"
        elif 40 <= ratio < 70:
            return "Mixed"
        elif 20 <= ratio < 40:
            return "Mostly Negative"
        else:
            return "Overwhelmingly Negative"
    elif 50 <= total <= 499:
        if 80 <= ratio <= 100:
            return "Very Positive"
        elif 70 <= ratio < 80:
            return "Mostly Positive"
        elif 40 <= ratio < 70:
            return "Mixed"
        elif 20 <= ratio < 40:
            return "Mostly Negative"
        else:
            return "Very Negative"
    elif 10 <= total <= 49:
        if 80 <= ratio <= 100:
            return "Positive"
        elif 70 <= ratio < 80:
            return "Mostly Positive"
        elif 40 <= ratio < 70:
            return "Mixed"
        elif 20 <= ratio < 40:
            return "Mostly Negative"
        else:
            return "Negative"
    else:
        return "Not enough reviews"

def load_and_clean_games(filepath):
    df = pd.read_csv(filepath)

    # Filter games with playtime > 0 and Peak.CCU > 0
    df = df[(df["Average playtime forever"] > 0) & (df["Peak CCU"] > 0)]

    # Fill NA with 0
    df.fillna(0, inplace=True)

    # Create new columns
    df["total_reviews"] = df.apply(lambda row: create_total_reviews(row["Positive"], row["Negative"]), axis=1)
    df["positive_ratio"] = df.apply(lambda row: create_positive_ratio(row["Positive"], row["Negative"]), axis=1)
    df["rating"] = df.apply(lambda row: create_rating(row["Positive"], row["Negative"]), axis=1)

    # Optional: force ordered categories like in R
    rating_levels = [
        "Overwhelmingly Positive", "Very Positive", "Positive", 
        "Mostly Positive", "Mixed", "Mostly Negative", "Negative", 
        "Very Negative", "Overwhelmingly Negative", "Not enough reviews"
    ]
    df["rating"] = pd.Categorical(df["rating"], categories=rating_levels, ordered=True)

    # Keep only relevant columns
    df = df[[
        "Name", "Publishers", "Average playtime forever", "Estimated owners",
        "Peak CCU", "rating", "Price", "Recommendations", "Required age",
        "Positive", "Negative", "total_reviews", "positive_ratio"
    ]]
    df = df.reset_index(drop=True)
    return df

def clean_column_names(df):
    """
    Renomme les colonnes en snake_case sans espaces ni majuscules.
    Exemple : "Estimated Owners" → "estimated_owners"
    """
    new_columns = {
        col: re.sub(r'\W+', '_', col.strip().lower())  # supprime les caractères non alphanumériques et met en snake_case
        for col in df.columns
    }
    df = df.rename(columns=new_columns)
    return df

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    gamesc = load_and_clean_games(filepath)
    print(gamesc.head())