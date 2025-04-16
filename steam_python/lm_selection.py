import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
from itertools import combinations
from sklearn.preprocessing import OneHotEncoder
from sklearn.compose import ColumnTransformer

from setup import load_and_clean_games, clean_column_names

# Set working directory
os.chdir(os.path.dirname(os.path.abspath(__file__)))

def select_model_glmulti(data, target_col='average_playtime_forever', criterion='aic', level=1):
    X = data.drop(columns=[target_col])
    y = data[target_col]

    best_score = np.inf
    best_model = None
    best_formula = None

    for i in range(1, level + 1):
        for combo in combinations(X.columns, i):
            X_combo = sm.add_constant(X[list(combo)])
            model = sm.OLS(y, X_combo).fit()
            score = model.aic if criterion == 'aic' else model.bic
            if score < best_score:
                best_score = score
                best_model = model
                best_formula = combo

    print(f"Best formula: {best_formula}, score ({criterion}): {best_score:.2f}")
    return best_model

def run_stepwise(data, target_col='average_playtime_forever', direction='forward', criterion='aic'):
    def compute_score(model):
        return model.aic if criterion == 'aic' else model.bic

    X = data.drop(columns=[target_col])
    y = data[target_col]
    included = []
    changed = True
    best_score = None

    while changed:
        changed = False
        excluded = list(set(X.columns) - set(included))
        candidates = []

        if direction in ['forward', 'both']:
            for new_col in excluded:
                model = sm.OLS(y, sm.add_constant(data[included + [new_col]])).fit()
                score = compute_score(model)
                candidates.append((score, new_col))

        if direction in ['backward', 'both'] and included:
            for col in included:
                temp_included = included.copy()
                temp_included.remove(col)
                if temp_included:
                    model = sm.OLS(y, sm.add_constant(data[temp_included])).fit()
                    score = compute_score(model)
                    candidates.append((score, f"-{col}"))

        if not candidates:
            break

        candidates.sort()
        best_candidate = candidates[0]
        if best_score is None or best_candidate[0] < best_score:
            best_score = best_candidate[0]
            col = best_candidate[1]
            if col.startswith('-'):
                included.remove(col[1:])
            else:
                included.append(col)
            changed = True

    final_model = sm.OLS(y, sm.add_constant(data[included])).fit()
    print(f"Final variables: {included}")
    return final_model

def turn_data_to_num(data, target_col='average_playtime_forever'):
    # Séparer variables numériques et catégorielles
    X = data.drop(columns=[target_col])
    y = data[target_col]

    categorical_cols = X.select_dtypes(include=['object', 'category']).columns.tolist()
    # numeric_cols = X.select_dtypes(include=[np.number]).columns.tolist()

    # Pipeline pour one-hot encoding
    transformer = ColumnTransformer(transformers=[
        ('cat', OneHotEncoder(drop='first', sparse_output=False), categorical_cols)
    ], remainder='passthrough')

    X_transformed = transformer.fit_transform(X)
    feature_names = transformer.get_feature_names_out()

    df_transformed = pd.DataFrame(X_transformed, columns=feature_names)
    df_transformed[target_col] = y.reset_index(drop=True)
    return df_transformed

if __name__ == "__main__":
    filepath = "../steam_data/games.csv"
    games = load_and_clean_games(filepath)
    # print(games.head())
    gamesc = games[[
        "Average playtime forever", "Estimated owners",
        "Peak CCU", "Price", "Recommendations", "Required age",
        "Positive", "Negative"
    ]]
    gamesc = clean_column_names(gamesc)
    # Transformation éventuelle si tu as des variables catégorielles
    gamesc_num = turn_data_to_num(gamesc, target_col='average_playtime_forever')

    print("\n--- Sélection via glmulti (niveau 1) ---")
    glmulti_model = select_model_glmulti(gamesc_num, criterion='aic', level=1)
    print(glmulti_model.summary())

    print("\n--- Sélection par stepwise (forward + AIC) ---")
    stepwise_model = run_stepwise(gamesc_num, direction='forward', criterion='aic')
    print(stepwise_model.summary())