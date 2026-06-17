from __future__ import annotations

import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder

from shaprpy import explain
from shaprpy.datasets import load_california_housing


def make_mixed_data(dfx_train: pd.DataFrame, dfx_test: pd.DataFrame) -> tuple[pd.DataFrame, pd.DataFrame]:
    dfx_train = dfx_train.copy()
    dfx_test = dfx_test.copy()

    dfx_train["IncomeCategory"] = pd.cut(
        dfx_train["MedInc"], bins=[0, 3, 6, 15], labels=["Low", "Medium", "High"]
    )
    dfx_test["IncomeCategory"] = pd.cut(
        dfx_test["MedInc"], bins=[0, 3, 6, 15], labels=["Low", "Medium", "High"]
    )

    dfx_train["AgeCategory"] = pd.cut(
        dfx_train["HouseAge"], bins=[0, 15, 35, 60], labels=["New", "Mid", "Old"]
    )
    dfx_test["AgeCategory"] = pd.cut(
        dfx_test["HouseAge"], bins=[0, 15, 35, 60], labels=["New", "Mid", "Old"]
    )

    dfx_train["LocationType"] = pd.cut(
        dfx_train["Latitude"], bins=[32, 34, 37, 42], labels=["South", "Central", "North"]
    )
    dfx_test["LocationType"] = pd.cut(
        dfx_test["Latitude"], bins=[32, 34, 37, 42], labels=["South", "Central", "North"]
    )

    for col in ["IncomeCategory", "AgeCategory", "LocationType"]:
        dfx_train[col] = dfx_train[col].cat.add_categories(["Unknown"]).fillna("Unknown").astype(str)
        dfx_test[col] = dfx_test[col].cat.add_categories(["Unknown"]).fillna("Unknown").astype(str)
        categories = pd.unique(pd.concat([dfx_train[col], dfx_test[col]]).values)
        categories = [cat for cat in categories if pd.notna(cat)]
        dfx_train[col] = pd.Categorical(dfx_train[col], categories=categories)
        dfx_test[col] = pd.Categorical(dfx_test[col], categories=categories)

    return dfx_train, dfx_test


def main() -> None:
    dfx_train, dfx_test, dfy_train, _ = load_california_housing()

    # Keep the example light enough to run quickly.
    dfx_train = dfx_train.iloc[:1000].copy()
    dfx_test = dfx_test.iloc[:3].copy()
    dfy_train = dfy_train.iloc[:1000].copy()

    phi0 = dfy_train.mean().item()

    model_numeric = RandomForestRegressor(random_state=1, n_estimators=100)
    model_numeric.fit(dfx_train, dfy_train.values.flatten())

    print("\n=== Numeric features with ARF ===")
    explanation_arf_num = explain(
        model=model_numeric,
        x_train=dfx_train,
        x_explain=dfx_test,
        approach="arf",
        phi0=phi0,
        max_n_coalitions=20,
        n_MC_samples=200,
        arf_num_trees=20,
        arf_max_iters=5,
        seed=1,
    )
    explanation_arf_num.print()

    print("\n=== Numeric features with VAEAC ===")
    explanation_vaeac_num = explain(
        model=model_numeric,
        x_train=dfx_train,
        x_explain=dfx_test,
        approach="vaeac",
        phi0=phi0,
        max_n_coalitions=20,
        n_MC_samples=100,
        vaeac_epochs=10,
        vaeac_width=16,
        vaeac_depth=2,
        vaeac_n_vaeacs_initialize=1,
        seed=1,
    )
    explanation_vaeac_num.print()

    dfx_train_cat, dfx_test_cat = make_mixed_data(dfx_train, dfx_test)

    numeric_features = [
        "MedInc",
        "HouseAge",
        "AveRooms",
        "AveBedrms",
        "Population",
        "AveOccup",
        "Latitude",
        "Longitude",
    ]
    categorical_features = ["IncomeCategory", "AgeCategory", "LocationType"]

    preprocessor = ColumnTransformer(
        transformers=[
            ("num", "passthrough", numeric_features),
            ("cat", OneHotEncoder(handle_unknown="ignore", sparse_output=False), categorical_features),
        ]
    )
    model_mixed = Pipeline(
        steps=[
            ("preprocessor", preprocessor),
            ("model", RandomForestRegressor(random_state=1, n_estimators=100)),
        ]
    )
    model_mixed.fit(dfx_train_cat, dfy_train.values.flatten())

    print("\n=== Mixed numeric/categorical features with ARF ===")
    explanation_arf_cat = explain(
        model=model_mixed,
        x_train=dfx_train_cat,
        x_explain=dfx_test_cat,
        approach="arf",
        phi0=phi0,
        max_n_coalitions=20,
        n_MC_samples=200,
        arf_num_trees=20,
        arf_max_iters=5,
        seed=1,
    )
    explanation_arf_cat.print()

    print("\n=== Mixed numeric/categorical features with VAEAC ===")
    explanation_vaeac_cat = explain(
        model=model_mixed,
        x_train=dfx_train_cat,
        x_explain=dfx_test_cat,
        approach="vaeac",
        phi0=phi0,
        max_n_coalitions=20,
        n_MC_samples=100,
        vaeac_epochs=10,
        vaeac_width=16,
        vaeac_depth=2,
        vaeac_n_vaeacs_initialize=1,
        seed=1,
    )
    explanation_vaeac_cat.print()


if __name__ == "__main__":
    main()
