import pandas as pd
from sklearn.datasets import fetch_california_housing, fetch_openml, load_iris
from sklearn.model_selection import train_test_split


def load_california_housing() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    housing = fetch_california_housing()
    dfx = pd.DataFrame(housing.data, columns=housing.feature_names)
    dfy = pd.DataFrame({"target": housing.target})
    dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(
        dfx, dfy, test_size=0.99, random_state=42
    )
    dfx_test, dfy_test = dfx_test[:5], dfy_test[:5]  # To reduce computational load
    return dfx_train, dfx_test, dfy_train, dfy_test


def load_binary_iris() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    bcancer = load_iris()
    dfx = pd.DataFrame(bcancer.data, columns=bcancer.feature_names).iloc[
        bcancer.target < 2
    ]  # Turning it into a binary classification problem
    dfy = pd.DataFrame({"target": bcancer.target}).iloc[
        bcancer.target < 2
    ]  # Turning it into a binary classification problem
    dfx_train, dfx_test, dfy_train, dfy_test = train_test_split(
        dfx, dfy, test_size=5, random_state=42
    )
    return dfx_train, dfx_test, dfy_train, dfy_test


def load_adult() -> tuple[pd.DataFrame, pd.DataFrame, pd.DataFrame, pd.DataFrame]:
    dfx, y = fetch_openml(data_id=1590, return_X_y=True)
    dfx = dfx.dropna(axis=1)  # Drop columns with NAs for simplicity
    dfy = pd.DataFrame({"target": y.factorize()[0]})
    return train_test_split(dfx, dfy, test_size=5, random_state=42)
