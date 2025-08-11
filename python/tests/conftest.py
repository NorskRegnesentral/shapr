import pytest
# import pandas as pd
# from sklearn.datasets import make_classification, make_regression
# from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
# from sklearn.ensemble import RandomForestClassifier, RandomForestRegressor
import xgboost as xgb
from shaprpy.datasets import load_california_housing, load_binary_iris

@pytest.fixture(scope="session")
def house_data():
    dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()
    return dfx_train, dfx_test, dfy_train, dfy_test

@pytest.fixture(scope="session")
def iris_data():
    dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()
    return dfx_train, dfx_test, dfy_train, dfy_test

@pytest.fixture(scope="session")
def model_xgb(house_data):
    dfx_train, _, dfy_train, _ = house_data

    model = xgb.XGBRegressor()
    model.fit(dfx_train, dfy_train.values.flatten())

    return model

@pytest.fixture(scope="session")
def model_lm(iris_data):
    dfx_train, _, dfy_train, _ = iris_data

    model = LinearRegression()
    model.fit(dfx_train, dfy_train.squeeze())
    return model

# @pytest.fixture(scope="session")
# def xtrain_binary(simulated_classification_data):
#     return simulated_classification_data[0]

# @pytest.fixture(scope="session")
# def xtest_binary(simulated_classification_data):
#     return simulated_classification_data[1]

# @pytest.fixture(scope="session")
# def ytrain_binary(simulated_classification_data):
#     return simulated_classification_data[2]

# @pytest.fixture(scope="session")
# def ytest_binary(simulated_classification_data):
#     return simulated_classification_data[3]

# @pytest.fixture(scope="session")
# def xtrain_numeric(simulated_regression_data):
#     return simulated_regression_data[0]

# @pytest.fixture(scope="session")
# def xtest_numeric(simulated_regression_data):
#     return simulated_regression_data[1]

# @pytest.fixture(scope="session")
# def ytrain_numeric(simulated_regression_data):
#     return simulated_regression_data[2]

# @pytest.fixture(scope="session")
# def ytest_numeric(simulated_regression_data):
#     return simulated_regression_data[3]

# @pytest.fixture(scope="session")
# def model_rf_regressor(xtrain_numeric, ytrain_numeric):
#     model = RandomForestRegressor(random_state=42)
#     model.fit(xtrain_numeric, ytrain_numeric)
#     return model

# @pytest.fixture(scope="session")
# def model_xgb_regressor(xtrain_numeric, ytrain_numeric):
#     model = xgb.XGBRegressor(random_state=42)
#     model.fit(xtrain_numeric, ytrain_numeric)
#     return model

# @pytest.fixture(scope="session")
# def model_logistic(xtrain_binary, ytrain_binary):
#     model = LogisticRegression(max_iter=200)
#     model.fit(xtrain_binary, ytrain_binary)
#     return model

# @pytest.fixture(scope="session")
# def model_rf_classifier(xtrain_binary, ytrain_binary):
#     model = RandomForestClassifier(random_state=42)
#     model.fit(xtrain_binary, ytrain_binary)
#     return model


# @pytest.fixture(scope="session")
# def simulated_classification_data():
#     X, y = make_classification(
#         n_samples=100,
#         n_features=5,
#         n_informative=3,
#         n_redundant=0,
#         n_classes=2,
#         random_state=42
#     )
#     columns = [f"feature{i}" for i in range(X.shape[1])]
#     xtrain, xtest, ytrain, ytest = train_test_split(
#         pd.DataFrame(X, columns=columns),
#         pd.Series(y, name="target_binary"),
#         test_size=0.2,
#         random_state=42
#     )
#     return xtrain, xtest, ytrain, ytest

# @pytest.fixture(scope="session")
# def simulated_regression_data():
#     X, y = make_regression(
#         n_samples=100,
#         n_features=5,
#         n_informative=3,
#         noise=0.1,
#         random_state=42
#     )
#     columns = [f"feature{i}" for i in range(X.shape[1])]
#     xtrain, xtest, ytrain, ytest = train_test_split(
#         pd.DataFrame(X, columns=columns),
#         pd.Series(y, name="target_numeric"),
#         test_size=0.2,
#         random_state=42
#     )
#     return xtrain, xtest, ytrain, ytest