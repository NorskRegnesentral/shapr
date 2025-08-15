import pytest
from sklearn.linear_model import LinearRegression
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
