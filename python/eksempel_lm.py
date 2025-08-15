import sklearn 
# from sklearn.linear_model import LinearRegression
from shaprpy.explain import explain
from shaprpy.datasets import load_california_housing, load_binary_iris
import xgboost as xgb

# dfx_train, dfx_test, dfy_train, _ = load_binary_iris()

# model = LinearRegression()
# model.fit(dfx_train, dfy_train)

# print(sklearn.__version__)
# print(model.predict(dfx_test))
# print(type(model.predict(dfx_test)))

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain)

phi0 = float(dfy_train.mean().item())

explanation = explain(
    testing = True,
    model=model,
    x_train=dfx_train,
     x_explain=dfx_test,
    approach="empirical",
    phi0=phi0, 
    seed = 42
)