import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain)

## Shapr
df_shapley, pred_explain, internal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)