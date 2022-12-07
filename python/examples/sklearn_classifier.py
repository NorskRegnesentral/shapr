from sklearn.ensemble import RandomForestClassifier
from shaprpy import explain
from shaprpy.utils import load_binary_iris

dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

## Fit model
model = RandomForestClassifier()
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)