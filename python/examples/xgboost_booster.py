import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain,)

## Shapr
explanation = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'gaussian',
    phi0 = dfy_train.mean().item(),
    seed = 1
)

print(explanation["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -0.800904 -0.048932 -0.331619   0.025193   -0.089941
2           2  2.205938 -0.798797 -0.014390 -0.333489  -0.098005   -0.076619
3           3  2.205937  0.956719  0.454242 -0.186181   0.009467   -0.029593
4           4  2.205938  0.295687 -0.018206  0.098645   0.137201    0.035552
5           5  2.205938 -0.456094 -0.024016  0.003656   0.068857    0.086050

   AveOccup  Latitude  Longitude
1 -0.186362 -0.140054  -0.069105
2 -0.027358  0.043843   0.012852
3  1.060445  0.055664   0.368599
4 -0.368234  0.090013   0.065593
5  0.049725  0.163133   0.125394
 """

print(explanation["MSEv"]["MSEv"])

"""
      MSEv   MSEv_sd
1  1.25269  0.829379
"""
