import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = xgb.XGBRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
explanation = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    phi0 = dfy_train.mean().item(),
    seed = 1
)
print(explanation["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205937 -0.675157  0.062133 -0.093517   0.129610   -0.040851
2           2  2.205938 -0.526706  0.009479 -0.476060  -0.204358    0.018729
3           3  2.205938  0.509772  0.700983  0.035686  -0.251212    0.071497
4           4  2.205938  0.378471 -0.074383 -0.032628   0.133872   -0.153245
5           5  2.205938 -0.445985 -0.110396 -0.118909   0.179782    0.134424

   AveOccup  Latitude  Longitude
1 -0.289735 -0.569850  -0.222993
2  0.157345 -0.213318  -0.022373
3  1.115800  0.109486   0.423542
4 -0.235451  0.072893   0.198188
5 -0.038472  0.054174   0.334684
"""

print(explanation["MSEv"]["MSEv"])

"""
       MSEv   MSEv_sd
1  0.855251  0.488635
"""
