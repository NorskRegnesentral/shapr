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

explanation.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.675  0.06213  -0.0935     0.130    -0.0409  -0.2897
2:          2  2.21 -0.527  0.00948  -0.4761    -0.204     0.0187   0.1573
3:          3  2.21  0.510  0.70098   0.0357    -0.251     0.0715   1.1158
4:          4  2.21  0.378 -0.07438  -0.0326     0.134    -0.1532  -0.2355
5:          5  2.21 -0.446 -0.11040  -0.1189     0.180     0.1344  -0.0385
   Latitude Longitude
      <num>     <num>
1:  -0.5699   -0.2230
2:  -0.2133   -0.0224
3:   0.1095    0.4235
4:   0.0729    0.1982
5:   0.0542    0.3347
"""

explanation.print("MSEv")

"""
    MSEv MSEv_sd
   <num>   <num>
1: 0.855   0.489
"""
