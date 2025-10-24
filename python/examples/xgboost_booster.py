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

explanation.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.801  -0.0489 -0.33162   0.02519    -0.0899  -0.1864
2:          2  2.21 -0.799  -0.0144 -0.33349  -0.09801    -0.0766  -0.0274
3:          3  2.21  0.957   0.4542 -0.18618   0.00947    -0.0296   1.0604
4:          4  2.21  0.296  -0.0182  0.09864   0.13720     0.0356  -0.3682
5:          5  2.21 -0.456  -0.0240  0.00366   0.06886     0.0860   0.0497
   Latitude Longitude
      <num>     <num>
1:  -0.1401   -0.0691
2:   0.0438    0.0129
3:   0.0557    0.3686
4:   0.0900    0.0656
5:   0.1631    0.1254
 """

explanation.print("MSEv")

"""
    MSEv MSEv_sd
   <num>   <num>
1:  1.25   0.829
"""
