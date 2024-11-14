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
)
print(explanation["shapley_values_est"])

""" 
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205937 -0.644869  0.092354 -0.105347   0.122196   -0.054501   
2           2  2.205938 -0.509659  0.087719 -0.503114  -0.160219    0.008278   
3           3  2.205938  0.511284  0.744960  0.036120  -0.235612    0.055126   
4           4  2.205938  0.389474 -0.104019 -0.026614   0.160248   -0.143182   
5           5  2.205938 -0.425561 -0.049056 -0.151081   0.153844    0.088019   

   AveOccup  Latitude  Longitude  
1 -0.289824 -0.584639  -0.235730  
2  0.119529 -0.234297  -0.065501  
3  1.095101  0.106010   0.402564  
4 -0.244692  0.053750   0.202752  
5  0.009034  0.057433   0.306669  
"""

explanation["MSEv"]["MSEv"]

"""
MSEv	MSEv_sd
1	0.838435	0.483059
"""
