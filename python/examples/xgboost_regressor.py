import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = xgb.XGBRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal, timing, MSEv = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)

""" 
       none    MedInc  HouseAge  AveRooms  AveBedrms  Population  AveOccup  \
1  2.205937 -0.655245  0.079722 -0.096497   0.126559   -0.056841 -0.287298   
2  2.205938 -0.512414  0.077358 -0.504863  -0.176676   -0.005584  0.128635   
3  2.205938  0.510828  0.719958  0.039504  -0.225118    0.044157  1.116464   
4  2.205938  0.381191 -0.098956 -0.022961   0.145486   -0.139457 -0.241768   
5  2.205938 -0.427220 -0.059622 -0.135028   0.158339    0.084157 -0.017783   

   Latitude  Longitude  
1 -0.579708  -0.231051  
2 -0.212670  -0.051049  
3  0.103866   0.405895  
4  0.062271   0.201911  
5  0.078978   0.307480  

 """

MSEv["MSEv"]

"""
MSEv	MSEv_sd
1	0.825758	0.465439
"""
