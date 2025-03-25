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
1           1  2.205938 -0.833311 -0.057978 -0.306664   0.029773   -0.069634   
2           2  2.205938 -0.813821 -0.054397 -0.342272  -0.081619   -0.077637   
3           3  2.205937  0.940286  0.441631 -0.201057  -0.027957    0.008278   
4           4  2.205938  0.322364 -0.014792  0.081928   0.129271    0.003449   
5           5  2.205938 -0.405174 -0.012654 -0.015332   0.037333    0.075758   

   AveOccup  Latitude  Longitude  
1 -0.224789 -0.100868  -0.078253  
2 -0.019725  0.069358   0.028153  
3  1.086106  0.018124   0.423951  
4 -0.346083  0.106678   0.053437  
5  0.081340  0.120895   0.134539  
 """

explanation["MSEv"]["MSEv"]

"""
MSEv	MSEv_sd
1	1.252105	0.820416
"""
