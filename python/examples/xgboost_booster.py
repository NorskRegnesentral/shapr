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
1           1  2.205938 -0.802031 -0.037126 -0.332701   0.010984   -0.109415   
2           2  2.205938 -0.808813 -0.023685 -0.337045  -0.099915   -0.075275   
3           3  2.205937  0.980612  0.416664 -0.150721   0.043384    0.044734   
4           4  2.205938  0.302170 -0.007116  0.089745   0.131076    0.032540   
5           5  2.205938 -0.436447 -0.029366 -0.005333   0.083110    0.130915   

   AveOccup  Latitude  Longitude  
1 -0.170803 -0.115314  -0.085316  
2 -0.008247  0.044485   0.016533  
3  0.996927 -0.022467   0.380230  
4 -0.357656  0.091057   0.054436  
5  0.021002  0.129993   0.122831  
 """

print(explanation["MSEv"]["MSEv"])

"""
       MSEv   MSEv_sd
1  1.228589  0.809516
"""
