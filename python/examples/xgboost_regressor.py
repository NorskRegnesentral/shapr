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
1           1  2.205937 -0.656673  0.094587 -0.075444   0.130272   -0.050592   
2           2  2.205938 -0.517043  0.072571 -0.511003  -0.182379    0.035779   
3           3  2.205938  0.542279  0.722088  0.042727  -0.234849    0.042726   
4           4  2.205938  0.354106 -0.094916 -0.013684   0.133045   -0.130711   
5           5  2.205938 -0.416277 -0.027874 -0.152649   0.153961    0.098047   

   AveOccup  Latitude  Longitude  
1 -0.283411 -0.610243  -0.248855  
2  0.107369 -0.200838  -0.061720  
3  1.125337  0.095749   0.379497  
4 -0.228465  0.068373   0.199969  
5 -0.050446  0.060515   0.324025  
"""

explanation["MSEv"]["MSEv"]

"""
MSEv	MSEv_sd
1	0.835836	0.474904
"""
