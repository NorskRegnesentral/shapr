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
1           1  2.205937 -0.653026  0.117386 -0.078029   0.100776   -0.060984   
2           2  2.205938 -0.441987  0.009181 -0.459548  -0.192894   -0.008709   
3           3  2.205938  0.481492  0.759036  0.038105  -0.298989    0.076037   
4           4  2.205938  0.399358 -0.099609 -0.041793   0.160021   -0.156963   
5           5  2.205938 -0.369152 -0.088905 -0.094943   0.160681    0.117488   

   AveOccup  Latitude  Longitude  
1 -0.310839 -0.576468  -0.239177  
2  0.093947 -0.230720  -0.026534  
3  1.120256  0.126024   0.413593  
4 -0.219880  0.071685   0.174899  
5 -0.071980  0.047893   0.288219  
"""

print(explanation["MSEv"]["MSEv"])

"""
       MSEv   MSEv_sd
1  0.847489  0.508072
"""
