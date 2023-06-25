import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = xgb.XGBRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal, timing = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)

""" 
       none    MedInc  HouseAge  AveRooms  AveBedrms  Population  AveOccup  \
1  2.205937 -0.697653  0.103323 -0.066003   0.115853   -0.057640 -0.292739   
2  2.205938 -0.521995  0.064876 -0.445466  -0.230454   -0.019290  0.080655   
3  2.205938  0.307681  0.563008  0.062743  -0.626912    0.050450  1.050069   
4  2.205938  0.479900 -0.100035  0.030474   0.104301   -0.154396 -0.148057   
5  2.205938 -0.088568 -0.101495 -0.121637   0.213535    0.169194  0.253711   

   Latitude  Longitude  
1 -0.573533  -0.237709  
2 -0.265165  -0.090790  
3  0.181936   0.412604  
4  0.078605   0.186957  
5  0.126759   0.376471  

 """