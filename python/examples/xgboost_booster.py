import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain,)

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
1  2.205937 -0.660639  0.085530 -0.103341   0.123983   -0.056817 -0.282401   
2  2.205938 -0.521214  0.073131 -0.506383  -0.184693   -0.011323  0.133417   
3  2.205938  0.513277  0.715094  0.044417  -0.220822    0.049277  1.096243   
4  2.205938  0.382929 -0.092198 -0.016058   0.150373   -0.137098 -0.260570   
5  2.205938 -0.424637 -0.060884 -0.136637   0.153806    0.100997 -0.020819   

   Latitude  Longitude  
1 -0.525114  -0.222924  
2 -0.211306  -0.063592  
3  0.099492   0.392385  
4  0.101910   0.206964  
5  0.090441   0.314439  
 """

MSEv["MSEv"]

"""
MSEv	MSEv_sd
1	0.815449	0.459069
"""
