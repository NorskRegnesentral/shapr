import xgboost as xgb
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
dtrain = xgb.DMatrix(data=dfx_train, label=dfy_train)
model = xgb.train(params={}, num_boost_round=20, dtrain=dtrain,)

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
1  2.205937 -0.701774  0.105231 -0.062693   0.119937   -0.054787 -0.296760   
2  2.205938 -0.518543  0.062408 -0.447457  -0.232334   -0.015201  0.080512   
3  2.205938  0.313422  0.558150  0.064590  -0.603992    0.052804  1.035786   
4  2.205938  0.473781 -0.093358  0.046108   0.105684   -0.153729 -0.151584   
5  2.205938 -0.084333 -0.099879 -0.119632   0.217170    0.162796  0.253543   

   Latitude  Longitude  
1 -0.553604  -0.223937  
2 -0.258331  -0.097497  
3  0.175916   0.410554  
4  0.080829   0.187370  
5  0.129439   0.382075  

 """