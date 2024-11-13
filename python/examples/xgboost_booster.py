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
)
print(explanation["shapley_values_est"])

""" 
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -0.832465 -0.053984 -0.311050   0.054333   -0.078014   
2           2  2.205938 -0.813781 -0.053652 -0.356067  -0.081677   -0.076472   
3           3  2.205937  0.965412  0.407928 -0.209353  -0.051755   -0.006602   
4           4  2.205938  0.297996 -0.040097  0.092800   0.132102    0.046493   
...
2 -0.025957  0.074273   0.041371  
3  1.077735  0.058107   0.447890  
4 -0.358188  0.094037   0.071108  
5  0.078736  0.139113   0.182091  
 """

explanation["MSEv"]["MSEv"]

"""
MSEv	MSEv_sd
1	1.25449	0.831646
"""
