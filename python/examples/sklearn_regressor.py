from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = RandomForestRegressor(random_state=0)
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal, timing, MSEv = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item()
)
print(df_shapley)

"""
       none    MedInc  HouseAge  AveRooms  AveBedrms  Population  AveOccup  \
1  2.205938 -0.633466  0.071652 -0.120805   0.121186   -0.034587 -0.241660   
2  2.205938 -0.529888  0.048025 -0.414597  -0.184992    0.021479  0.152382   
3  2.205938  0.303072  0.578311 -0.087687  -0.160909    0.055895  0.932739   
4  2.205938  0.318539 -0.079595  0.033414   0.132047   -0.129203 -0.124437   
5  2.205938 -0.369314 -0.094776 -0.136746   0.150069    0.090628  0.015991   

   Latitude  Longitude  
1 -0.448458  -0.200910  
2 -0.194464  -0.084543  
3  0.147927   0.290942  
4  0.118805   0.203213  
5  0.099410   0.315230   
"""

MSEv["MSEv"]
"""
MSEv	MSEv_sd
1	0.534141	0.247984
"""

# Now do this for grouping as well

group = {'A': ['MedInc','HouseAge','AveRooms'],
         'B': ['AveBedrms','Population','AveOccup'],
         'C': ['Latitude','Longitude']}

df_shapley_g, pred_explain_g, internal_g, timing_g, MSEv_g = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
    group = group
)
print(df_shapley_g)

"""
       none         A         B         C
1  2.205937 -0.593807 -0.209397 -0.683844
2  2.205938 -1.227960 -0.206201  0.247563
3  2.205938  0.918459  0.650756  0.491075
4  2.205938  0.206152  0.007262  0.259368
5  2.205938 -0.535351 -0.014697  0.620540
"""
