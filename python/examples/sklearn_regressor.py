from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = RandomForestRegressor(random_state=0)
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
explanation = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    phi0 = dfy_train.mean().item()
)
print(explanation["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -0.623764  0.062273 -0.111969   0.106106   -0.027083   
2           2  2.205938 -0.538988  0.049095 -0.423751  -0.193274    0.015560   
3           3  2.205938  0.314155  0.577410 -0.074360  -0.166675    0.063638   
4           4  2.205938  0.318181 -0.069231  0.042482   0.124511   -0.122143   
5           5  2.205938 -0.368407 -0.088046 -0.140630   0.154444    0.084880   

   AveOccup  Latitude  Longitude  
1 -0.243124 -0.442461  -0.207026  
2  0.157790 -0.181538  -0.071491  
3  0.921495  0.148821   0.275805  
4 -0.122394  0.107674   0.193703  
5 -0.010989  0.102267   0.336973  
"""

explanation["MSEv"]["MSEv"]
"""
MSEv	MSEv_sd
1	0.556654	0.25165
"""

# Now do this for grouping as well

group = {'A': ['MedInc','HouseAge','AveRooms'],
         'B': ['AveBedrms','Population','AveOccup'],
         'C': ['Latitude','Longitude']}

explanation_g = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    phi0 = dfy_train.mean().item(),
    group = group
)
print(explanation_g["shapley_values_est"])

"""
   explain_id      none         A         B         C
1           1  2.205937 -0.593807 -0.209397 -0.683844
2           2  2.205938 -1.227960 -0.206201  0.247563
3           3  2.205938  0.918459  0.650756  0.491075
4           4  2.205938  0.206152  0.007262  0.259368
5           5  2.205938 -0.535351 -0.014697  0.620540
"""


#### Asymmetric and causal Shapley values

# Assuming a causal ordering (just random in this case)
causal_ordering = {"A": ["MedInc"],
                   "B": ["HouseAge", "AveRooms"],
                   "C": ["AveBedrms", "Population", "AveOccup", "Latitude", "Longitude"]}


explanation_c_a = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    causal_ordering = causal_ordering,
    asymmetric = True,
    phi0 = dfy_train.mean().item()
)
print(explanation_c_a["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -1.245302 -0.013757 -0.036561  -0.000087   -0.032422   
2           2  2.205938 -0.547557 -0.110084 -0.457877   0.000986   -0.016211   
3           3  2.205938 -0.301502  0.993866  0.650967   0.008163   -0.054721   
4           4  2.205938  0.610580 -0.242452  0.001717   0.044329    0.007417   
5           5  2.205938  0.127702 -0.207553  0.033916  -0.006458   -0.010412   

   AveOccup  Latitude  Longitude  
1 -0.016386 -0.134272  -0.008260  
2  0.024030 -0.082117   0.002232  
3  0.731722  0.015783   0.016013  
4 -0.057977  0.032417   0.076751  
5 -0.162505  0.185926   0.109878  
"""