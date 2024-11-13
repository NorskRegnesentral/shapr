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
1           1  2.205938 -0.631399  0.073367 -0.120932   0.110256   -0.032183   
2           2  2.205938 -0.533538  0.048795 -0.391921  -0.169092    0.022795   
3           3  2.205938  0.308461  0.601246 -0.094463  -0.181351    0.059955   
4           4  2.205938  0.311945 -0.077067  0.039246   0.140439   -0.129731   
5           5  2.205938 -0.371848 -0.083760 -0.135069   0.143870    0.104284   

   AveOccup  Latitude  Longitude  
1 -0.239760 -0.450027  -0.196370  
2  0.145005 -0.205826  -0.102814  
3  0.928566  0.149806   0.288071  
4 -0.126607  0.115072   0.199486  
5  0.016557  0.082128   0.314332  
"""

explanation["MSEv"]["MSEv"]
"""
	MSEv	MSEv_sd
1	0.53958	0.257247
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
    paired_shap_sampling=False,
    phi0 = dfy_train.mean().item()
)
print(explanation_c_a["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -1.322186 -0.051417  0.035802   0.002919   -0.046796   
2           2  2.205938 -0.464073 -0.093125 -0.639687   0.022725    0.009969   
3           3  2.205938 -0.166641  1.108175  0.867212   0.056946   -0.121676   
4           4  2.205938  0.579213 -0.218696 -0.083284   0.099674    0.094715   
5           5  2.205938  0.059085 -0.214197 -0.000353   0.050200    0.123651   

   AveOccup  Latitude  Longitude  
1 -0.105191 -0.199058  -0.014433  
2 -0.003866 -0.092886   0.003678  
3  0.945082 -0.140175   0.166631  
4 -0.208487 -0.056344   0.080926  
5 -0.198311  0.104367   0.064859  
"""