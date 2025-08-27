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
    phi0 = dfy_train.mean().item(),
    seed = 1
)

print(explanation["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -0.642517  0.063359 -0.118928   0.118218   -0.035499
2           2  2.205938 -0.532737  0.015025 -0.412473  -0.196463    0.027573
3           3  2.205938  0.313866  0.578758 -0.072065  -0.164828    0.060197
4           4  2.205938  0.315127 -0.070342  0.044249   0.124803   -0.129249
5           5  2.205938 -0.371743 -0.114729 -0.130228   0.164497    0.104605

   AveOccup  Latitude  Longitude
1 -0.237244 -0.437054  -0.197382
2  0.166689 -0.178902  -0.075310
3  0.929404  0.137453   0.277506
4 -0.130029  0.120018   0.198205
5 -0.000864  0.090562   0.328392
"""

print(explanation["MSEv"]["MSEv"])
"""
      MSEv   MSEv_sd
1  0.54416  0.256793
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
    group = group,
    seed = 1
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
    phi0 = dfy_train.mean().item(),
    seed = 1
)
print(explanation_c_a["shapley_values_est"])

"""
   explain_id      none    MedInc  HouseAge  AveRooms  AveBedrms  Population  \
1           1  2.205938 -1.245496 -0.010636 -0.033439   0.000006   -0.040841
2           2  2.205938 -0.535270 -0.129456 -0.477248   0.010329    0.010191
3           3  2.205938 -0.293531  0.985786  0.642887  -0.002725   -0.074447
4           4  2.205938  0.624581 -0.265422 -0.021254   0.057169    0.022231
5           5  2.205938  0.154202 -0.257612 -0.016143   0.011933    0.078253

   AveOccup  Latitude  Longitude
1 -0.018731 -0.132435  -0.005476
2  0.038423 -0.086585  -0.016982
3  0.712934  0.018863   0.070523
4 -0.059541  0.025263   0.089754
5 -0.053733  0.121151   0.032441
"""