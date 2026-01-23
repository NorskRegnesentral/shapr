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

explanation.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population  AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>     <num>
1:          1  2.21 -0.643   0.0634  -0.1189     0.118    -0.0355 -0.237244
2:          2  2.21 -0.533   0.0150  -0.4125    -0.196     0.0276  0.166689
3:          3  2.21  0.314   0.5788  -0.0721    -0.165     0.0602  0.929404
4:          4  2.21  0.315  -0.0703   0.0442     0.125    -0.1292 -0.130029
5:          5  2.21 -0.372  -0.1147  -0.1302     0.164     0.1046 -0.000864
   Latitude Longitude
      <num>     <num>
1:  -0.4371   -0.1974
2:  -0.1789   -0.0753
3:   0.1375    0.2775
4:   0.1200    0.1982
5:   0.0906    0.3284
"""

explanation.print("MSEv")
"""
    MSEv MSEv_sd
   <num>   <num>
1: 0.544   0.257
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
explanation_g.print()


"""
   explain_id  none      A        B      C
        <int> <num>  <num>    <num>  <num>
1:          1  2.21 -0.594 -0.20940 -0.684
2:          2  2.21 -1.228 -0.20620  0.248
3:          3  2.21  0.918  0.65076  0.491
4:          4  2.21  0.206  0.00726  0.259
5:          5  2.21 -0.535 -0.01470  0.621
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

explanation_c_a.print()


"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -1.245  -0.0106  -0.0334  6.28e-06    -0.0408  -0.0187
2:          2  2.21 -0.535  -0.1295  -0.4772  1.03e-02     0.0102   0.0384
3:          3  2.21 -0.294   0.9858   0.6429 -2.72e-03    -0.0744   0.7129
4:          4  2.21  0.625  -0.2654  -0.0213  5.72e-02     0.0222  -0.0595
5:          5  2.21  0.154  -0.2576  -0.0161  1.19e-02     0.0783  -0.0537
   Latitude Longitude
      <num>     <num>
1:  -0.1324  -0.00548
2:  -0.0866  -0.01698
3:   0.0189   0.07052
4:   0.0253   0.08975
5:   0.1212   0.03244
"""
