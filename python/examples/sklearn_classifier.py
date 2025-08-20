from sklearn.ensemble import RandomForestClassifier
from shaprpy import explain
from shaprpy.datasets import load_binary_iris

dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

## Fit model
model = RandomForestClassifier(random_state=0)
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
   explain_id      none  sepal length (cm)  sepal width (cm)  \
1           1  0.494737           0.125632          0.127187
2           2  0.494737           0.061942          0.084664
3           3  0.494737           0.158276         -0.045561
4           4  0.494737          -0.167825          0.032064
5           5  0.494737          -0.082809         -0.139540

   petal length (cm)  petal width (cm)
1           0.127187          0.125257
2           0.179329          0.179329
3           0.191433          0.191115
4          -0.179700         -0.179275
5          -0.135924         -0.136463

 """
