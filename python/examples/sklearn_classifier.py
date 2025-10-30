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

explanation.print()


"""
   explain_id  none sepal length (cm) sepal width (cm) petal length (cm)
        <int> <num>             <num>            <num>             <num>
1:          1 0.495            0.1256           0.1272             0.127
2:          2 0.495            0.0619           0.0847             0.179
3:          3 0.495            0.1583          -0.0456             0.191
4:          4 0.495           -0.1678           0.0321            -0.180
5:          5 0.495           -0.0828          -0.1395            -0.136
   petal width (cm)
              <num>
1:            0.125
2:            0.179
3:            0.191
4:           -0.179
5:           -0.136
 """
