from keras import Sequential
from keras import layers
from keras import utils
from shaprpy import explain
from shaprpy.datasets import load_binary_iris


dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

utils.set_random_seed(1)

## Build model
model = Sequential([
    layers.Dense(units=8, activation='relu'),
    layers.Dense(units=16, activation='relu'),
    layers.Dense(units=8, activation='relu'),
    layers.Dense(units=1, activation='sigmoid')
])
model.compile(optimizer="adam",
              loss ="binary_crossentropy",
              metrics=["accuracy"])

## Fit Model
model.fit(dfx_train, dfy_train,
          epochs=10,
          validation_data=(dfx_test, dfy_test))
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
1:          1 0.495            0.0415           0.0371            0.0583
2:          2 0.495            0.0335           0.0284            0.0442
3:          3 0.495            0.0450           0.0311            0.0574
4:          4 0.495            0.0143           0.0318            0.0137
5:          5 0.495            0.0222           0.0252            0.0267
   petal width (cm)
              <num>
1:           0.0577
2:           0.0528
3:           0.0699
4:           0.0180
5:           0.0262
 """

# Look at the (overall) MSEv
explanation.print("MSEv")

"""
       MSEv MSEv_sd
      <num>   <num>
1: 0.000312 0.00014
"""
