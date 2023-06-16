from keras import Sequential
from keras import layers
from shaprpy import explain
from shaprpy.datasets import load_binary_iris

dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

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
df_shapley, pred_explain, internal, timing = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)
