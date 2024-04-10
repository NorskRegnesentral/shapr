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
df_shapley, pred_explain, internal, timing, MSEv = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)

""" 
       none  sepal length (cm)  sepal width (cm)  petal length (cm)  \
1  0.494737           0.042263          0.037911           0.059232   
2  0.494737           0.034217          0.029183           0.045027   
3  0.494737           0.045776          0.031752           0.058278   
4  0.494737           0.014977          0.032691           0.014280   
5  0.494737           0.022742          0.025851           0.027427   

   petal width (cm)  
1          0.058412  
2          0.053639  
3          0.070650  
4          0.018697  
5          0.026814  

 """

# Look at the (overall) MSEv
MSEv["MSEv"]

"""
	MSEv	MSEv_sd
1	0.000312	0.00014
"""