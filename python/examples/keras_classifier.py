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
print(explanation["shapley_values_est"])

""" 
   explain_id      none  sepal length (cm)  sepal width (cm)  \
1           1  0.494737           0.041518          0.037129   
2           2  0.494737           0.033541          0.028414   
3           3  0.494737           0.045033          0.031092   
4           4  0.494737           0.014281          0.031831   
5           5  0.494737           0.022155          0.025154   

   petal length (cm)  petal width (cm)  
1           0.058252          0.057664  
2           0.044242          0.052839  
3           0.057368          0.069891  
4           0.013667          0.018016  
5           0.026672          0.026181  
 """

# Look at the (overall) MSEv
print(explanation["MSEv"]["MSEv"])

"""
	MSEv	MSEv_sd
1	0.000312	0.00014
"""
