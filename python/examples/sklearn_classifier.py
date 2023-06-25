from sklearn.ensemble import RandomForestClassifier
from shaprpy import explain
from shaprpy.datasets import load_binary_iris

dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

## Fit model
model = RandomForestClassifier(random_state=0)
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal, timing = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)

""" 
       none  sepal length (cm)  sepal width (cm)  petal length (cm)  \
1  0.494737           0.125632          0.127187           0.127187   
2  0.494737           0.061942          0.084664           0.179329   
3  0.494737           0.158276         -0.045561           0.191433   
4  0.494737          -0.167825          0.032064          -0.179700   
5  0.494737          -0.082809         -0.139540          -0.135924   

   petal width (cm)  
1          0.125257  
2          0.179329  
3          0.191115  
4         -0.179275  
5         -0.136463  

 """