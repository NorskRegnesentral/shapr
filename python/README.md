## shaprpy

Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr).

### Install

The below instructions assume you already have R installed, and exposed to your python environment.
Official instructions for installing R can be found her (https://cran.r-project.org/).
R can also be installed with pip as follows:
```
pip install rbase
```
and conda:
```
conda install -c r r
```

#### Install R-package
Install the `shapr` R-package by running the following terminal command from folder of this readme file (`.../shapr/python`):

```
Rscript install_r_packages.R
```

# Install python wrapper
In the folder of this readme file (`.../shapr/python`), run

```
pip install -e .
```

### Demo

```python
from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = RandomForestRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
df_shapley, pred_explain, internal = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)
```


This works since `shaprpy` knows how to deal with models from `sklearn` and `xgboost`. 
For other models, one can provide a custom `predict_model` function (and optionally a custom `get_model_specs`) to `shaprpy.explain`.

See `/examples` for runnable examples, including an example of a custom PyTorch model.