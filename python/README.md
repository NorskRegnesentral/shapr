## shaprpy

Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr).

NOTE: This wrapper is in an EXPERIMENTAL state. Bugs and breaking changes are not unlikely to occur.

### Install

The below instructions assume you already have `pip` and `R` installed and exposed to the python environment where you want to run `shaprpy`. 
Official instructions for installing `pip` can be found [here](https://pip.pypa.io/en/stable/installation/), and for `R` [here](https://cran.r-project.org/).
R can also be installed with pip as follows:
```
pip install rbase
```
and conda:
```
conda install -c r r
```

#### Install R-package
The `shaprpy` Python wrapper requires the development version of the `shapr` R-package (from the master branch on GitHub).
Install it by running the following terminal command from the folder of this readme file (`.../shapr/python`):

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
df_shapley, pred_explain, internal, timing = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    prediction_zero = dfy_train.mean().item(),
)
print(df_shapley)
```

`shaprpy` knows how to explain predictions from models from `sklearn`, `keras` and `xgboost`. 
For other models, one can provide a custom `predict_model` function (and optionally a custom `get_model_specs`) to `shaprpy.explain`.

See `/examples` for runnable examples, including an example of a custom PyTorch model.

The `/examples/regression_paradigm.py` file demonstrates how
to use the regression paradigm explained in 
[Olsen et al. (2024)](https://link.springer.com/article/10.1007/s10618-024-01016-z).
We describe how to specify the regression model, how to enable automatic
cross-validation of the model's hyperparameters, and applying
pre-processing steps to the data before fitting the regression
models. We refer to 
[Olsen et al. (2024)](https://link.springer.com/article/10.1007/s10618-024-01016-z)
for when one should  use the different paradigms, method classes, and methods.

