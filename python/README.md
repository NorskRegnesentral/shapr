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
import numpy as np
from shaprpy import explain
from sklearn.ensemble import RandomForestRegressor

## Fit model on data X, y
model = RandomForestRegressor()
model.fit(X, y)

## Explain predictions on X_test
df_shapley, pred_explain, internal = explain(
    model = model,
    x_train = X,
    x_explain = X_test,
    approach = 'empirical',
    prediction_zero = np.mean(y),
)
```

This works since `shaprpy` knows how to deal with models from `sklearn` and `xgboost`. 
For other models, one can provide a custom `predict_model` function (and optionally a custom `get_model_specs`) to `shaprpy.explain`.

See `/examples` for runnable examples, including an example of a custom PyTorch model.