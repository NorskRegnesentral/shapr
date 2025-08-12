
# shaprpy

Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr).

NOTE: This wrapper is not as comprehensively tested as the `R`-package.

`shaprpy` relies heavily on the `rpy2` Python library for accessing R from within Python.
`rpy2` has limited support on Windows. `shaprpy` has only been tested on Linux.
The below instructions assumes a Linux environment.

# shaprpy

`shaprpy` is a Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr),
using the [`rpy2`](https://rpy2.github.io/) Python library to access R from within Python.

> **Note:** This wrapper is **not** as comprehensively tested as the R package.
> `rpy2` has limited support on Windows, and the same therefore applies to `shaprpy`.
> `shaprpy` has only been tested on Linux, and the below instructions assume a Linux environment.

---

## Installation

These instructions assume you already have **pip** and **R** installed and available to the Python environment in which you want to run `shaprpy`.

- Official instructions for installing `pip` can be found [here](https://pip.pypa.io/en/stable/installation/).
- Official instructions for installing R can be found [here](https://cran.r-project.org/).

On Debian/Ubuntu-based systems, R can also be installed via:
```bash
sudo apt update
sudo apt install r-base r-base-dev -y
```

### 1. Install the R package `shapr`

`shaprpy` requires the R package `shapr` (version 1.0.5 or newer).
In your R environment, install the latest version from CRAN using:

```bash
Rscript -e 'install.packages("shapr", repos="https://cran.rstudio.com")'
```

### 2. Ensure R is discoverable (R_HOME and PATH)

If `shaprpy` cannot find your R installation, ensure that:
- R is on your system `PATH`, **or**
- The `R_HOME` environment variable is set to your R installation directory.

Example:
```bash
export R_HOME=$(R RHOME)
export PATH=$PATH:$(R RHOME)/bin
```

### 3. Install the Python wrapper

To install from **TestPyPI** (for testing the package before the official release), run:

```bash
python -m pip install -i https://test.pypi.org/simple/ --extra-index-url https://pypi.org/simple shaprpy
```

> The `--extra-index-url` flag ensures that dependencies (such as `rpy2`, `numpy`, etc.) are installed from the main PyPI repository.

Once `shaprpy` is available on the official PyPI, you will be able to install it directly with:
```bash
pip install shaprpy
```

#### Local development install (for contributors)
If you have cloned the repository and want to install in editable mode:
```bash
pip install -e .
```
The `-e` flag installs in editable mode, allowing local code changes to be reflected immediately.

---

## Quick Demo

```python
from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing

# Load example data
dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

# Fit a model
model = RandomForestRegressor()
model.fit(dfx_train, dfy_train.values.flatten())

# Explain predictions
explanation = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_test,
    approach="empirical",
    phi0=dfy_train.mean().item(),
    seed=1
)

print(explanation["shapley_values_est"])
```

---

## Supported Models

`shaprpy` can explain predictions from models built with:
- [`scikit-learn`](https://scikit-learn.org/)
- [`keras`](https://keras.io/) (Sequential API)
- [`xgboost`](https://xgboost.readthedocs.io/)

For other model types, you can supply:
- A custom `predict_model` function
- (Optionally) a custom `get_model_specs` function
to `shaprpy.explain`.

---

## Examples

See the `/examples` folder for runnable examples, including:
- A custom PyTorch model
- The **regression paradigm** described in [Olsen et al. (2024)](https://link.springer.com/article/10.1007/s10618-024-01016-z),
  which shows:
  - How to specify the regression model
  - How to enable automatic cross-validation of hyperparameters
  - How to apply pre-processing steps before fitting regression models
