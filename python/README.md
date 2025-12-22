# shaprpy

`shaprpy` is a Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr),
using the [`rpy2`](https://rpy2.github.io/) Python library to access R from within Python.

> **Note:** This wrapper is **not** as comprehensively tested as the R package.
> `rpy2` has limited support on Windows, and the same therefore applies to `shaprpy`.
> `shaprpy` has only been tested on Linux (and WSL - Windows Subsystem for Linux), and the below instructions assume a Linux environment.
>
> **Requirement:** Python 3.10 or later is required to use `shaprpy`.

## Changelog

For a list of changes and updates to the `shaprpy` package, see the [shaprpy CHANGELOG](https://norskregnesentral.github.io/shapr/py_changelog.html).

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

Sometimes `rpy2` (which `shaprpy` relies on) cannot automatically locate your R installation. To ensure proper detection, verify that:

- R is available in your system `PATH`, **or**
- The `R_HOME` environment variable is set to your R installation directory.

Example:
```bash
export R_HOME=$(R RHOME)
export PATH=$PATH:$(R RHOME)/bin
```

### 3. Install the Python wrapper

Install directly from PyPI with:

```bash
pip install shaprpy
```

#### Local development install (for contributors)
If you have cloned the repository and want to install in development mode for local changes, navigate to the `./python` directory and run:
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

explanation.print() # Print the Shapley values

# Get a summary object with computation details
summary = explanation.summary()
print(summary)  # Displays a formatted summary (also available directly via explanation.summary())

# Access specific summary attributes (available with tab-completion in Jupyter)
summary['approach']     # Approach used
summary['timing_summary']['total_time_secs']  # Total computation time

# Extract one or more specific result objects directly
explanation.get_results("proglang") # Programming language used (Python/R)
explanation.get_results("approach") # Approach used
explanation.get_results().keys()  # All available result objects

# Plotting (requires the 'shap' library)
# Convert to a SHAP Explanation object
shap_exp = explanation.to_shap()

import shap
shap.plots.waterfall(shap_exp[0]) # Plot the first observation

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

See the [examples folder](https://github.com/NorskRegnesentral/shapr/tree/master/python/examples) folder on GitHub for runnable examples, including:

- Basic usage with `scikit-learn` models
- Usage with `xgboost` models
- Usage with `keras` models
- A custom PyTorch model
- Usage of the `Shapr` class and associated `ShaprSummary` class for exploration and extraction of explanation results.
- Plotting functionality for the Shapley values through the `shap` package
- The **regression paradigm** described in [Olsen et al. (2024)](https://link.springer.com/article/10.1007/s10618-024-01016-z),
  which shows:
  - How to specify the regression model
  - How to enable automatic cross-validation of hyperparameters
  - How to apply pre-processing steps before fitting regression models
