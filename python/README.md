## shaprpy

Python wrapper for the R package [shapr](https://github.com/NorskRegnesentral/shapr).

### Install

In the folder containing `setup.py`, run
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