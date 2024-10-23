import xgboost as xgb
import pandas as pd
from shaprpy import explain


# Read data  
x_train = pd.read_csv("inst/code_paper/x_train.csv")
y_train = pd.read_csv("inst/code_paper/y_train.csv")

x_explain = pd.read_csv("inst/code_paper/x_explain.csv")
y_explain = pd.read_csv("inst/code_paper/y_explain.csv")

# Load the XGBoost model from the raw format
model = xgb.Booster()
model.load_model("inst/code_paper/xgb.model")

# Add feature names to the model
model.feature_names = x_train.columns.tolist()

phi0 = y_train.mean().item()

## Shapr

exp_full_emp_head = explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain.head(),
    approach = 'empirical',
    prediction_zero = phi0,
    iterative = False)

print(exp_full_emp_head['shapley_values_est'])

# Print the keys of the exp_full_emp_head dictionary
print(exp_full_emp_head.keys())





