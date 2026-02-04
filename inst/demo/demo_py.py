# %% Demo of shaprpy

import os
os.chdir("inst/demo")


import xgboost as xgb
import pandas as pd
from shaprpy import explain

## %

# Read data
x_train = pd.read_csv("data_and_models/" + "x_train.csv")
x_explain = pd.read_csv("data_and_models/" + "x_explain.csv")
y_train = pd.read_csv("data_and_models/" + "y_train.csv")

# Load the XGBoost model from the raw format and add feature names
model = xgb.Booster()
model.load_model("data_and_models/" + "model.ubj")
model.feature_names = x_train.columns.tolist()

# Predict x_explain using the XGBoost model
expl_empirical_40 = explain(model = model,
                            x_train = x_train,
                            x_explain = x_explain,
                            approach = "empirical",
                            phi0 = y_train.mean().item(),
                            max_n_coalitions = 40,
                            iterative = False,
                            seed = 123)


# Print the Shapley values
expl_empirical_40

expl_empirical_40.summary()

expl_summary = expl_empirical_40.summary()
# Access components from the summary object
expl_summary['shapley_sd']  # Estimated Shapley values
expl_summary['timing_summary']['total_time_secs']  # Total computation time
expl_summary['approach'][0]     # Approach used


# Print the MSE of the v(S)
expl_empirical_40.print(what = "MSEv")

# Display a "force plot" of observation eight using the shap package
from shap import plots as shap_plt
import matplotlib.pyplot as plt

shapExpl = expl_empirical_40.to_shap() # Convert to shap's object class
shap_plt.force(shapExpl[7], matplotlib=True) # Display plot

# Plots of individual predictions
shap_plt.bar(shapExpl[7])

shap_plt.waterfall(shapExpl[7])

# Plots of several predictions similtaneously
shap_plt.beeswarm(shapExpl)

shap_plt.heatmap(shapExpl)

shap_plt.scatter(shapExpl)

shap_plt.violin(shapExpl)

#### Regression paradigm examples

expl_sep_xgboost = explain(
    model = model,
    x_train = x_train,
    x_explain = x_explain,
    approach = 'regression_separate',
    regression_model = "parsnip::boost_tree(engine = 'xgboost', mode = 'regression')",
    phi0 = y_train.mean().item(),
    max_n_coalitions = 70,
    iterative = True,
    verbose = ["basic", "convergence"],
    seed = 123)

expl_sep_xgboost

expl_sep_xgboost.summary()

shapExpl_reg = expl_sep_xgboost.to_shap() # Convert to shap's object class
shap_plt.scatter(shapExpl_reg)
