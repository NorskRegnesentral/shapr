from sklearn.ensemble import RandomForestRegressor
from shaprpy import explain
from shaprpy.datasets import load_california_housing

dfx_train, dfx_test, dfy_train, dfy_test = load_california_housing()

## Fit model
model = RandomForestRegressor(random_state=0)
model.fit(dfx_train, dfy_train.values.flatten())

## Shapr
explanation = explain(
    model = model,
    x_train = dfx_train,
    x_explain = dfx_test,
    approach = 'empirical',
    phi0 = dfy_train.mean().item(),
    seed = 1,
    max_n_coalitions = 50
)


#### Summary and object extraction ####

### print() ###
# By default printing the Shapley values from the explanation object (all give the same output)
explanation.print() # default print method
print(explanation)  # __str__ method
explanation # __repr__ method (for interactive environments)

# More sophisticated printing options
explanation.print("MSEv",digits=5)


### summary() ###
# Call summary without assignment - prints formatted output to console
explanation.summary()

# Assign to variable - returns ShaprSummary with summary information for later use
expl_summary = explanation.summary()  # print(expl_summary) provides the formatted output

# Access components from the summary object
expl_summary['shapley_est']  # Estimated Shapley values
expl_summary['timing_summary']['total_time_secs']  # Total computation time
expl_summary['approach']     # Approach used

# Get all available keys in the summary
print("Available summary elements:", list(expl_summary.keys()))


### get_results() ###
# Get all available keys in the results (identical to those in the summary)
print("All available result elements:", list(explanation.get_results().keys()))

# Get results without assignment - returns single result
explanation.get_results("MSEv")

# Assign to variable - returns dict with multiple results for later use
res = explanation.get_results(["MSEv","approach","shapley_est"])

# Access components from the res object
res["approach"]      # Approach used
res["shapley_est"]   # Estimated Shapley values
res["MSEv"]          # MSEv criterion


#### Plotting through the SHAP package (for interactive environments) ####

shapExpl = explanation.to_shap()

from shap import plots

# Plots of individual predictions
plots.bar(shapExpl[3])

plots.waterfall(shapExpl[3])


# Plots of several predictions similtaneously
plots.beeswarm(shapExpl)

plots.heatmap(shapExpl)

plots.scatter(shapExpl)

plots.violin(shapExpl)

# Decision plot with a slightly different style
plots.decision(shapExpl[0].base_values, shapExpl.values, feature_names = shapExpl.feature_names)

# Additional force plot for individual prediction (requires Javascript support)
from shap import initjs
initjs()
plots.force(shapExpl[3])

