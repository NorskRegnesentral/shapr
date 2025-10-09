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
    seed = 1
)


from shaprpy import plot
import shap

# Our function
expl = plot.prep_data(explanation)


# From plots in shap

# Plots of individual predictions
shap.plots.bar(expl[3])

shap.plots.waterfall(expl[3])

# Plots of several predictions similtaneously
shap.plots.beeswarm(expl)

shap.plots.heatmap(expl)

shap.plots.scatter(expl)

shap.plots.violin(expl)

# Maybe skip decision_plot since it uses a different format. In any case, if we only provide the
# conversion function, the user can use the SHAP plotting functions directly.
shap.decision_plot(expl[0].base_values, expl.values)


### Just checking also with shap object directly


# Create SHAP explainer
explainer_shap = shap.KernelExplainer(model.predict, dfx_train)

expl_shap = explainer_shap(dfx_test)

# Plots of individual predictions
shap.plots.bar(expl_shap[3])

shap.plots.waterfall(expl_shap[3])

# Plots of several predictions similtaneously
shap.plots.beeswarm(expl_shap)

shap.plots.heatmap(expl_shap)

shap.plots.scatter(expl_shap)

shap.plots.violin(expl_shap)

