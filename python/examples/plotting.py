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


# Summary and object extraction

explanation.summary()
explanation.print()

explanation.print("MSEv",digits=5)

explanation.print("MSEv")

explanation.get_results("MSEv")
ttt=explanation.get_results(["MSEv","approach","shapley_est"])
ttt["approach"]
ttt["shapley_est"]

# Plotting

shapExpl = explanation.to_shap()

from shap import plots

plots.bar(shapExpl[3])

# Plots of individual predictions
plots.bar(shapExpl[3])

plots.waterfall(shapExpl[3])

plots.force(shapExpl[3])


# Plots of several predictions similtaneously
plots.beeswarm(shapExpl)

plots.heatmap(shapExpl)

plots.scatter(shapExpl)

plots.violin(shapExpl)

# Decison plot with a slightly differnet style
plots.decision(shapExpl[0].base_values, shapExpl.values,feature_names = shapExpl.feature_names)




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

