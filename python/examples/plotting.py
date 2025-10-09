import shaprpy
from sklearn.ensemble import RandomForestClassifier
from shaprpy import explain
from shaprpy.datasets import load_binary_iris

dfx_train, dfx_test, dfy_train, dfy_test = load_binary_iris()

## Fit model
model = RandomForestClassifier(random_state=0)
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
print(explanation["shapley_values_est"])

"""
   explain_id      none  sepal length (cm)  sepal width (cm)  \
1           1  0.494737           0.125632          0.127187
2           2  0.494737           0.061942          0.084664
3           3  0.494737           0.158276         -0.045561
4           4  0.494737          -0.167825          0.032064
5           5  0.494737          -0.082809         -0.139540

   petal length (cm)  petal width (cm)
1           0.127187          0.125257
2           0.179329          0.179329
3           0.191433          0.191115
4          -0.179700         -0.179275
5          -0.135924         -0.136463

 """

from shaprpy import plot
import shap


plot.plot(explanation)

plot.plot(explanation, plot_type="beeswarm",idx=slice(2))

plot.plot(explanation, plot_type="waterfall")

plot.plot(explanation, plot_type="violin", idx=2)

plot.plot(explanation, plot_type="scatter")

plot.plot(explanation, plot_type="heatmap")

expl = plot.prep_data(explanation)

# From plots.

shap.plots.bar(expl[2])

shap.plots.beeswarm(expl)

shap.decision_plot(expl[0].base_values, expl.values) # Maybe skip since it uses a different format?

shap.plots.heatmap(expl)

shap.plots.scatter(expl)

shap.plots.violin(expl)

shap.plots.waterfall(expl[2])


import shap
from shap import KernelExplainer

# Create SHAP explainer
explainer = KernelExplainer(model.predict_proba, dfx_train)
shap_values = explainer.shap_values(dfx_test)

shap.force_plot(explainer.expected_value[0], shap_values[..., 0], dfx_test)

shap.violin_plot(shap_values[..., 0])

# Plot SHAP results
shap.summary_plot(explainer, dfx_test)

shap.plots.bar(shap_values[1])

"bar", "beeswarm", "heatmap", "scatter", "violin", "waterfall"


plot.waterfall(explanation, index=1)
