import pandas as pd
from sklearn.compose import ColumnTransformer
from sklearn.ensemble import RandomForestRegressor
from sklearn.pipeline import Pipeline
from sklearn.preprocessing import OneHotEncoder

from pyshapr import explain
from pyshapr.datasets import load_california_housing

dfx_train, dfx_explain, dfy_train, dfy_explain = load_california_housing()
dfx_train = dfx_train.iloc[:600].copy()
dfx_explain = dfx_explain.iloc[:3].copy()
dfy_train = dfy_train.iloc[:600].copy()

## Fit model
model = RandomForestRegressor(random_state=0)
model.fit(dfx_train, dfy_train.values.flatten())

## ARF approach (numeric features)
explanation_arf = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_explain,
    approach="arf",
    phi0=dfy_train.mean().item(),
    n_MC_samples=200,
    arf_num_trees=20,
    arf_max_iters=5,
    max_n_coalitions=20,
    seed=1,
)

explanation_arf.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.845   -0.119   -0.176    0.0479   -0.11500   -0.274
2:          2  2.21 -0.655    0.129   -0.492   -0.1662   -0.00546    0.117
3:          3  2.21  0.417    0.611    0.241   -0.8324   -0.07894    0.787
   Latitude Longitude
      <num>     <num>
1:  -0.0182    0.0138
2:  -0.0792   -0.0352
3:  -0.0436    0.9592
"""

## VAEAC approach (numeric features)
# Requires the R package torch (install.packages("torch"); torch::install_torch())
explanation_vaeac = explain(
    model=model,
    x_train=dfx_train,
    x_explain=dfx_explain,
    approach="vaeac",
    phi0=dfy_train.mean().item(),
    n_MC_samples=100,
    vaeac_epochs=10,
    vaeac_width=16,
    vaeac_depth=2,
    vaeac_n_vaeacs_initialize=1,
    max_n_coalitions=20,
    seed=1,
)

explanation_vaeac.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -1.308   0.0600   0.0209    0.0436    -0.1320  -0.0156
2:          2  2.21 -1.015  -0.0114  -0.2196    0.0784     0.0269  -0.0300
3:          3  2.21  0.197   0.7837   0.0484   -0.0974    -0.0794   1.1205
   Latitude Longitude
      <num>     <num>
1:  -0.1211   -0.0342
2:  -0.0869    0.0713
3:  -0.0570    0.1448
"""

## ARF and VAEAC also support categorical (factor) features.
# Add three categorical columns derived from existing numeric ones.
for df in [dfx_train, dfx_explain]:
    df["IncomeCategory"] = pd.cut(
        df["MedInc"], bins=[0, 3, 6, 15], labels=["Low", "Medium", "High"]
    )
    df["AgeCategory"] = pd.cut(df["HouseAge"], bins=[0, 15, 35, 60], labels=["New", "Mid", "Old"])
    df["LocationType"] = pd.cut(
        df["Latitude"], bins=[32, 34, 37, 42], labels=["South", "Central", "North"]
    )

# Ensure consistent categorical levels and convert to pandas Categorical
for col in ["IncomeCategory", "AgeCategory", "LocationType"]:
    for df in [dfx_train, dfx_explain]:
        df[col] = df[col].cat.add_categories(["Unknown"]).fillna("Unknown").astype(str)
    cats = [
        c for c in pd.unique(pd.concat([dfx_train[col], dfx_explain[col]]).values) if pd.notna(c)
    ]
    dfx_train[col] = pd.Categorical(dfx_train[col], categories=cats)
    dfx_explain[col] = pd.Categorical(dfx_explain[col], categories=cats)

# Build a model that handles the mixed feature types
numeric_features = [
    "MedInc",
    "HouseAge",
    "AveRooms",
    "AveBedrms",
    "Population",
    "AveOccup",
    "Latitude",
    "Longitude",
]
categorical_features = ["IncomeCategory", "AgeCategory", "LocationType"]

model_mixed = Pipeline(
    steps=[
        (
            "pre",
            ColumnTransformer(
                [
                    ("num", "passthrough", numeric_features),
                    (
                        "cat",
                        OneHotEncoder(handle_unknown="ignore", sparse_output=False),
                        categorical_features,
                    ),
                ]
            ),
        ),
        ("rf", RandomForestRegressor(random_state=0)),
    ]
)
model_mixed.fit(dfx_train, dfy_train.values.flatten())

## ARF with mixed features
explanation_arf_mixed = explain(
    model=model_mixed,
    x_train=dfx_train,
    x_explain=dfx_explain,
    approach="arf",
    phi0=dfy_train.mean().item(),
    n_MC_samples=200,
    arf_num_trees=20,
    arf_max_iters=5,
    max_n_coalitions=20,
    seed=1,
)

explanation_arf_mixed.print()

"""
   explain_id  none MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>  <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.358  -0.0344   -0.150    0.0448    -0.2492  -0.0689
2:          2  2.21 -0.332   0.1954   -0.204   -0.1338    -0.0682   0.1023
3:          3  2.21 -0.276   0.3038   -0.898    0.5820    -0.7012   0.9768
   Latitude Longitude IncomeCategory AgeCategory LocationType
      <num>     <num>          <num>       <num>        <num>
1:   -0.223   -0.0104         -0.358     -0.0254      -0.0769
2:   -0.181   -0.0632         -0.332     -0.0708      -0.1023
3:   -0.164   -0.0160         -0.276      1.5607       0.9812
"""

## VAEAC with mixed features
explanation_vaeac_mixed = explain(
    model=model_mixed,
    x_train=dfx_train,
    x_explain=dfx_explain,
    approach="vaeac",
    phi0=dfy_train.mean().item(),
    n_MC_samples=100,
    vaeac_epochs=10,
    vaeac_width=16,
    vaeac_depth=2,
    vaeac_n_vaeacs_initialize=1,
    max_n_coalitions=20,
    seed=1,
)

explanation_vaeac_mixed.print()

"""
   explain_id  none   MedInc HouseAge AveRooms AveBedrms Population AveOccup
        <int> <num>    <num>    <num>    <num>     <num>      <num>    <num>
1:          1  2.21 -0.59851    0.139   0.1018   0.13597     -0.287   -0.131
2:          2  2.21 -0.40255    0.077  -0.0298  -0.01953     -0.216   -0.023
3:          3  2.21 -0.00262    0.467   0.0270   0.00472      0.251    0.714
   Latitude Longitude IncomeCategory AgeCategory LocationType
      <num>     <num>          <num>       <num>        <num>
1:   -0.259   -0.0344       -0.59851     0.00404       0.0189
2:   -0.171   -0.0742       -0.40255    -0.00521       0.0783
3:    0.270    0.1988       -0.00262     0.16048      -0.0158
"""
