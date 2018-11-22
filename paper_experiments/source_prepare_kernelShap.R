## Preparing kernelShap


l <- prepare_kernelShap(
    m = ncol(Xtrain),
    Xtrain = Xtrain,
    Xtest = Xtest,
    exact = TRUE,
    distance_metric = "Mahalanobis_scaled",
    normalize_distance_rows = TRUE
)
