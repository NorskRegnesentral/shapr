# Load data -----------
data("Boston", package = "MASS")
df <- tail(Boston, 50)

# Fit linear model
set.seed(123)
model <- lm(medv ~ lstat + rm + dis + indus, data = df)

saveRDS(object = model,"tests/testthat/test_objects/lm_model_object.rds")
