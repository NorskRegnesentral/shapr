# Load data -----------
data("Boston", package = "MASS")
df <- tail(Boston, 50)

# Fit linear model
set.seed(123)
model <- lm(medv ~ lstat + rm + dis + indus, data = df)

saveRDS(object = model, "inst/model_objects/lm_model_object.rds")

# Used for testing as well, so need a copy un the testthat directory
saveRDS(object = model, "tests/testthat/model_objects/lm_model_object.rds")
