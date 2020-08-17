# Load data -----------
data("Boston", package = "MASS")

Boston$rad <- as.factor(Boston$rad)
Boston$chas <- as.factor(Boston$chas)

df <- tail(Boston, 350)

# Fit linear model
set.seed(123)
model_cat <- lm(medv ~ chas + rad, data = df)

saveRDS(object = model_cat, "inst/model_objects/lm_model_with_cat_object.rds")

# Used for testing as well, so need a copy in the testthat directory
saveRDS(object = model_cat, "tests/testthat/model_objects/lm_model_with_cat_object.rds")
