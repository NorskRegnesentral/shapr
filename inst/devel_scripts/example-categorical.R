library(MASS)
library(shapr)

# continuous
x_train <- head(Boston, -3)
x_test <- tail(Boston, 3)

# Fit a linear model
model <- lm(medv ~ crim + zn, data = x_train)

# Create an explainer object
explainer <- shapr(x_train, model)

# Explain predictions
p <- mean(x_train$medv)

explain1 <- explain(x_test, explainer, approach = "empirical", prediction_zero = p)

explain2 <- explain(x_test, explainer, approach = "gaussian", prediction_zero = p)
explain2$dt

explain3 <- explain(x_test, explainer, approach = "copula", prediction_zero = p)
explain4 <- explain(x_test, explainer, approach = "ctree", prediction_zero = p)


# categorical approach# need only factor variables
Boston$rad <- as.factor(Boston$rad)
Boston$chas <- as.factor(Boston$chas)

x_train <- head(Boston, -3)
x_test <- tail(Boston, 3)

# Fit a linear model
model <- lm(medv ~ rad + chas, data = x_train)

# Create an explainer object
explainer <- shapr(x_train, model)

# Explain predictions
p <- mean(x_train$medv)

explain4 <- explain(x_test, explainer, approach = "ctree", prediction_zero = p)
explain4$dt


explain5 <- explain(x_test, explainer, approach = "categorical", prediction_zero = p)
plot(explain5)
explain5$dt
