# Load example data
data("airquality")
airquality <- airquality[complete.cases(airquality), ]
x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

# Split data into test- and training data
data_train <- head(airquality, -3)
data_explain <- tail(airquality, 3)

x_train <- data_train[, x_var]
x_explain <- data_explain[, x_var]

y_train <- data_train[[y_var]]

# Fit a linear model
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var, collapse = " + ")))
model <- lm(lm_formula, data = data_train)

# Explain predictions
p <- mean(data_train[, y_var])


group_list <- list(A = c("Temp"), B= c("Month"), D = c("Wind"), E= c("Solar.R"))

explain_groups_sage <- explain(
  model = model,
  x_explain = x_train,
  x_train = x_train,
  group = group_list,
  approach = "empirical",
  phi0 = p,
  n_MC_samples = 1e2,
  sage = TRUE,
  response = y_train
)

explain_groups_sage <- explain(
  model = model,
  x_explain = x_train,
  x_train = x_train,
  group = group_list,
  approach = "empirical",
  phi0 = p,
  n_MC_samples = 1e2,
  sage = TRUE,
  response = y_train
)

explain_no_groups_sage <- explain(
  model = model,
  x_explain = x_train,
  x_train = x_train,
  approach = "empirical",
  phi0 = p,
  n_MC_samples = 1e2,
  sage = TRUE,
  response = y_train
)

plot(explain_groups_sage)

plot(explain_no_groups_sage)
