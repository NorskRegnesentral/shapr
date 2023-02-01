devtools::load_all()
library(data.table)
data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]
x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:100
x_train <- data[, ..x_var]
y_train <- data[, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# convert to factors
data[,Month_factor := as.factor(month.abb[Month])]
# data[, Temp_factor := fcase(Temp < 71, "low",
#                             Temp %between% c(71, 84), "medium",
#                             Temp > 84, "high")]
# data[, Temp_factor := as.factor(Temp_factor)]



data[, Temp_factor := as.factor(round(Temp, -1))]
data_train_cat <- copy(data)
data_explain_cat <- data[ind_x_explain,]

x_var_cat <- c("Solar.R", "Wind", "Temp_factor", "Month_factor")
x_train_cat <- data_train_cat[, ..x_var_cat]


# Example 1 - No errors -------------------------------------------------------------------------------------------
x_explain_cat <- data_explain_cat[, ..x_var_cat]
# x_explain_cat[, Wind := 10]
#x_explain_cat[, Month_factor := Month_factor[1]]
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var_cat, collapse = " + ")))
model_lm_cat <- lm(lm_formula,data_train_cat)

p0 <- mean(y_train)
explanation_cat <- explain(
  model = model_lm_cat,
  x_explain = x_explain_cat,
  x_train = x_train_cat,
  approach = "ctree",
  prediction_zero = p0
)


plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter")
plot(explanation_cat, plot_phi0 = FALSE, plot_type = "scatter", scatter_hist = FALSE)



# Example 2 - One factor value  -----------------------------------------------------------------------------------
x_explain_cat <- data_explain_cat[, ..x_var_cat]
x_explain_cat[, Month_factor := Month_factor[1]]
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var_cat, collapse = " + ")))
model_lm_cat <- lm(lm_formula,data_train_cat)

p0 <- mean(y_train)
explanation_cat <- explain(
  model = model_lm_cat,
  x_explain = x_explain_cat,
  x_train = x_train_cat,
  approach = "ctree",
  prediction_zero = p0
)

# Works fine
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter")
# Wrong x-labels due to breaks being different from when scatter_hist = TRUE
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter", scatter_hist = FALSE)



# Example 3 - few test observations  ------------------------------------------------------------------------------

x_explain_cat <- data_explain_cat[, ..x_var_cat]
x_explain_cat <- x_explain_cat[1:3, ]
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var_cat, collapse = " + ")))
model_lm_cat <- lm(lm_formula,data_train_cat)

p0 <- mean(y_train)
explanation_cat <- explain(
  model = model_lm_cat,
  x_explain = x_explain_cat,
  x_train = x_train_cat,
  approach = "ctree",
  prediction_zero = p0
)

# Only 4 ticks in the x-axis for the factor
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter")
# Wrong x-labels due to breaks being different from when scatter_hist = TRUE
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter", scatter_hist = FALSE)


# Example 4 - few observations - to many x-ticks with same label  -----------------------------------------

x_explain_cat <- data_explain_cat[, ..x_var_cat]
x_explain_cat <- x_explain_cat[1:4, ]
lm_formula <- as.formula(paste0(y_var, " ~ ", paste0(x_var_cat, collapse = " + ")))
model_lm_cat <- lm(lm_formula,data_train_cat)

p0 <- mean(y_train)
explanation_cat <- explain(
  model = model_lm_cat,
  x_explain = x_explain_cat,
  x_train = x_train_cat,
  approach = "ctree",
  prediction_zero = p0
)

# Duplicated labels on the x-axis
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter")
# Wrong x-labels due to breaks being different from when scatter_hist = TRUE
plot(explanation_cat, bar_plot_phi0 = FALSE, plot_type = "scatter", scatter_hist = FALSE)

