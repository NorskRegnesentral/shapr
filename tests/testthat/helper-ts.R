options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(1234)

data_ts <- data.frame(matrix(NA, ncol = 41, nrow = 4))
for (n in 1:100) {
  set.seed(n)
  e <- rnorm(42, mean = 0, sd = 1)

  m_1 <- 0
  for (i in 2:length(e)) {
    m_1[i] <- 1 + 0.8 * m_1[i - 1] + e[i]
  }
  data_ts[n, ] <- m_1[-1]
}
data_ts <- data.table::as.data.table(data_ts)

x_var_ts <- paste0("X", 1:40)
y_var_ts <- "X41"

ind_x_explain <- 1:2
data_ts_train <- data_ts[-ind_x_explain]

# Creating a predictive model (for illustration just predicting the next point in the time series with a linear model)
lm_ts_formula <- as.formula(X41 ~ .)
model_lm_ts <- lm(lm_ts_formula, data_ts_train)

x_explain_ts <- data_ts[ind_x_explain, ..x_var_ts]
x_train_ts <- data_ts[-ind_x_explain, ..x_var_ts]

# Spitting the time series into 4 segments
group_ts <- list(
  S1 = paste0("X", 1:10),
  S2 = paste0("X", 11:20),
  S3 = paste0("X", 21:30),
  S4 = paste0("X", 31:40)
)


p0_ts <- mean(unlist(data_ts_train[, ..y_var_ts]))
