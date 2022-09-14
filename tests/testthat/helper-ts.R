options(digits = 5) # To avoid round off errors when printing output on different systems

set.seed(1234)

data_timeseries = data.frame(matrix(NA, ncol = 9, nrow = 4))
for(n in 1:100){
  set.seed(n)
  e <- rnorm(10, mean = 0, sd = 1)

  m_1 <- 0
  for(i in 2:length(e)){
    m_1[i] <- 1 + 0.8 * m_1[i - 1] + e[i]
  }
  data_timeseries[n, ] = m_1[-1]
}

group_timeseries <- list(Q1 = paste0("X", 1:2),
                         Q2 = paste0("X", 3:4),
                         Q3 = paste0("X", 5:6),
                         Q4 = paste0("X", 7:8))


formula = as.formula(paste0("X9", "~ ", paste0("X", 1:8, collapse = " + ")))
model_lm_timeseries = lm(formula, data_timeseries)
#
x_explain_timeseries = data_timeseries[1:2, 1:8]
x_train_timeseries = data_timeseries[99:100, 1:8]

p0_timeseries <- mean(data_timeseries[, 'X8'])
