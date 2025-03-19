library(xgboost)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Looking at the dependence between the features
cor(x_train)

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 20,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Computing the Shapley values with kernelSHAP accounting for feature dependence using
# the empirical (conditional) distribution approach with bandwidth parameter sigma = 0.1 (default)
explanation <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "empirical",
  phi0 = p0, max_n_coalitions = 10,
)

##### TEST 1 ####
# Specify coalition_approach_dt with the approach to use for each specific coalition

coalition_approach_dt <- rbind(
  data.table(coalitions_str = c("2","3","4","2 3","3 4","2 3 4"),
             approach_new = "ctree"),
  data.table(coalitions_str = c("1","1 2","1 3","1 4","1 2 3","1 2 4","1 3 4"),
             approach_new = "independence"))


test1_indep <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence",
  phi0 = p0, max_n_coalitions = 10
  )

test1_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = p0,
  max_n_coalitions = 10,
  ctree.sample = FALSE,
  n_MC_samples = 1000
  )

test1_mixed <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence", # Does not matter
  phi0 = p0,
  max_n_coalitions = 10,
  ctree.sample = FALSE,
  n_MC_samples = 1000,
  experimental_args = list(coalition_approach_dt=coalition_approach_dt)
)

coal_indep <- test1_indep$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="independence",coalitions_str],id_coalition]
coal_ctree <- test1_ctree$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="ctree",coalitions_str],id_coalition]


all.equal(test1_indep$internal$output$dt_vS[id_coalition%in%coal_indep],
          test1_mixed$internal$output$dt_vS[id_coalition%in%coal_indep])


all.equal(test1_ctree$internal$output$dt_vS[id_coalition%in%coal_ctree],
          test1_mixed$internal$output$dt_vS[id_coalition%in%coal_ctree])

#####
#
#
#
# merge(test1_indep$internal$output$dt_vS,
#       test1_indep$internal$objects$X[,.(id_coalition,approach)],
#       by="id_coalition")
#
# merge(test1_ctree$internal$output$dt_vS,
#       test1_ctree$internal$objects$X[,.(id_coalition,approach)],
#       by="id_coalition")
#
#
# merge(test1_mixed$internal$output$dt_vS,
#       test1_mixed$internal$objects$X[,.(id_coalition,approach)],
#       by="id_coalition")


##### TEST 2 ####
# Specify coalition_approach_dt with the approach to use for each specific coalition
# BUT USING GROUPS

coalition_approach_dt <- rbind(
  data.table(coalitions_str = c("1","3"),
             approach_new = "ctree"),
  data.table(coalitions_str = "2",
             approach_new = "independence")
)

group = list(A = c("Wind","Temp"), B = c("Month"), C = "Solar.R")



test1_indep <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence",
  group = group,
  phi0 = p0, max_n_coalitions = 10
)

test1_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  group = group,
  phi0 = p0,
  max_n_coalitions = 10,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test1_mixed <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence", # Does not matter, but needs to be passed
  group = group,
  phi0 = p0,
  max_n_coalitions = 10,
  ctree.sample = FALSE,
  n_MC_samples = 1000,
  experimental_args = list(coalition_approach_dt=coalition_approach_dt)
)

coal_indep <- test1_indep$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="independence",coalitions_str],id_coalition]
coal_ctree <- test1_ctree$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="ctree",coalitions_str],id_coalition]


all.equal(test1_indep$internal$output$dt_vS[id_coalition%in%coal_indep],
          test1_mixed$internal$output$dt_vS[id_coalition%in%coal_indep])


all.equal(test1_ctree$internal$output$dt_vS[id_coalition%in%coal_ctree],
          test1_mixed$internal$output$dt_vS[id_coalition%in%coal_ctree])



coalition_approach_dt



test1_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  phi0 = p0,
  seed = 1245,
  max_n_coalitions = 16,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)



