library(xgboost)
library(shapr)
library(data.table)

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



test2_indep <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "independence",
  group = group,
  phi0 = p0, max_n_coalitions = 10
)

test2_ctree <- explain(
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

test2_mixed <- explain(
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

coal_indep <- test2_indep$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="independence",coalitions_str],id_coalition]
coal_ctree <- test2_ctree$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="ctree",coalitions_str],id_coalition]


all.equal(test2_indep$internal$output$dt_vS[id_coalition%in%coal_indep],
          test2_mixed$internal$output$dt_vS[id_coalition%in%coal_indep])


all.equal(test2_ctree$internal$output$dt_vS[id_coalition%in%coal_ctree],
          test2_mixed$internal$output$dt_vS[id_coalition%in%coal_ctree])



##### TEST 3 ####
# Specify coalition_approach_dt with the approach to use for each specific coalition
# BUT USING GROUPS, and with regression_separate as one of the approaches

coalition_approach_dt <- rbind(
  data.table(coalitions_str = c("1","3"),
             approach_new = "ctree"),
  data.table(coalitions_str = "2",
             approach_new = "regression_separate")
)

group = list(A = c("Wind","Temp"), B = c("Month"), C = "Solar.R")



test3_reg <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "regression_separate",
  group = group,
  phi0 = p0, max_n_coalitions = 10
)

test3_ctree <- explain(
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

test3_mixed <- explain(
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

coal_reg <- test3_reg$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="regression_separate",coalitions_str],id_coalition]
coal_ctree <- test3_ctree$internal$objects$X[coalitions_str%in%coalition_approach_dt[approach_new=="ctree",coalitions_str],id_coalition]


all.equal(test3_reg$internal$output$dt_vS[id_coalition%in%coal_indep],
          test3_mixed$internal$output$dt_vS[id_coalition%in%coal_indep])


all.equal(test3_ctree$internal$output$dt_vS[id_coalition%in%coal_ctree],
          test3_mixed$internal$output$dt_vS[id_coalition%in%coal_ctree])



##### TEST 4 ####
# Test overlapping groups


group = list(A = c("Wind","Temp"), B = c("Month","Wind"), C = c("Solar.R","Month"))


test4_reg <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "regression_separate",
  group = group,
  phi0 = p0, max_n_coalitions = 10,
  experimental_args = list(allow_overlapping_groups = TRUE)
)

test4_reg$internal$objects$S

test4_reg$internal$objects$X



group = list(A = c("Wind","Month"), B = c("Month","Wind","Temp"), C = c("Solar.R"),C2="Solar.R")


test4_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  group = group,
  phi0 = p0,
  ctree.sample = FALSE,
  n_MC_samples = 1000,
  experimental_args = list(allow_overlapping_groups = TRUE)
)

test4_ctree$shapley_values_est
# Sanity check, C and C2 gets exactly the same shapley values.

test4_ctree$internal$objects$S

test4_ctree$internal$objects$X

test4_ctree$internal$output$dt_vS



##### TEST 5 ####
# Non-complete causal ordering

# First an actual complete causal ordering

causal_ordering = list(4,c(2,3), 1) # Equivalent to list(c("Month"),c("Wind","Temp"), "Solar.R")


test5_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  causal_ordering = causal_ordering,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)


test5_gaussian <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  causal_ordering = causal_ordering,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test5_gaussian$internal$objects$X


# Now an incomplete-ordering
causal_ordering2 = list(4,c(2,3)) # 1 is free

test5_gaussian2 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  causal_ordering = causal_ordering2,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test5_gaussian2$internal$objects$X
test5_gaussian2$internal$objects$S

# Now an incomplete-ordering
causal_ordering3 = list(c(2,3),1) # 4 is free

test5_gaussian3 <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  causal_ordering = causal_ordering3,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test5_gaussian3$internal$objects$X
test5_gaussian3$internal$objects$S


#### Incomplete causal ordering with groups ####

causal_ordering3 = list(1,2)#list("A", "B") # C is free

group <- list(A=c("Wind","Temp"), B=c("Solar.R"),C=c("Month"))

test6_gaussian <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "gaussian",
  causal_ordering = causal_ordering3,
  group = group,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test6_gaussian$internal$objects$S
test6_gaussian$internal$objects$X

# Not working for some reason
test6_ctree <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  causal_ordering = causal_ordering3,
  group = group,
  asymmetric = TRUE,
  phi0 = p0,
  max_n_coalitions = 1000,
  ctree.sample = FALSE,
  n_MC_samples = 1000
)

test6_ctree$internal$objects$S
test6_ctree$internal$objects$X

test6_ctree$shapley_values_est
test6_gaussian$shapley_values_est


### Looking carefully into the valid coalitions for random case

m <- 6
causal_ordering <- list(6,c(3,2),1) # 4 and 5 are free
free_causal_values <- seq_len(m)[!(seq_len(m) %in% unlist(causal_ordering))]

valid_coal_list <- shapr:::get_valid_causal_coalitions(causal_ordering = causal_ordering,free_causal_values = free_causal_values)

# Getting the binary matrix for these coalitions

(S_valid <- shapr:::coalition_matrix_cpp(valid_coal_list,m=m))

# Looks good!
