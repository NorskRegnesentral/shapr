

source("inst/explain_manual_source.R")

library(xgboost)
library(data.table)
library(shapr)

data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

# Renaming for convenience
names(data)[2:5] <- c("G","S0","C1","C2")

x_var <- c("G","S0","C1","C2") #c("Solar.R", "Wind", "Temp", "Month")
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

# Settings

# Sanity check using all coaltions

# Using ctree without node sampling for full reproducability
exp_ctree_full <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  phi0 = p0,
)


coalition_list <- list(numeric(0), c(1), c(2), c(3), c(4), c(1,2), c(1,3), c(1,4), c(2,3), c(2,4), c(3,4), c(1,2,3), c(1,2,4), c(1,3,4), c(2,3,4), c(1,2,3,4))
R_D <- exp_ctree_full$internal$objects$W

exp_ctree_full_man <- explain_manual(model = model,
                                     x_explain = x_explain,
                                     x_train = x_train,
                                     approach = "ctree",
                                     phi0 = p0,
                                     coalition_list = coalition_list,
                                     R_D = R_D,
                                     ctree.sample = FALSE
)

exp_ctree_full_man

# Checking equality
all.equal(exp_ctree_full_man,
          exp_ctree_full$shapley_values_est[,-1])

# Yes, identical

### Doing the same for groups:


#### So what if consider groups of features

group <- list(A = c("Solar.R","Wind"),
              B = "Temp",
              C = "Month")

exp_ctree_full_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  group = group,
  phi0 = p0
)

# Note that coalitions group now refer to coaltions of the groups (where A=1, B=2, C=3 in the above example)
coalition_list <- exp_ctree_full_group$internal$objects$X$coalitions
R_D <- exp_ctree_full_group$internal$objects$W

exp_ctree_full_group_man <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_full_group_man

# Checking equality
all.equal(exp_ctree_full_group_man,
          exp_ctree_full_group$shapley_values_est[,-1])


### Let also check using just a few of the coalitions

exp_ctree_samp_group <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = "ctree",
  ctree.sample = FALSE,
  group = group,
  phi0 = p0,
  max_n_coalitions = 4
)

coalition_list <- exp_ctree_samp_group$internal$objects$X$coalitions
R_D <- exp_ctree_samp_group$internal$objects$W

exp_ctree_samp_group_man <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_samp_group_man

# Checking equality
all.equal(exp_ctree_samp_group_man,
          exp_ctree_samp_group$shapley_values_est[,-1])

# Still identical

# Also checking that the order we provide the coalitons in does not matter (except that the empty and full set are first and last)

exp_ctree_samp_group$internal$objects$X$coalitions

coalition_list <- exp_ctree_samp_group$internal$objects$X$coalitions[c(1,3,2,4)]
R_D <- exp_ctree_samp_group$internal$objects$W[,c(1,3,2,4)]

exp_ctree_samp_group_man2 <- explain_manual(model = model,
                                           x_explain = x_explain,
                                           x_train = x_train,
                                           approach = "ctree",
                                           phi0 = p0,
                                           coalition_list = coalition_list,
                                           R_D = R_D,
                                           ctree.sample = FALSE,
                                           group = group
)

exp_ctree_samp_group_man2

# Checking equality
all.equal(exp_ctree_samp_group_man,
          exp_ctree_samp_group_man2)

# TRUE


### And finally a manual example

set.seed(123)


# Note that parallelization and progress bar are still supported
future::plan("multisession", workers = 2) # Increase the number of workers for increased performance with many features
progressr::handlers(global = TRUE)
progressr::handlers("cli") # Using the cli package as backend (recommended for the estimates of the remaining time)


coalition_list <- list(numeric(0), c(1,3), c(1,3,4), c(1,2,4), c(4), c(3), c(1,2,3,4))
R_D <- exp_ctree_full$internal$objects$W[,c(1,sample(2:15,5,replace = FALSE),16)]
phi0  = p0


explain_manual(model = model,
               x_explain = x_explain,
               x_train = x_train,
               approach = "gaussian",
               phi0 = p0,
               coalition_list = coalition_list,
               R_D = R_D
)

### Jeroens code and example


# Parsing function to get coalition_list
parse_coalitions <- function(shap_names, coalition_str) {
  result <- vector("list", length(coalition_str))

  for (i in seq_along(coalition_str)) {
    if (coalition_str[i] == "Ø") {
      result[[i]] <- numeric()
    } else {
      components <- strsplit(coalition_str[i], "_")[[1]]
      result[[i]] <- match(components, shap_names)
    }
  }

  return(result)
}

