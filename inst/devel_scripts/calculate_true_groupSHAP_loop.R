library(MASS)
library(data.table)
library(shapr)

source("/nr/project/stat/BigInsight/Projects/Fraud/Subprojects/NAV/Annabelle/shapr/inst/devel_scripts/3-calculate_true_shapley_withdatatable.R")

# parameters
dim <- 4
mu <- rep(0, dim)
no_categories <- 3
cutoff <- c(-200, 0, 1, 200)
no_categories <- length(cutoff) - 1
set.seed(1); beta <- round(rnorm(dim * no_categories + 1), 1)
Sample_test <- TRUE
No_train_obs <- 1000
No_test_sample <- 100
N_sample_gaussian <- 1000
noise <- TRUE
response_mod <- function(mod_matrix_full, beta, epsilon){
  as.vector(mod_matrix_full %*% beta) + epsilon
}
fit_mod <- "regression"
methods <- c("empirical", "gaussian", "ctree")
seed <- 1
corr <- c(0, 0.1, 0.3, 0.5, 0.8)
Sigma_diag <- 1

group2_1 <- list(c('feat_1_', 'feat_3_'), c('feat_2_', 'feat_4_'))
group2_2 <- list(c('feat_1_', 'feat_3_', 'feat_4_'), c('feat_2_'))
groupings <- list(group2_1, group2_2)

results <- NULL
timing0 <- Sys.time()
for(cc in corr){
  print(paste0("Correlation: ", cc))
  #grouping_results <- list()
  for(group2 in groupings){
    print(paste0("Grouping: ", group2))

    Sigma <- matrix(rep(cc, dim^2), nrow = dim, ncol = dim)
    for(i in 1:dim){
      Sigma[i, i] <- Sigma_diag
    }

    ## 1. simulate training data
    set.seed(seed)
    x <- mvrnorm(n =  No_train_obs, mu = mu, Sigma = Sigma)

    dt <- NULL
    for(i in 1:dim){
      dt <- cbind(dt, cut(x[, i], cutoff, labels = 1:no_categories))
    }

    ## Get test data
    x_test_list <- list()
    for(i in 1:dim){
      x_test_list[[i]] <- 1:no_categories
    }
    x_test_dt <- do.call(CJ, x_test_list)

    if(Sample_test){
      if(nrow(x_test_dt) > No_test_sample){
        sampled_rows <- sample(1:nrow(x_test_dt), size = No_test_sample, replace = FALSE)
        x_test_dt <- x_test_dt[sampled_rows, ]
      }
    }
    No_test_obs <- nrow(x_test_dt)
    dt <- data.table(rbind(dt, x_test_dt))
    setnames(dt, names(dt), paste0("feat_", 1:dim, "_"))
    feat_names <- names(dt[, 1:dim])

    dt_numeric <- dt
    dt <- dt[, lapply(.SD, as.factor)]

    set.seed(seed)
    if(noise == TRUE){
      epsilon1 <- rnorm(No_train_obs, 0, 0.1^2)
      epsilon2 <- rnorm(No_test_obs, 0, 0.1^2)
      epsilon <- c(epsilon1, epsilon2)

      dt_numeric[, epsilon := epsilon]
      dt[, epsilon := epsilon]
    } else{
      dt_numeric[, epsilon := 0]
      dt[, epsilon := 0]
    }

    ## 2. One-hot encoding of training data
    mod_matrix <- model.matrix(~.-1, data = dt[, 1:dim], contrasts.arg = lapply(dt[, 1:dim], contrasts, contrasts = FALSE))

    dt <- cbind(dt, data.table(mod_matrix))
    full_onehot_names <- colnames(mod_matrix)
    reduced_onehot_names <- full_onehot_names[-grep("_1$", full_onehot_names)] # names without reference levels

    ## 3. Calculate response
    dt[, response := response_mod(mod_matrix_full = cbind(1, mod_matrix), beta = beta, epsilon = epsilon)]

    ## 4. Fit model
    if(fit_mod == 'regression'){
      form <- as.formula(paste0("response ~", paste(feat_names, collapse = "+")))
      model <- lm(formula = form, data = dt[(1:No_train_obs), ])

      fmla_onehot <- as.formula(paste("response ~", paste(reduced_onehot_names, collapse = " + ")))
      model_onehot <- lm(fmla_onehot, data = dt[(1:No_train_obs)])
    }

    ## 5. initalize shapr object with trained model -- this is used for calculating true shapley
    x_train <- dt[(1:No_train_obs), ..feat_names]
    x_test <- dt[-(1:No_train_obs), ..feat_names]
    y_train <- dt[(1:No_train_obs), .(response)]

    # For computing the true Shapley values (with correlation 0)
    x_test_onehot_full <- dt[-(1:No_train_obs), ..full_onehot_names]
    x_test_onehot_reduced <- dt[-(1:No_train_obs), ..reduced_onehot_names]
    x_train_onehot_reduced <- dt[(1:No_train_obs), ..reduced_onehot_names]

    explainer <- shapr(x_train, model)
    if(any(grepl("empirical", methods)) | any(grepl("gaussian", methods)) | any(grepl("ctree_onehot", methods))){
      explainer_onehot <- shapr(x_train_onehot_reduced, model_onehot)
    }

    ## Create custom function of model_type for lm
    model_type.numeric_lm <<- function(x) {
    }

    features.numeric_lm <<- function(x, cnms, feature_labels = NULL) {
      if (!is.null(feature_labels)) message_features_labels()

      nms <- tail(all.vars(x$terms), -1)
      if (!all(nms %in% cnms)) error_feature_labels()
      return(nms)
    }

    # Create custom function of predict_model for caret
    predict_model.numeric_lm <<- function(x, newdata) {
      newdata <- as.data.table(newdata)
      newdata0 <- newdata[, lapply(.SD, as.factor)]
      class(x) <- "lm"
      predict(x, newdata0)
    }

    class(model) <- "numeric_lm"
    ## End custom function

    ## Start grouping stuff
    group1 <- list(c(1), # no groups at all
                   c(2),
                   c(3),
                   c(4))
    group1_names = lapply(group1, function(x){names(x_test)[x]})
    explainer_group1 <- shapr(x_train, model, group = group1_names)


    #group2_names = lapply(group2, function(x){names(x_test)[x]})
    group2_names <- group2
    explainer_group2 <- shapr(x_train, model, group = group2_names)

    # no grouping - used as a test
    set.seed(10) # even if you set a seed, joint_prob_dt_list[[2]] will be different! Ask Martin? But only for corr > 0
    joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                                  algorithm = mvtnorm::Miwa())
    marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
    cond_list0 <- cond_prob(marg_list,
                            joint_prob_dt = joint_prob_dt_list[[1]],
                            explainer,
                            model)

    cond_expec_mat0 <- cond_expec_new(cond_list = cond_list0,
                                      explainer,
                                      x_test,
                                      prediction_zero = joint_prob_dt_list[[2]],
                                      joint_prob_dt = joint_prob_dt_list[[1]])

    Kshap0 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer$W))
    for (i in 1:nrow(x_test)) {
      Kshap0[i, ] = explainer$W %*% t(as.matrix(cond_expec_mat0[i, ]))
    }
    Kshap0 <- data.table(Kshap0)
    dim <- ncol(x_test)
    setnames(Kshap0, 1:(dim + 1), c("none", names(x_test)))

    # no ACTUAL grouping but use groups - used as a test
    set.seed(10)
    joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                                  algorithm = mvtnorm::Miwa())
    marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
    cond_list1 <- cond_prob(marg_list,
                            joint_prob_dt = joint_prob_dt_list[[1]],
                            explainer,
                            model,
                            group = group1_names)
    cond_expec_mat1 <- cond_expec_new(cond_list = cond_list1,
                                      explainer,
                                      x_test,
                                      prediction_zero = joint_prob_dt_list[[2]],
                                      joint_prob_dt = joint_prob_dt_list[[1]],
                                      group_names = explainer_group1$X$id_combination)

    Kshap1 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group1$W))
    for (i in 1:nrow(x_test)) {
      Kshap1[i, ] = explainer_group1$W %*% t(as.matrix(cond_expec_mat1[i, ]))
    }
    Kshap1 <- data.table(Kshap1)
    setnames(Kshap1, 1:(length(group1_names)+1), c("none", paste0("group", 1:length(group1_names))))

    # grouping actually starts
    set.seed(10)
    joint_prob_dt_list <- create_exact_joint_prob(mu, Sigma, beta, explainer, cutoff, response_mod,
                                                  algorithm = mvtnorm::Miwa())
    marg_list <- marg_prob(joint_prob_dt_list[[1]], explainer)
    cond_list2 <- cond_prob(marg_list,
                            joint_prob_dt = joint_prob_dt_list[[1]],
                            explainer,
                            model,
                            group = group2_names)
    cond_expec_mat2 <- cond_expec_new(cond_list = cond_list2,
                                      explainer,
                                      x_test,
                                      prediction_zero = joint_prob_dt_list[[2]],
                                      joint_prob_dt = joint_prob_dt_list[[1]],
                                      group_names = explainer_group2$X$id_combination)

    Kshap2 <- matrix(0, nrow = nrow(x_test), ncol = nrow(explainer_group2$W))
    for (i in 1:nrow(x_test)) {
      Kshap2[i, ] = explainer_group2$W %*% t(as.matrix(cond_expec_mat2[i, ]))
    }
    Kshap2 <- data.table(Kshap2)
    setnames(Kshap2, 1:(length(group2_names)+1), c("none", paste0("group", 1:length(group2_names))))

    ##  -------------------------------------------

    Kshap0_dt <- data.table(Kshap0)
    names <- NULL
    for(i in 1:length(group2)){
      names <- c(names, paste0('group', i))
      Kshap0_dt[, paste0('group', i) := rowSums(.SD), .SDcols = group2[[i]]]
    }

    results0 <- data.table(correlation = cc,
                           base_MAE = mean(rowMeans(abs(Kshap0 - Kshap1)[,-1])),
                           group_MAE = mean(rowMeans(abs(Kshap0_dt[,..names] - Kshap2[,-1]))))
    for(i in 1:length(group2)){
      results0[, paste0('group', i) := paste0(group2[[i]], collapse = ", ") ]
    }

    results <- rbind(results, results0)
  }
}

print(Sys.time() - timing0)


