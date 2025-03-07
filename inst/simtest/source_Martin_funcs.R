## DOING TOSMEHING HERE

### ajaj



fit_model <- function(predictive_function_name,formula, data, tune = FALSE){
  p <- data$p


  if (predictive_function_name == "lm") {
    model = lm(formula$formula_as_formula, data = data$training_df)
  } else if (predictive_function_name == "gam") {
    model = mgcv::bam(formula$formula_as_formula_gam2, data = data$training_df,discrete = TRUE) # Discrete for much faster prediction, with very similar numbers
  } else if (predictive_function_name == "rf") {

    if(!tune){
      model = ranger::ranger(y~., data = data$training_df,
                             num.trees = 500,
                             splitrule = "variance")
    } else {
      grid = expand.grid(mtry = unique(c(floor(sqrt(data$p)), ceiling(sqrt(data$p)), data$p-2, data$p)),
                         splitrule = c("variance", "extratrees"),
                         min.node.size = c(1, 3, 5, 10, 20))
      fitControl <- trainControl(method = "CV",
                                 number = 4,
                                 verboseIter = TRUE)
      fit = caret::train(
        x = data$training_x,
        y = as.vector(data$training_y),
        method = 'ranger',
        num.trees = 500,
        tuneGrid = grid,
        trControl = fitControl
      )
      fit
      model = ranger::ranger(y~.,
                             data = data$training_df,
                             mtry = fit$bestTune$mtry,
                             splitrule = fit$bestTune$splitrule,
                             min.node.size = fit$bestTune$min.node.size,
                             num.trees = 500)
    }

  } else if (predictive_function_name == "ppr") {
    # Projected pursuit regression

    if(!tune){
      model = ppr(y ~ ., data$training_df, nterms = data$p)
    } else {

      # Get the number of terms we want to look at
      nterms = c(1, data$p)
      if (2 < data$p) nterms = c(nterms, seq(2, data$p, 1))
      nterms = sort(unique(nterms))

      # Create a grid of possible hyper-parameters
      grid = expand.grid(nterms = nterms)

      # Set the controls of the hyper paramert tuning
      fitControl <- trainControl(method = "CV", #"repeatedcv"
                                 number = 10,
                                 verboseIter = TRUE)

      # Tune the model
      model = caret::train(
        x = data$training_x,
        y = as.vector(data$training_y),
        method = 'ppr',
        tuneGrid = grid,
        trControl = fitControl
      )
      # model
      model = ppr(y ~ ., data$training_df, nterms =  model$bestTune$nterms)
    }

  } else {
    stop(sprintf("Do not recognise the specified predictive function '%s'", predictive_function_name))
  }

  return(model)


}

comp_true_vS = function(expl_object,
                        x_explain,
                        distribution = NULL,
                        mu = NULL,
                        cov = NULL,
                        lambda = NULL,
                        omega = NULL,
                        sigma = NULL,
                        beta = NULL,
                        p = NULL,
                        b = NULL,
                        r = NULL,
                        n_MC_samples = 10000L,
                        seed = NULL) {

  set.seed(seed)

  # Check for valid distribution
  distribution = match.arg(distribution, c("mvn", "gh", "burr"))

  n_features <- ncol(x_explain)
  n_explain <- nrow(x_explain)
  x_explain_mat <- as.matrix(x_explain)
  feature_names <- colnames(x_explain)
  S <- expl_object$internal$objects$S
  X <- expl_object$internal$objects$X
  phi0 <- expl_object$internal$parameters$phi0


  if (distribution == "mvn") {

    # Check that the parameters of the mvn distribution is provided
    if (is.null(mu) || is.null(cov)) {
      stop(sprintf("Function did not recieve either 'mu' or 'cov' which are needed for the 'mvn' distribution."))
    }


    # Generate the MC samples from N(0, 1)
    MC_samples_mat <- matrix(rnorm(n_MC_samples * n_features), nrow = n_MC_samples, ncol = n_features)


    dt <- shapr:::prepare_data_gaussian_cpp(MC_samples_mat = MC_samples_mat, x_explain_mat = x_explain_mat, S = S, mu = mu, cov_mat = cov)
    dim(dt) <- c(nrow(S) * n_explain * n_MC_samples, n_features)


  } else if (distribution == "gh") {

    # Check that the parameters of gh the distribution is provided
    if (is.null(mu) || is.null(lambda) || is.null(omega) || is.null(sigma) || is.null(beta)) {
      stop(sprintf("Function did not recieve either 'mu', 'lambda', 'omega', 'sigma', or 'beta' which are needed for the 'gh' distribution."))
    }

    # Something like this
    X_aux = X$features

    gh_samp = lapply(
      X = X_aux,
      FUN = simulateCondDistHyperbolic,
      nSim = n_MC_samples,
      Sigma = sigma,
      lambda = lambda,
      omega = omega,
      beta = beta,
      mu = mu,
      p = M,
      Xtest = x_explain_mat[i, , drop = FALSE])

    # Combine the list of matrices into one common data.table
    dt = data.table::rbindlist(gh_samp, idcol = "id_coalition")



  } else if (distribution == "burr") {

    # Check that the parameters of the burr distribution is provided
    if (is.null(p) || is.null(b) || is.null(r)) {
      stop(sprintf("Function did not recieve either 'p', 'b', or 'r' which are needed for the 'burr' distribution."))
    }

    # Something like this
    X_aux = X$features


    Burr_samp = lapply(
      X = X_aux,
      FUN = simulateCondDistBurr,
      nSim = n_MC_samples,
      a = p,
      c = b,
      d = r,
      p = M,
      Xtest = x_explain_mat[i, , drop = FALSE])

    # Combine the list of matrices into one common data.table
    dt = data.table::rbindlist(Burr_samp, idcol = "id_coalition")

  } else {
    stop("Function does not support the provided distribution.")
  }

  # Convert to a data.table and add extra identification columns
  dt <- data.table::as.data.table(dt)
  data.table::setnames(dt, feature_names)
  dt[, id_coalition := rep(seq_len(nrow(S)), each = n_MC_samples * n_explain)]
  dt[, id := rep(seq(n_explain), each = n_MC_samples, times = nrow(S))]
  dt[, w := 1 / n_MC_samples]
  data.table::setcolorder(dt, c("id_coalition", "id", feature_names))

  pred_cols <- paste0("p_hat", seq_len(output_size))

  compute_preds(
    dt, # Updating dt by reference
    feature_names = feature_names,
    predict_model = predict_model,
    model = model,
    pred_cols = pred_cols,
    type = type,
    horizon = horizon,
    n_endo = n_endo,
    explain_idx = explain_idx,
    explain_lags = explain_lags,
    y = y,
    xreg = xreg
  )
  dt_vS <- compute_MCint(dt, pred_cols)



  # Return the explanations and the time it took to compute them
  return(dt)
}


get_sh_dt <- function(explainer){
  sh_dt <- data.table::melt(explainer$shapley_values_est, id.vars="explain_id")
}

get_vS_dt <- function(explainer){
  dt0 <- explainer$internal$output$dt_vS

  names(dt0) <- str_replace_all(names(dt0), "p_hat1_", "")
  vS_dt <- data.table::melt(dt0, id.vars="id_coalition",variable.name="explain_id")
  vS_dt[,explain_id := as.numeric(explain_id)]
  setcolorder(vS_dt, "explain_id")
  vS_dt
}

get_computation_time <- function(explainer){
  explainer$timing$total_time_secs
}
