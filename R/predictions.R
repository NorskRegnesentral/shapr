#'
#'
#' OLD FUNCTION. Not in use any more.
#'
#' Get predictions
#'
#' @param feature_list List
#' @param pred_zero Numeric
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite, Martin Jullum
predictions <- function(model,
                        D,
                        h_optim_vec,
                        kernel_metric,
                        S,
                        Xtrain,
                        Xtest,
                        w_threshold = .7,
                        noSamp_MC = 1e3,
                        verbose = FALSE,
                        cond_approach_list,
                        feature_list,
                        pred_zero,
                        mu,
                        Sigma,
                        mu_Gauss_trans = mu_Gauss_trans,
                        Sigma_Gauss_trans = Sigma_Gauss_trans,
                        Xtest_Gauss_trans,
                        ensure_condcov_symmetry = F) {
  p <- ncol(Xtrain)

  DTp.Gaussian <- DTp.copula <- DTp.empirical <- NULL

  if ("Gaussian" %in% names(cond_approach_list)) {
    ## Assume Gaussian distributed variables and sample from the various conditional distributions
    these_wcomb <- cond_approach_list$Gaussian
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(S)))]

    samp_list <- lapply(
      X = feature_list[these_wcomb],
      FUN = sample_gaussian,
      noSamp_MC = noSamp_MC,
      mu = mu,
      Sigma = Sigma,
      p = p,
      Xtest = Xtest,
      ensure_condcov_symmetry = ensure_condcov_symmetry
    )

    DTp.Gaussian <- rbindlist(samp_list, idcol = "wcomb")
    DTp.Gaussian[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    DTp.Gaussian[, w := 1 / noSamp_MC]
  }
  if ("copula" %in% names(cond_approach_list)) {
    these_wcomb <- cond_approach_list$copula
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(S)))]


    samp_list <- lapply(
      X = feature_list[these_wcomb],
      FUN = sample_copula,
      noSamp_MC = noSamp_MC,
      mu = mu_Gauss_trans,
      Sigma = Sigma_Gauss_trans,
      p = p,
      Xtest_Gauss_trans = Xtest_Gauss_trans,
      Xtrain = Xtrain,
      Xtest = Xtest
    )

    DTp.copula <- rbindlist(samp_list, idcol = "wcomb")
    DTp.copula[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
    DTp.copula[, w := 1 / noSamp_MC]
  }

  if ("empirical" %in% names(cond_approach_list)) {
    these_wcomb <- cond_approach_list$empirical
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(S)))]

    no_wcomb <- length(these_wcomb)

    # Handle the computation of all training-test weights for ALL combinations here, before looping
    if (kernel_metric == "independence") {
      # Just random noise to "fake" a distance between observations
      D <- D[sample.int(n=nrow(D)),] # Randomly reordering the distance
      h_optim_vec <- mean(D)*1000 # Setting a very large bandwidth to give all used observation identical weight
    }
    # Common for both Gaussan and independence
    val <- t(t(-0.5 * D) / h_optim_vec^2)
    W_kernel <- exp(val)

    ## Get imputed data
    DTp.empirical <- observation_impute(
      W_kernel = W_kernel,
      S = S[these_wcomb, ],
      Xtrain = Xtrain,
      Xtest = Xtest,
      w_threshold = w_threshold,
      noSamp_MC = noSamp_MC
    )
    DTp.empirical[, wcomb := these_wcomb[wcomb]] # Correcting originally assigned wcomb
  }

  ## Performing prediction
  nms <- colnames(Xtest)

  DTp <- rbind(DTp.Gaussian, DTp.copula, DTp.empirical)
  DTp <- merge(DTp, data.table(wcomb = c(1, 2^p), w = 1), all = T)
  setkey(DTp, wcomb)


  DTp[!(wcomb %in% c(1, 2^p)), p_hat := predict_model(model, .SD), .SDcols = nms]
  DTp[wcomb == 2^p, p_hat := predict_model(model, as.data.frame(Xtest))]
  DTp[wcomb == 1, p_hat := pred_zero]

  ## Get mean probability
  DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
  setkey(DTres, wcomb)
  return(DTres)
}

#' @export
prediction <- function(dt, prediction_zero, explainer) {

  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "wcomb"))
  dt[, p_hat := predict_model(x = explainer$model, newdata = .SD), .SDcols = cnms]
  dt[wcomb == 1, p_hat := prediction_zero]

  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, wcomb)]
  data.table::setkeyv(dt_res, c("id", "wcomb"))

  # REMOVE?
  if(length(dt_res[id == 1, k])<ncol(explainer$W)){
    explainer$W=explainer$W[,-c(1,ncol(explainer$W))]
  }
  # Get mean probability - TODO: move this into a function (perhaps Rcpp)
  kshap <- matrix(0.0, nrow(explainer$W), nrow(explainer$x_test)) # Moved this to be after the test above.

  for (j in 1:ncol(kshap)) {

    kshap[, j] <- explainer$W %*% dt_res[id == j, k]
  }
  dt_kshap <- data.table::as.data.table(t(kshap))
  colnames(dt_kshap) <- c("none", cnms)

  return(dt_kshap)
}
