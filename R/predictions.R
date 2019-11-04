#' Calculate shapley weights
#'
#' @description
#' TODO: Write a better description
#'
#' @param dt data.table
#' @param prediction_zero Positive integer between 0 & 1.
#' @param explainer An object of class \code{explainer}
#'
#' @details
#' TODO: Write details about how this is done (reference to paper)
#'
#' @examples
#' # TODO: Add simple examples
#'
#' @author Nikolai Sellereite
prediction <- function(dt, prediction_zero, explainer) {

  # Predictions
  cnms <- colnames(explainer$x_test)
  data.table::setkeyv(dt, c("id", "wcomb"))
  dt[, p_hat := predict_model(explainer$model, newdata = .SD), .SDcols = cnms]
  dt[wcomb == 1, p_hat := prediction_zero]
  p_all <- predict_model(explainer$model, newdata = explainer$x_test)
  dt[wcomb == max(wcomb), p_hat := p_all[id]]

  # Calculate contributions
  dt_res <- dt[, .(k = sum((p_hat * w) / sum(w))), .(id, wcomb)]
  data.table::setkeyv(dt_res, c("id", "wcomb"))
  dt_mat <- data.table::dcast(dt_res, wcomb ~ id, value.var = "k")
  dt_mat[, wcomb := NULL]
  kshap <-  t(explainer$W %*% as.matrix(dt_mat))
  dt_kshap <- data.table::as.data.table(kshap)
  colnames(dt_kshap) <- c("none", cnms)

  r <- list(dt = dt_kshap, model = explainer$model, p = p_all, x_test = explainer$x_test)
  attr(r, "class") <- c("shapr", "list")

  return(r)
}

#' Note that this function is deprecated, but we'll keep it for a week
#' to check that results are stable.
#'
#' TODO: Delete this function from the codebase
#'
#' @keywords internal
#'
#' @export
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
                        Xtest_Gauss_trans) {
  p <- ncol(Xtrain)

  DTp.Gaussian <- DTp.copula <- DTp.empirical <- NULL

  if ("Gaussian" %in% names(cond_approach_list)) {
    ## Assume Gaussian distributed variables and sample from the various conditional distributions
    these_wcomb <- cond_approach_list$Gaussian
    these_wcomb <- these_wcomb[!(these_wcomb %in% c(1, nrow(S)))]
    samp_list <- lapply(
      X = feature_list[these_wcomb],
      FUN = sample_gaussian,
      n_samples = noSamp_MC,
      mu = mu,
      cov_mat = Sigma,
      p = p,
      x_test = Xtest
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
      n_samples = noSamp_MC,
      mu = mu_Gauss_trans,
      cov_mat = Sigma_Gauss_trans,
      p = p,
      x_test_gaussian = Xtest_Gauss_trans,
      x_train = Xtrain,
      x_test = Xtest
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
      D <- D[sample.int(n = nrow(D)), ] # Randomly reordering the distance
      h_optim_vec <- mean(D) * 1000 # Setting a very large bandwidth to give all used observation identical weight
    }
    # Common for both Gaussan and independence
    val <- t(t(-0.5 * D) / h_optim_vec^2)
    W_kernel <- exp(val)

    ## Get imputed data
    DTp.empirical <- observation_impute(
      W_kernel = W_kernel,
      S = S[these_wcomb, ],
      x_train = Xtrain,
      x_test = Xtest,
      w_threshold = w_threshold,
      n_samples = noSamp_MC
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
