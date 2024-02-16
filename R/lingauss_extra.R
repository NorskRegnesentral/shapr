#' Compute Tmu and Tx for lingauss approach
#' @inheritParams default_doc_explain
#' @inheritParams setup_approach.gaussian
#'
#' @export
compute_lingauss_Tmu_Tx <- function(internal,
                                    gaussian.mu = NULL,
                                    gaussian.cov_mat = NULL, ...) {
  x_train <- internal$data$x_train
  n_explain <- internal$parameters$n_explain
  gaussian.cov_mat <- internal$parameters$gaussian.cov_mat
  gaussian.mu <- internal$parameters$gaussian.mu
  n_features <- internal$parameters$n_features
  perms_mat <- internal$objects$perms_mat

  n_permutations_used <- nrow(perms_mat)

  # For consistency
  defaults <- mget(c("gaussian.mu", "gaussian.cov_mat"))
  internal <- insert_defaults(internal, defaults)

  x_train <- internal$data$x_train
  feature_specs <- internal$objects$feature_specs

  # Checking if factor features are present
  if (any(feature_specs$classes == "factor")) {
    factor_features <- names(which(feature_specs$classes == "factor"))
    factor_approaches <- get_factor_approaches()
    stop(paste0(
      "The following feature(s) are factor(s): ", factor_features, ".\n",
      "approach = 'lingauss' does not support factor features.\n",
      "Please change approach to one of ", paste0(factor_approaches, collapse = ", "), "."
    ))
  }

  # If gaussian.mu is not provided directly in internal list, use mean of training data
  if (is.null(gaussian.mu)) {
    gaussian.mu <- get_mu_vec(x_train)
  }

  # If gaussian.cov_mat is not provided directly in internal list, use sample covariance of training data
  if (is.null(gaussian.cov_mat)) {
    gaussian.cov_mat <- get_cov_mat(x_train)
  }

  Tmu_list <- Tx_list <- list()
  for (j in 1:n_features) {
    Tmu_list[[j]] <- Tx_list[[j]] <- matrix(0, nrow = n_features, ncol = n_features)
    Tmu_list[[j]][j, j] <- -1
    Tx_list[[j]][j, j] <- 1

    for (i in seq_len(n_permutations_used)) {
      ### TODO: Call which(perms_mat == j,arr.ind=TRUE) on the outside, then extract
      # all which have the first and last position and handle those separately

      perm0 <- perms_mat[i, ]

      position <- which(perm0 == j)
      PSfull <- diag(n_features)
      PS <- PSj <- diag(0, n_features)

      if (position == 1) {
        # U_S <- diag(0,n_features)

        #       this_Sj <- perm0[1]
        #       this_Sj_bar <- perm0[-1]

        PSj <- PSfull[j, , drop = FALSE]
        PSj_bar <- PSfull[-j, , drop = FALSE]

        #       U_Sj <- t(PSj_bar)%*%PSj_bar%*%gaussian.cov_mat%*%t(PSj)%*%solve(PSj%*%gaussian.cov_mat%*%t(PSj))%*%PSj

        Udiff <- t(PSj_bar) %*% PSj_bar %*% gaussian.cov_mat %*% t(PSj) %*%
          solve(PSj %*% gaussian.cov_mat %*% t(PSj)) %*% PSj
      } else {
        this_S <- sort(perm0[seq_len(position - 1)])
        this_Sj <- sort(perm0[seq_len(position)])
        this_S_bar <- sort(perm0[-seq_len(position - 1)])
        this_Sj_bar <- sort(perm0[-seq_len(position)])

        PS <- PSfull[this_S, , drop = FALSE]
        PSj <- PSfull[this_Sj, , drop = FALSE]
        PS_bar <- PSfull[this_S_bar, , drop = FALSE]
        PSj_bar <- PSfull[this_Sj_bar, , drop = FALSE]

        U_S <- t(PS_bar) %*% PS_bar %*% gaussian.cov_mat %*% t(PS) %*% solve(PS %*% gaussian.cov_mat %*% t(PS)) %*% PS
        U_Sj <- t(PSj_bar) %*% PSj_bar %*% gaussian.cov_mat %*% t(PSj) %*%
          solve(PSj %*% gaussian.cov_mat %*% t(PSj)) %*% PSj
        Udiff <- U_Sj - U_S
      }


      # Could compute these here as well, and then DO not use paired sampling, but I do assume the U-computation is the
      # most expensive part anyway, so will not do it here now.
      # U_Sbar <- t(PS)%*%PS%*%gaussian.cov_mat%*%t(PS_bar)%*%solve(PS_bar%*%gaussian.cov_mat%*%t(PS_bar))%*%PS_bar
      # U_Sj_bar <- t(PSj)%*%PSj%*%gaussian.cov_mat%*%t(PSj_bar)%*%
      #                 solve(PSj_bar%*%gaussian.cov_mat%*%t(PSj_bar))%*%PSj_bar


      Tmu_list[[j]] <- Tmu_list[[j]] - Udiff / n_permutations_used
      Tx_list[[j]] <- Tx_list[[j]] + Udiff / n_permutations_used
    }
  }

  internal$objects$Tmu_list <- Tmu_list
  internal$objects$Tx_list <- Tx_list

  internal$parameters$gaussian.mu <- gaussian.mu
  internal$parameters$gaussian.cov_mat <- gaussian.cov_mat

  return(internal)
}
