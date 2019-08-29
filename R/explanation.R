#' Explaining the output of machine learning models with more accurately estimated Shapley values
#'
#' @param x 1
#' @param explainer 2
#' @param approach 3
#' @param prediction_zero 4
#' @param ... Soething
#'
#' @export
explain <- function(x, explainer, approach, prediction_zero, n_samples, ...) {

  str_error <- paste(
    "It seems that you passed a non-valid value for approach.",
    "It should be either 'empirical', 'gaussian', 'copula' or",
    "a list."
  )

  nms_valid <- c("empirical", "gaussian", "copula", "combined")

  if(is.list(approach)){
    if(! all(colnames(approach) %in% nms_valid)){
      stop(str_error)
    }
    class(x) <- "combined"
  } else {
    if(!is.element(approach, nms_valid)) {
      stop(str_error)
    }
    class(x) <- approach
  }

  UseMethod("explain", x)
}

#' @rdname explain
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero, index_features,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                              AIC_optim_startval = 0.1, w_threshold = 0.95) {

  if(is.null(dim(x))){
    x=t(as.matrix(x))
    }
  # Add arguments to explainer object
  explainer$x_test <- x # The data to explain
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$AICc_no_samp_per_optim <- AICc_no_samp_per_optim
  explainer$AIC_optim_max_eval <- AIC_optim_max_eval
  explainer$AIC_optim_startval <- AIC_optim_startval
  explainer$w_threshold <- w_threshold
  explainer$n_samples <- AICc_no_samp_per_optim
  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$x_train,
    x,
    explainer$X$features
  )
  # Generate data
  dt <- prepare_data(explainer)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
}

#' @rdname explain
#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, n_samples = 1e3) {
  if(is.null(dim(x))){
    x=t(as.matrix(x))
  }
  # Add arguments to explainer object
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- unname(colMeans(explainer$x_train))
  } else{explainer$mu=mu}

  # If cov_mat is not provided directly, use sample covariance of training data
  if (is.null(cov_mat)) {
    cov_mat <- stats::cov(explainer$x_train)
  }

  # Make sure that covariance matrix is positive-definite
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }

  # Generate data
  dt <- prepare_data(explainer)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}

#' @rdname explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, n_samples = 1e3) {
  if(is.null(dim(x))){
    x=t(as.matrix(x))
  }
  # Setup
  explainer$n_samples <- n_samples
  explainer$x_test <- x
  explainer$approach <- approach

  # Prepare transformed data
  x_train <- apply(
    X = explainer$x_train,
    MARGIN = 2,
    FUN = gaussian_transform
  )
  x_test <- apply(
    X = rbind(explainer$x_test, explainer$x_train),
    MARGIN = 2,
    FUN = gaussian_transform_separate,
    n_y = nrow(explainer$x_test)
  )

  explainer$mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train) # Gaussian transformed cov. mat
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }
  # Generate data
  dt <- prepare_data(explainer, x_test)

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)

}

#' @rdname explain
#' @export
explain.combined <- function(x, explainer, prediction_zero, approach=NULL,
                             empirical.types=NULL,nsamples=1e3,fixed_sigma_vec = 0.1,
                             AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                             AIC_optim_startval = 0.1, w_threshold = 0.95,mu = NULL, cov_mat = NULL,...) {
  one.obs.only=is.null(nrow(x))

  if(is.null(approach)){
    if(one.obs.only){approach=list("empirical"=1)}else {approach=list("empirical"=1:nrow(x))}
  }
  if(is.null(empirical.types)){
    if(!one.obs.only){empirical.types=list("independence"=1:nrow(x))}else{empirical.types=list("independence"=1)}
  }
  dt.final=NULL
  dt.rownames=NULL
  if ("empirical" %in% names(approach)) {
    if(! all(names(empirical.types) %in% c("independence","fixed_sigma","AICc_each_k","AICc_full"))){
      stop("Empirical approach must be 'independence','fixed_sigma','AICc_each_k' or 'AICc_full'")
    }
    if("independence" %in% names(empirical.types)){
      emp_indep_ind = empirical.types$independence
      if(one.obs.only){x.emp.indep=x} else{x.emp.indep=x[emp_indep_ind,]}
      dt.indep <- explain.empirical(x.emp.indep, explainer, approach="empirical", type="independence",prediction_zero,w_threshold)
      dt.final<-rbind(dt.final,dt.indep)
      dt.rownames<- c(dt.rownames,emp_indep_ind)
    }
    if("fixed_sigma"%in% names(empirical.types)){
      emp_fixed_ind = empirical.types$fixed_sigma
      if(one.obs.only){x.emp.fixed=x} else{x.emp.fixed=x[emp_fixed_ind,]}
      dt.fixed <- explain.empirical(x.emp.fixed, explainer, approach="empirical", prediction_zero,
                          type = "fixed_sigma", fixed_sigma_vec, w_threshold)
      dt.final<-rbind(dt.final,dt.fixed)
      dt.rownames<- c(dt.rownames,emp_fixed_ind)
    }
    if("AICc_each_k"%in% names(empirical.types)){
      emp_each_ind = empirical.types$AICc_each_k
      if(one.obs.only){x.emp.each=x} else{x.emp.each=x[emp_each_ind,]}
      dt.AICc_each <- explain.empirical(x.emp.each, explainer, approach="empirical", type="AICc_each_k",prediction_zero,
              AICc_no_samp_per_optim, AIC_optim_max_eval, AIC_optim_startval, w_threshold)
      dt.final<-rbind(dt.final,dt.AICc_each)
      dt.rownames<- c(dt.rownames,emp_each_ind)
    }
    if("AICc_full"%in% names(empirical.types)){
      emp_full_ind = empirical.types$AICc_full
      if(one.obs.only){x.emp.full=x} else{x.emp.full=x[emp_full_ind,]}
      dt.AICc_full <- explain.empirical(x.emp.full, explainer, approach="empirical", prediction_zero,
          type = "AICc_full", AICc_no_samp_per_optim, AIC_optim_max_eval, AIC_optim_startval, w_threshold)
      dt.final<-rbind(dt.final,dt.AICc_full)
      dt.rownames<- c(dt.rownames,emp_full_ind)
    }
  }
  if ("gaussian" %in% names(approach)) {
    gaussian_ind <- approach$gaussian
    if(!one.obs.only){x.gauss=x[gaussian_ind,]} else {x.gauss=x}
    dt.gaussian <- explain.gaussian(x.gauss, explainer, approach="gaussian", prediction_zero, mu, cov_mat, n_samples=nsamples)
    dt.final<-rbind(dt.final,dt.gaussian)
    dt.rownames<- c(dt.rownames,gaussian_ind)

  }
  if ("copula" %in% names(approach)) {
    copula_ind <- approach$copula
    if(!one.obs.only){x.copula=x[copula_ind,]} else {x.copula=x}
    dt.copula <- explain.copula(x.copula, explainer, approach="copula", prediction_zero, n_samples=nsamples)
    dt.final<-rbind(dt.final,dt.copula)
    dt.rownames<- c(dt.rownames,copula_ind)
  }
  if(!one.obs.only){
    rownames(dt.final)=dt.rownames
    dt.final=dt.final[order(dt.rownames),]
  }
  return(dt.final)
}
