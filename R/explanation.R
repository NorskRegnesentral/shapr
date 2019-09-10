#' Explain the output of machine learning models with more accurately estimated Shapley values
#'
#' @description TODO: Add a more detailed description
#'
#' @param x A matrix or data.frame. Contains the the features, whose
#' predictions ought to be explained (test data).
#'
#' @param explainer An \code{explainer} object to use for exaplaining the observations.
#' See \code{\link{shapr}}.
#'
#' @param approach Character. Note that \code{1 <= length(approach) <= n_features}, where
#' \code{n_features} where equals the total number of features in the model. All elements should
#' either be \code{gaussian}, \code{copula} or \code{empirical}. See details for more information.
#'
#' @param prediction_zero The prediction value for unseen data, typically equal to the mean of
#' the response.
#'
#' @param n_samples Positive integer. Indicating the maximum number of samples to use in the
#' Monte Carlo integration for every conditional expectation.
#'
#' @param seed Positive integer. If \code{NULL} a random seed will be used.
#'
#' @param ... Additional arguments passed to \code{\link{prepare_data}}
#'
#' @details
#' TODO: Add information about approach.
#' TODO: Some additional details about the returned object
#'
#' @return data.frame. Contains the estimated Shapley values for the test data. Note that
#' the dimensions of the data.frame equals \code{n x (p+1)}, where \code{n} equals the number
#' of test observations, and \code{p} equals the total number of features.
#'
#' @export
#'
#' @author Camilla Lingjaerde
explain <- function(x, explainer, approach, prediction_zero, ...) {

  # Check input for x
  if (!is.matrix(x) & !is.data.frame(x)) {
    stop("x should be a matrix or a dataframe.")
  }

  # Check input for approach
  if (!(is.vector(approach) &&
        is.atomic(approach) &&
        (length(approach) < ncol(x)) &&
        all(is.element(approach, c("empirical", "gaussian", "copula"))))
  ) {
    stop(
      paste(
        "It seems that you passed a non-valid value for approach.",
        "It should be either 'empirical', 'gaussian', 'copula' or",
        "a list."
      )
    )
  }

  if (length(approach) > 1) {
    class(x) <- "combined"
  } else {
    class(x) <- approach
  }

  UseMethod("explain", x)
}

#' @param type String or list. Only applicable when \code{approach='empirical'}. If a string, the
#' type of empirical approach to use,  equal to 'independence, 'gaussian' or 'fixed_sigma'. If a
#' list, the elements in the list refers to the rows in \code{x} that ought to be included in
#' each of the empirical approaches.
#'
#' @param fixed_sigma_vec Vector or numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='fixed_sigma'}. The bandwidth to use. Default value \code{0.1}
#'
#' @param AICc_no_samp_per_optim Positive integer. Only applicable when
#' \code{approach='empirical'} and \code{type='AICc_each_k'} or
#' \code{type='AICc_full'}. Number of samples to consider in AICc optimization.
#'
#' @param AIC_optim_max_eval Positive integer. Only applicable when \code{approach='empirical'}
#' and \code{type='AICc_each_k'} or \code{type='AICc_full'}. Numeric. Maximum value when
#' optimizing the AICc.
#'
#' @param AIC_optim_startval Numeric. Only applicable when \code{approach='empirical'} and
#' \code{type='AICc_each_k'} or \code{type='AICc_full'}. Starting value when optimizing the AICc.
#'
#' @param w_threshold Postive integer between 0 and 1.
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.empirical <- function(x, explainer, approach, prediction_zero,
                              type = "fixed_sigma", fixed_sigma_vec = 0.1,
                              AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                              AIC_optim_startval = 0.1, w_threshold = 0.95,combined=F, ...) {

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach
  explainer$type <- type
  explainer$fixed_sigma_vec <- fixed_sigma_vec
  explainer$AICc_no_samp_per_optim <- AICc_no_samp_per_optim
  explainer$AIC_optim_max_eval <- AIC_optim_max_eval
  explainer$AIC_optim_startval <- AIC_optim_startval
  explainer$w_threshold <- w_threshold

  # Get distance matrix ----------------
  explainer$D <- distance_matrix(
    explainer$x_train,
    x,
    explainer$X$features
  )
  # Generate data
  dt <- prepare_data(explainer, ...)

  # If combined method is used, do not compute Shapley values yet
  if(combined==T){
    return(list(dt,explainer))
  }

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)

  return(dt_kshap)
}

#' @inheritParams explain
#'
#' @param mu Numeric vector. (Optional) Containing the mean of the data generating distribution.
#' If \code{NULL} the expected values are estimated from the data. Note that this is only used
#' when \code{approach = "gaussian"}.
#'
#' @param cov_mat Numeric matrix. (Optional) Containing the covariance matrix of the data
#' generating distribution. \code{NULL} means it is estimated from the data if needed
#' (in the Gaussian approach).
#'
#' @rdname explain
#' @name explain
#'
#' @export
explain.gaussian <- function(x, explainer, approach, prediction_zero, mu = NULL, cov_mat = NULL, combined=F, ...) {

  # Add arguments to explainer object
  explainer$x_test <- as.matrix(x)
  explainer$approach <- approach

  # If mu is not provided directly, use mean of training data
  if (is.null(mu)) {
    explainer$mu <- unname(colMeans(explainer$x_train))
  } else {
    explainer$mu <- mu
  }

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
  dt <- prepare_data(explainer, ...)

  # If combined method is used, do not compute Shapley values yet
  if(combined==T){
    return(list(dt,explainer))
  }

  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}

#' @rdname explain
#' @name explain
#' @export
explain.copula <- function(x, explainer, approach, prediction_zero, combined=F,...) {

  # Setup
  explainer$x_test <- as.matrix(x)
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
  if (is.null(dim(x))) {
    x_test <- t(as.matrix(x))
  }

  explainer$mu <- rep(0, ncol(explainer$x_train))
  cov_mat <- stats::cov(x_train) # Gaussian transformed cov. mat
  eigen_values <- eigen(cov_mat)$values
  if (any(eigen_values <= 1e-06)) {
    explainer$cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
  } else {
    explainer$cov_mat <- cov_mat
  }
  # Generate data
  dt <- prepare_data(explainer, x_test = x_test, ...)

  # If combined method is used, do not compute Shapley values yet
  if(combined==T){
    return(list(dt,explainer))
  }
  # Predict
  dt_kshap <- prediction(dt, prediction_zero, explainer)
  return(dt_kshap)
}



#' @rdname explain
#' @export
explain.combined <- function(x, explainer, prediction_zero, approach = NULL,
                             type = NULL, fixed_sigma_vec = 0.1,
                             AICc_no_samp_per_optim = 1000, AIC_optim_max_eval = 20,
                             AIC_optim_startval = 0.1, w_threshold = 0.95, mu = NULL, cov_mat = NULL,
                             n_samples=1e5,...) {
  one.obs.only <- is.null(nrow(x))
  empirical.types=type
  if (is.null(approach)) {
    #if (one.obs.only) {
    # approach <- rep("empirical",1) # If nothing else is specified, empirical is used
    #} else {
    approach <- rep("empirical", explainer$n_features)
    #}
  }
  if (is.null(empirical.types)) {
    #if (!one.obs.only) {
    empirical.types <- rep("independence", sum(approach=='empirical'))
    #} else {
    # empirical.types <- list("independence" = 1)
    #}
  }
  if(length(type)==1){
    empirical.types<-rep(type,sum(approach=='empirical'))
  }
  # Setup
  explainer$x_test <- as.matrix(x)
  explainer$x_test <- x
  explainer$approach <- approach
  dt.final <- NULL
  dt.rownames <- NULL
  fixed.ind=indep.ind=gauss.ind=each.ind=full.ind=cop.ind=numeric(0)
  if (!all(approach %in% c("empirical", "gaussian", "copula"))) {
    stop("Approach must be 'empirical','gaussian' or 'copula'")
  }
  if ("empirical" %in% approach) {
    if (!all(empirical.types %in% c("independence", "fixed_sigma", "AICc_each_k", "AICc_full"))) {
      stop("Empirical approach must be 'independence','fixed_sigma','AICc_each_k' or 'AICc_full'")
    }
    if ("independence" %in% empirical.types) {
      emp_indep<- which(empirical.types=='independence')
      indep.ind=which(explainer$X$nfeatures %in% emp_indep)
      #if(1 %in% emp_indep) {emp_indep=c(0,emp_indep)} # If this approach is used when conditionioning on one var, include conditioning on none
      #expl = explainer
      #this.ind = which(explainer$X$nfeatures %in% emp_indep)
      #expl$S=explainer$S[this.ind,]
      #expl$X=explainer$X[this.ind,]
      #expl$W = explainer$W[,this.ind]
      dt.indep <- explain.empirical(x, explainer,
                                    approach = "empirical", type = "independence",
                                    prediction_zero = prediction_zero, w_threshold = w_threshold,combined=T
      )
      dt.indep.reduced = dt.indep[[1]][wcomb %in% indep.ind,]
      dt.final <- rbind(dt.final, dt.indep.reduced)
      explainer=dt.indep[[2]]
      #dt.rownames <- c(dt.rownames, emp_indep_ind)
    }
    if ("fixed_sigma" %in% empirical.types) {
      emp_fixed<- which(empirical.types=='fixed_sigma')
      fixed.ind = which(explainer$X$nfeatures %in% emp_fixed)
      #(1 %in% emp_fixed) {emp_fixed=c(0,emp_fixed)} # Include conditioning on none
      #expl = explainer
      #this.ind = which(explainer$X$nfeatures %in% emp_fixed)
      #expl$S=explainer$S[this.ind,]
      #expl$X=explainer$X[this.ind,]
      #expl$W = explainer$W[,this.ind]
      dt.fixed <- explain.empirical(x, explainer,
                                    approach = "empirical", prediction_zero = prediction_zero,
                                    type = "fixed_sigma", fixed_sigma_vec = fixed_sigma_vec, w_threshold = w_threshold,combined=T
      )
      dt.fixed.reduced = dt.fixed[[1]][wcomb %in% fixed.ind,]
      dt.final <- rbind(dt.final, dt.fixed.reduced)
      explainer=dt.fixed[[2]]
      #dt.rownames <- c(dt.rownames, emp_fixed_ind)
    }
    if ("AICc_each_k" %in% empirical.types) {
      emp_each <- which(empirical.types=='AICc_each_k')
      each.ind = which(explainer$X$nfeatures %in% emp_each)
      #if(1 %in% emp_each) {emp_each=c(0,emp_each)} # Include conditioning on none
      #expl = explainer
      #this.ind = which(explainer$X$nfeatures %in% emp_each)
      #expl$S=explainer$S[this.ind,]
      ##expl$X=explainer$X[this.ind,]
      #expl$W = explainer$W[,this.ind]
      dt.AICc_each <- explain.empirical(x, explainer,
                                        approach = "empirical", type = "AICc_each_k", prediction_zero = prediction_zero,
                                        AICc_no_samp_per_optim = AICc_no_samp_per_optim, AIC_optim_max_eval = AIC_optim_max_eval,
                                        AIC_optim_startval = AIC_optim_startval, w_threshold = w_threshold,combined=T
      )
      dt.each.reduced = dt.AICc_each[[1]][wcomb %in% each.ind,]
      dt.final <- rbind(dt.final, dt.each.reduced)
      explainer=dt.AICc_each[[2]]
      #dt.rownames <- c(dt.rownames, emp_each_ind)
    }
    if ("AICc_full" %in% empirical.types) {
      emp_full <- which(empirical.types=='AICc_full')
      full.ind = which(explainer$X$nfeatures %in% emp_full)
      #if(1 %in% emp_full) {emp_full=c(0,emp_full)} # Include conditioning on none
      #expl = explainer
      #this.ind = which(explainer$X$nfeatures %in% emp_full)
      #expl$S=explainer$S[this.ind,]
      #expl$X=explainer$X[this.ind,]
      #expl$W = explainer$W[,this.ind]
      dt.AICc_full <- explain.empirical(x, explainer,
                                        approach = "empirical", prediction_zero = prediction_zero,
                                        type = "AICc_full", AICc_no_samp_per_optim = AICc_no_samp_per_optim, AIC_optim_max_eval = AIC_optim_max_eval,
                                        AIC_optim_startval = AIC_optim_startval, w_threshold = w_threshold,combined=T
      )
      dt.full.reduced = dt.AICc_full[[1]][wcomb %in% full.ind,]
      dt.final <- rbind(dt.final, dt.full.reduced)
      explainer=dt.AICc_full[[2]]
      #dt.rownames <- c(dt.rownames, emp_full_ind)
    }
  }
  if ("gaussian" %in% approach) {
    gaussian_ind <- which(approach=='gaussian')
    gauss.ind = which(explainer$X$nfeatures %in% gaussian_ind)
    #if(1 %in% gaussian_ind) {gaussian_ind=c(0,gaussian_ind)} # Include conditioning on none
    #expl = explainer
    #this.ind = which(explainer$X$nfeatures %in% gaussian_ind)
    #expl$S=explainer$S[this.ind,]
    #expl$X=explainer$X[this.ind,]
    #expl$W = explainer$W[,this.ind]
    dt.gaussian <- explain.gaussian(x, explainer, approach = "gaussian", prediction_zero = prediction_zero, mu = mu,
                                    cov_mat = cov_mat, n_samples = n_samples,combined=T)
    dt.gaussian.reduced = dt.gaussian[[1]][wcomb %in% gauss.ind,]
    dt.final <- rbind(dt.final, dt.gaussian.reduced)
    explainer=dt.gaussian[[2]]
    #dt.rownames <- c(dt.rownames, gaussian_ind)
  }
  if ("copula" %in% approach) {
    copula_ind <- which(approach=='copula')
    cop.ind=which(explainer$X$nfeatures %in% copula_ind)
    #if(1 %in% copula_ind) {copula_ind=c(0,copula_ind)} # Include conditioning on none
    #expl = explainer
    #this.ind = which(explainer$X$nfeatures %in% copula_ind)
    #expl$S=explainer$S[this.ind,]
    #expl$X=explainer$X[this.ind,]
    #expl$W = explainer$W[,this.ind]
    dt.copula <- explain.copula(x, explainer, approach = "copula", prediction_zero = prediction_zero, n_samples = n_samples,combined=T)
    dt.copula.reduced = dt.copula[[1]][wcomb %in% cop.ind,]
    dt.final <- rbind(dt.final, dt.copula.reduced)
    explainer=dt.copula[[2]]
    #dt.rownames <- c(dt.rownames, copula_ind)
  }
  # Add results from conditioning on none or all variables:
  if(! one.obs.only){
      dt.empty = dt.final[1:nrow(x),]
      dt.empty[,1:(ncol(dt.empty)-3)]=as.data.frame(x)
      dt.empty$wcomb= 1.0
      dt.empty$id=1:nrow(x)
      dt.empty$w = 1
      dt.all= dt.empty
      dt.all$wcomb=length(explainer$X$nfeatures)
  }
  else{
    dt.empty=dt.final[1,,drop=F]
    dt.empty[,1:(ncol(dt.empty)-3)]=as.data.frame(x)
    dt.empty$wcomb= 1.0
    dt.empty$id=dt.empty$w=1
    dt.all=dt.empty
    dt.all$wcomb=length(explainer$X$nfeatures)
  }
  dt.final=rbind(dt.empty,dt.final,dt.all)


  # Sort by id
  dt.final = dt.final[order(dt.final$id),]
  # Sort by wcomb
  for(i in unique(dt.final$id)){
    dt.final[id==i,]=dt.final[id==i,][order(wcomb),]
  }
  # Predict
  dt_kshap <- prediction(dt.final, prediction_zero, explainer)

  #if (!one.obs.only) {
  #rownames(dt.final) <- dt.rownames
  #dt.final <- dt.final[order(dt.rownames), ]

  return(dt_kshap)
}

