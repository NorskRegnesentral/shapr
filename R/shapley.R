#' Calculate shapley weight
#'
#' @inheritParams global_arguments
#'
#' @return Numeric
#'
#' @export
#'
#' @author Nikolai Sellereite
w_shapley <- function(m, N, s) {
    (m - 1) / (N * s * (m - s))
}

#' Get combinations
#'
#' @param m Integer.
#' @param exact Logical.
#' @param nrows Integer
#'
#' @details
#' The returned data.table contains the following columns
#' \describe{
#' \item{ID}{Postive integer. Unique key for combination}
#' \item{features}{List}
#' \item{nfeautres}{Postive integer}
#' \item{N}{Postive integer}
#' }
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
get_combinations <- function(m, exact = TRUE, nrows = 200) {
    if (nrows>2^m){
        exact = TRUE
        cat(paste0("nrows is larger than 2^m = ",2^m,". Using exact instead."))
    }
    if (exact == TRUE) {
        N <- 2 ^ m
        X <- data.table(ID = 1:N)
        combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
        X[, features := unlist(combinations, recursive = FALSE)]
        X[, nfeatures := length(features[[1]]), ID]
        X[, N := .N, nfeatures]
    } else {

        ## Find weights for given number of features ----------
        DT <- data.table(nfeatures = head(1:m, -1))
        DT[, N := unlist(lapply(nfeatures, choose, n = m))]
        DT[, weight := w_shapley(m = m, N = N, s = nfeatures)]

        ## Sample number of features ----------
        X <- data.table(
            ID = seq(nrows),
            nfeatures = sample(
                x = DT[["nfeatures"]],
                size = nrows,
                replace = TRUE,
                prob = DT[["weight"]]
            )
        )

        ## Sample specific set of features ----------
        setkey(X, nfeatures)
        X[, ID := .I]
        X[, features := lapply(nfeatures, sample, x = 1:m)]

        ## Add zero features and m features ----------
        X_zero_all <- data.table(
            ID = seq(X[, max(ID)] + 1, length.out = 2),
            num_var = c(0, m),
            comb = c(list(numeric(0)), list(1:m))
        )
        X <- rbindlist(list(X, X_zero_all))
        setkey(X, nfeatures)
        X[, ID := .I]

        ## Add number of combinations
        X <- merge(x = X, y = DT[, .(nfeatures, N)], all.x = TRUE, on = "nfeatures")
        nms <- c("ID", "features", "nfeatures", "N")
        setcolorder(X, nms)
    }

    return(X)
}

#' Calculate shapley weights
#'
#' @param X data.table
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
get_weights <- function(X, m) {
    X[-c(1, .N), weight := w_shapley(m = m, N = N, s = nfeatures), ID]
    X[c(1, .N), weight := 10 ^ 6]

    return(X)
}

#' Get weighted matrix
#'
#' @param X data.table
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite
get_weighted_matrix <- function(X) {
    W <- weighted_matrix(
        features = X[["features"]],
        m = X[.N][["nfeatures"]],
        n = X[, .N]
    )

    return(W)
}

#' Scale training and test data
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
scale_data <- function(Xtrain, Xtest, scale = TRUE) {
    if (!is.data.table(Xtrain)) {
        Xtrain <- as.data.table(Xtrain)
    }
    if (!is.data.table(Xtest)) {
        Xtest <- as.data.table(Xtest)
    }

    if (scale) {
        nms <- colnames(Xtrain)
        setcolorder(Xtest, nms)
        sd <- Xtrain[, unname(sapply(.SD, sd, na.rm = TRUE))]
        Xtrain[, (nms) := .SD / sd]
        Xtest[, (nms) := .SD / sd]
    }

    return(list(Xtrain = Xtrain, Xtest = Xtest))
}

#' Get predictions
#'
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
impute_data <- function(W_kernel, S, Xtrain, Xtest, w_threshold = .7, n_threshold = 1e3) {

    ## Find weights for all combinations and training data
    DT = as.data.table(W_kernel)
    DT[, ID := .I]
    DT = data.table::melt(data = DT, id.vars = "ID", variable.name = "comb", value.name = "w", variable.factor = FALSE)

    ## Remove training data with small weight
    setkey(DT, comb, w)
    DT[, w := w / sum(w), comb]
    DT[, wcum := cumsum(w), comb]
    DT <- DT[wcum > 1 - w_threshold][, wcum := NULL]
    DT <- DT[, tail(.SD, n_threshold), comb]
    DT[, comb := gsub(comb, pattern = "V", replacement = ""), comb]
    DT[, wcomb := as.integer(comb), comb][, comb := NULL]

    ## Generate data used for prediction
    DTp <- impute_cpp(
        ID = DT[["ID"]],
        Comb = DT[["wcomb"]],
        Xtrain = Xtrain,
        Xtest = Xtest,
        S = S
    )

    ## Add keys
    DTp <- as.data.table(DTp)
    setnames(DTp, colnames(Xtrain))
    DTp[, wcomb := DT[["wcomb"]]]
    DTp[, w := DT[["w"]]]

    return(DTp)
}

#' Sample conditional Gaussian variables
#'
#' @param given_ind Vector
#' @param mu Vector
#' @param p Positive integer
#' @param Sigma Matrix
#'
#' @inheritParams global_arguments
#'
#' @return data.table with n_threshold (conditional) Gaussian samples
#'
#' @import condMVNorm
#' @export
#'
#' @author Martin Jullum
samp_Gauss_func <- function(given_ind, n_threshold, mu, Sigma, p, Xtest) {
    # Handles the unconditional and full conditional separtely when predicting
    if (length(given_ind) %in% c(0, p)) {
        ret <- matrix(Xtest, ncol = p, nrow = 1)
    } else {
        dependent_ind <- (1:length(mu))[-given_ind]
        X_given <- Xtest[given_ind]
        # ret0 <- condMVNorm::rcmvnorm(
        #     n = n_threshold,
        #     mean = mu,
        #     sigma = Sigma,
        #     dependent.ind = dependent_ind,
        #     given.ind = given_ind,
        #     X.given = X_given,
        #     method = "chol")
        tmp <- condMVNorm::condMVN(
            mean = mu,
            sigma = Sigma,
            dependent.ind = dependent_ind,
            given.ind = given_ind,
            X.given = X_given)
        #ret0 <- rmvnorm(n = n_threshold, mean = tmp$condMean, sigma = (tmp$condVar+t(tmp$condVar))/2, method = "chol")
        ret0 <- rmvnorm(n = n_threshold, mean = tmp$condMean, sigma = tmp$condVar, method = "chol")

        ret <- matrix(NA, ncol = p, nrow = n_threshold)
        ret[, given_ind] <- rep(X_given,each=n_threshold)
        ret[, dependent_ind] <- ret0
    }
    colnames(ret) <- colnames(Xtest)
    return(as.data.table(ret))
}

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
get_predictions <- function(model,
                            W_kernel,
                            S,
                            Xtrain,
                            Xtest,
                            w_threshold = .7,
                            n_threshold = 1e3,
                            verbose = FALSE,
                            cond_approach = "empirical",
                            feature_list,
                            pred_zero,
                            mu,
                            Sigma) {
    p <- ncol(Xtrain)

    if (cond_approach == "Gaussian") {
        ## Assume Gaussian distributed variables and sample from the various conditional distributions
        Gauss_samp <- lapply(
            X = feature_list,
            FUN = samp_Gauss_func,
            n_threshold = n_threshold,
            mu = mu,
            Sigma = Sigma,
            p = p,
            Xtest = Xtest
        )

        DTp <- rbindlist(Gauss_samp, idcol = "wcomb")
        DTp[, w := 1 / n_threshold]
        DTp[wcomb %in% c(1, 2 ^ p), w := 1] # Adjust weights for zero and full model
    } else {
        ## Get imputed data
        DTp <- impute_data(
            W_kernel = W_kernel,
            S = S,
            Xtrain = Xtrain,
            Xtest = Xtest,
            w_threshold = w_threshold,
            n_threshold = n_threshold
        )
    }

    ## Performing prediction
    nms <- colnames(Xtest)

    DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
    if(nrow(Xtest)==1){
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(rbind(Xtest,Xtest)))[1]] # Just a hack for a single prediction
    } else {
        DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(Xtest))]
    }
    DTp[wcomb == 1, p_hat := pred_zero]

    ## Get mean probability
    DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
    setkey(DTres, wcomb)

    return(DTres)
}


#' Predict on vector form
#'
#' @description Performs prediction of response for model classes lm, glm, ranger and xgboost with binary or continuous response.
#' Outpus the prediction on vector form. May let the user provide this function to handle any prediction model in the future.
#'
#' @inheritParams global_arguments
#' @param data data.table or data.frame with data to perform prediction
#' @return Vector of predictions
#'
#' @export
#'
#' @author Martin Jullum
pred_vector = function(model, data) {
    ## Figure out which model type we're using
    model_class <- head(class(model), 1)

    if (model_class == "glm") {
        if (model$family[[1]] == "binomial") {
            ret <- predict(model, newdata = data, type = "response")
        } else {
            ret <- predict(model, newdata = data)
        }
    }
    if (model_class == "lm") {
        ret <- predict(model, newdata = data)
    }
    if (model_class == "ranger") {
        if (model$treetype == "Probability estimation") {
            ret <- predict(model, data = data)$predictions[, 2]
        } else {
            ret <- predict(model, data = data)$predictions
        }
    }
    if (model_class == "xgb.Booster") {
        ret <- predict(model, newdata = as.matrix(data))
    }

    if (model_class == "gam"){
        ret <- predict(model,newdata = data)
    }

    return(ret)
}

#' Computes the kernelShap values for the test data given to prepare_kernelShao
#'
#' @inheritParams global_arguments
#' @param l The output from prepare_kernelShap
#' @param sigma Bandwidth in the Gaussian kernel if the empirical conditional sampling approach is used (cond_approach == "empirical")
#' @param pred_zero The prediction value for unseen data, typically equal to the mean of the response
#'
#' @return List with kernel Shap values (Kshap) and other object used to perform the computation (helpful for debugging etc.)
#'
#' @export
#'
#' @author Martin Jullum
compute_kernelShap = function(model,
                              l,
                              sigma = 0.1,
                              w_threshold = 0.95,
                              n_threshold = 1e3,
                              verbose = FALSE,
                              cond_approach = "empirical",
                              pred_zero,
                              kernel_metric = "Gaussian",
                              mu = NULL,
                              Sigma = NULL) {
    ll = list()

    # Handle the computation of all training-test weights for ALL combinations here, before looping
    if (kernel_metric == "independence"){
        W_kernel <- array(rnorm(prod(dim(l$D)))^2,dim = dim(l$D)) # Just random noise to "fake" a distance between observations
    }
    if (kernel_metric =="Gaussian"){
        val <- (-0.5*l$D)/sigma^2
        W_kernel <- exp(val) # To avoid numerical problems for small sigma values, we need to substract some constant from val here. Check if it is possible to do this per column/row of l$D[,i,]
    }
    if (kernel_metric =="Gaussian_old"){
        val <- (-0.5*l$D)/sigma^2
        W_kernel <- sqrt(exp(val)) # To avoid numerical problems for small sigma values
    }


    if(is.null(mu)){ # Using the mean of the training data in the Gaussian approach if not provided directly
        mu <- colMeans(l$Xtrain)
    }
    if(is.null(Sigma)){ # Using the sample covariance of the training data in the Gaussian approach if not provided directly
        Sigma <- stats::cov(l$Xtrain)
    }

    if (any(eigen(Sigma)$values <= 1e-06)) { # Make matrix positive definite if not, or close to not.
        Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
    }
    #Sigma <- (Sigma + t(Sigma))/2

    for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
        print(sprintf("%d out of %d", i, l$Xtest[, .N]))

        ll[[i]] <- get_predictions(
            model = model,
            W_kernel = W_kernel[,i,],
            S = l$S,
            Xtrain = as.matrix(l$Xtrain),
            Xtest = as.matrix(l$Xtest)[i, , drop = FALSE],
            w_threshold = w_threshold,
            n_threshold = n_threshold,
            verbose = verbose,
            cond_approach = cond_approach,
            feature_list = l$X$features,
            pred_zero = pred_zero,
            mu = mu,
            Sigma = Sigma)
        ll[[i]][, id := i]
    }

    DT <- rbindlist(ll)

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }

    ret_list = list(Kshap = Kshap, other_objects = list(ll = ll, DT = DT,W_kernel=W_kernel))
    return(ret_list)
}




#' Get shapley weights for test data
#'
#' @inheritParams global_arguments
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite
prepare_kernelShap <- function(m,
                               Xtrain,
                               Xtest,
                               exact = TRUE,
                               nrows = NULL,
                               scale = FALSE,
                               distance_metric = "Mahalanobis_scaled",
                               cond_approach = "empirical") {

    ## Get all combinations ----------------
    X <- get_combinations(m = m, exact = exact, nrows = nrows)

    ## Add weights ----------------
    X <- get_weights(X = X, m = m)

    ## Get weighted matrix ----------------
    W <- get_weighted_matrix(X)

    ## Transform to data table and scale data ----------------
    l <- scale_data(Xtrain, Xtest, scale = scale) # MJ: One should typically scale if using Euclidean, while it is no point when using Mahalanobis

    ## Get distance ---------
    if (distance_metric=="Euclidean"){
        mcov <- diag(m) # Should typically use scale if Euclidean is being used.
    } else { # i.e. If distance_metric == "Mahalanobis"
        mcov <- cov(Xtrain) # Move distance_metric if-test here and replace by diag(m) if "Euclidean" once you see everything works fine
    }

    if (distance_metric=="Mahalanobis_scaled"){
        S_scale_dist <- T
    } else {
        S_scale_dist <- F
    }

    if(cond_approach  == "empirical"){ # Only compute the distances if the empirical approach is used
        D <- gen_Mahlanobis_dist_cpp(X$features,as.matrix(Xtrain),as.matrix(Xtest),mcov=mcov,S_scale_dist = S_scale_dist) # This is D_S(,)^2 in the paper
    } else {
        D <- NULL
    }

    ## Get feature matrix ---------
    S <- feature_matrix_cpp(features = X[["features"]], nfeatures = ncol(Xtrain)) # The scaling of S influence onyl the matrix product with D, as we only care about the elements of S being zero/nonzero elsewhere.

    return(list(D = D, S = S, W = W, X = X, Xtrain = l$Xtrain, Xtest = l$Xtest))
}




