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

    if (exact == TRUE) {
        N <- 2^m
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
        X <- data.table(ID = seq(nrows),
                        nfeatures = sample(x = DT[["nfeatures"]],
                                         size = nrows,
                                         replace = TRUE,
                                         prob = DT[["weight"]]))

        ## Sample specific set of features ----------
        setkey(X, nfeatures)
        X[, ID := .I]
        X[, features := lapply(nfeatures, sample, x = 1:m)]

        ## Add zero features and m features ----------
        X_zero_all <- data.table(ID = seq(X[, max(ID)] + 1, length.out = 2),
                                 num_var = c(0, m),
                                 comb = c(list(numeric(0)), list(1:m)))
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
get_weights <- function(X) {
    X[-c(1, .N), weight := w_shapley(m = m, N = N, s = nfeatures), ID]
    X[c(1, .N) , weight := 10 ^ 6]

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
#' @param Xtrain data.frame
#' @param Xtest data.frame
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

#' Create imputed test data
#'
#' @param Xtrain data.table
#' @param Xtest data.table
#' @param features Postive integer vector
#' @param sigma Positive numeric
#' @param nsamples Positive integer
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
impute_data <- function(Xtrain, Xtest, features, sigma, nsamples) {

    ## Get distance for all combinations ---------
    if (missing(sigma) || sigma == 0) sigma <- 1.75 * (nrow(Xtrain)) ^ (-1 / 6)
    D <- distance_cpp(
        features = features,
        Xtrain = Xtrain,
        Xtest = Xtest,
        ncomb = length(features),
        sigma = sigma
    )


    D <- sample_unit_cpp(
        features = features,
        Xtrain = Xtrain,
        Xtest = Xtest[1, ],
        ncomb = length(features),
        sigma = sigma
    )

    ## Sample ids for all test observations ---------
    I <- sample_cpp(D, nsamples, length(features))

    ## Get imputed data ---------
    X <- impute_cpp(I, Xtrain, Xtest, features)
    colnames(X) <- c("test_id", "sample_id", names(Xtrain))

    return(as.data.table(X))
}

#' Get predictions
#'
#' @param DT data.table
#' @param ranger Logical
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
get_predictions <- function(model, DT, W, p_default = .5, ranger = TRUE) {

    ## Setup ----------------
    nfeatures <- nrow(W)
    ntest <- DT[, uniqueN(test_id)]
    kernShap <- matrix(0, ntest, nfeatures)

    ## Get predictions ----------
    if (ranger) {
        phat <- predict(model, data = DT, type = "response")$predictions[, 2]
    } else {
        phat <- predict(model, data = DT, type = "response")
    }

    DT[, phat := phat]
    predMat <- DT[, .(mphat = mean(phat)), .(test_id, sample_id)]

    for (k in 1:nrow(kernShap)) {
        kernShap[k, ] <- W %*% predMat[test_id == k, mphat]
    }

    return(list(kernShap = kernShap, DT = DT))
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
kernelShap <- function(m,
                       Xtrain,
                       Xtest,
                       nsamples,
                       exact = TRUE,
                       sigma,
                       nrows = NULL,
                       scale = FALSE) {

    ## Get all combinations ----------------
    X <- get_combinations(m = m, exact = exact, nrows = nrows)

    ## Add weights ----------------
    X <- get_weights(X = X)

    ## Get weighted matrix ----------------
    W <- get_weighted_matrix(X)

    ## Transform to data table and scale data ----------------
    S <- scale_data(Xtrain, Xtest, scale = scale)

    ## Get imputed data ---------
    DT <- impute_data(
        Xtrain = S$Xtrain,
        Xtest = S$Xtest,
        features = X[["features"]],
        nsamples = nsamples
    )

    return(list(DT = DT, W = W, X = X))
}

## Get predictions ----------------
# X <- get_predictions(
#     X = X,
#     model = model,
#     trainData = S$Xtrain,
#     testData = S$Xtest,
#     W = W,
#     sigma = sigma,
#     p_default = p_default,
#     nSamples = nSamples
# )
