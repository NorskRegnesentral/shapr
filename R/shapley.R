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
#' @param nRows Integer
#'
#' @details
#' The returned data.table contains the following columns
#' \describe{
#' \item{ID}{Postive integer. Unique key for combination}
#' \item{comb}{List.}
#' \item{num_var}{Postive integer.}
#' \item{N}{Postive integer.}
#' }
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
get_combinations <- function(m, exact = TRUE, nRows = 200) {

    if (exact == TRUE) {
        N <- 2^m
        X <- data.table(ID = 1:N)
        all_combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
        X[, comb := unlist(all_combinations, recursive = FALSE)]
        X[, num_var := length(comb[[1]]), ID]
        X[, N := .N, num_var]
    } else {

        ## Find weights for given number of features ----------
        dt_feat <- data.table(num_var = head(1:m, -1))
        dt_feat[, N := unlist(lapply(num_var, choose, n = m))]
        dt_feat[, w := w_shapley(m = m, N = N, s = num_var)]

        ## Sample number of features ----------
        X <- data.table(ID = 1:nRows,
                        num_var = sample(x = dt_feat[["num_var"]],
                                         size = nRows,
                                         replace = TRUE,
                                         prob = dt_feat[["w"]]))

        ## Sample specific set of features ----------
        setkey(X, num_var)
        X[, ID := .I]
        X[, comb := lapply(num_var, sample, x = 1:m)]

        ## Add zero features and m features ----------
        X_zero_all <- data.table(ID = seq(X[, max(ID)] + 1, length.out = 2),
                                 num_var = c(0, m),
                                 comb = c(list(NULL), list(1:m)))
        X <- rbindlist(list(X, X_zero_all))
        setkey(X, num_var)
        X[, ID := .I]

        ## Add number of combinations
        X <- merge(x = X, y = dt_feat[, .(num_var, N)], all.x = TRUE, on = "num_var")

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
    X[-c(1, .N), w := w_shapley(m = m, N = N, s = num_var), ID]
    X[c(1, .N) , w := 10 ^ 6]

    return(X)
}

#' Get subset data
#'
#' @description Currently not used.
#'
#' @param X data.table.
#' @param nRows Integer.
#'
#' @return data.table
#'
#' @export
#'
#' @author Nikolai Sellereite
get_subset_data <- function(X, nRows) {
    d <- X[, .N] - 2
    if (nRows < d) {
        ind_sample <- X[-c(1, .N), .I] + 1
        X[, keep := 0]
        X[c(1, .N), keep := 1]
        ind_keep <- sample(x = ind_sample, size = nRows, replace = FALSE, prob = X[ind_sample, w])
        X[ind_keep, keep := 1]
        X <- X[keep == 1][, keep := NULL]
    }
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
        comb = X[["comb"]],
        w = X[["w"]],
        m = X[.N][["num_var"]],
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
scale_data <- function(Xtrain, Xtest) {
    Xtrain <- trainData
    Xtest <- testData
    if (!is.data.table(Xtrain)) {
        Xtrain <- as.data.table(Xtrain)
    }
    if (!is.data.table(Xtest)) {
        Xtest <- as.data.table(Xtest)
    }
    nms <- colnames(Xtrain)
    setcolorder(Xtest, nms)
    sd <- unlist(Xtrain[, lapply(.SD, sd, na.rm = TRUE)])
    Xtrain[, (nms) := .SD / lapply(.SD, sd, na.rm = TRUE)]
    Xtest[, (nms) := .SD / sd, .SDcols = nms]

    return(list(Xtrain = Xtrain, Xtest = Xtest))
}

#' Create imputed test data
#'
#' @param I Array
#' @param trainData data.table
#' @param testData data.table
#' @param model_features Postive integer vector
#' @param which_comb Positive integer.
#'
#' @return Matrix
#'
#' @export
#'
#' @author Nikolai Sellereite
impute_data <- function(I, trainData, testData) {

    X <- impute_cpp(
        I = I,
        train = trainData,
        test = testData
    )

    colnames(X) <- c("ID", names(trainData))

    return(X)
}

#' Get predictions
#'
#' @param X data.table
#' @inheritParams global_arguments
#'
#' @return List
#'
#' @export
#'
#' @author Nikolai Sellereite
get_predictions <- function(X,
                            model,
                            trainData,
                            testData,
                            W,
                            sigma = 0,
                            p_default = .5,
                            nSamples = 100) {

    ## Setup ----------------
    l <- list()
    nfeatures <- nrow(W)
    ntest <- nrow(testData)
    ntrain <- nrow(trainData)
    kernShap <- matrix(0, ntest, nfeatures)
    if (sigma == 0) sigma <- 1.75 * (ntrain) ^ (-1 / 6)

    ## Get distance for all combinations ---------
    D <- distance_cpp(
        comb = X[["comb"]],
        train = trainData,
        test = testData,
        ncomb = X[, .N],
        sigma = sigma
    )

    ## Sample ids for all test observations ---------
    I <- sample_cpp(
        X = D,
        nSamples = nSamples,
        ncomb = X[, .N]
    )

    ## Get imputed data ---------
    nms <- tail(names(model$coefficients), -1)
    nms_ind <- which(colnames(trainData) %in% nms)
    impute_data <- impute_data(
        I = I,
        train = trainData,
        test = testData
    )

    ## Get predictions ----------
    phat <- predict(model, data = impute_data, type = "response")
    yMatTot <- as.data.table(impute_data)
    yMatTot[, phat := phat]
    predMat <- yMatTot[, .(mphat = mean(phat)), ID][order(ID)]
    predMat[1, mphat := p_default]

    l[[k]] <- predMat[, k := k]
    kernelShapley[k, ] <- W %*% predMat[, mphat]


    p_dt <- rbindlist(l, use.names = TRUE)

    list(kernelShapley = kernelShapley, p_dt = p_dt)
}


# for (k in 1:nrow(testData)) {
#
#     ## Print ---------
#     print(sprintf("%d out of %d", k, nrow(testData)))
#
#     # if (ranger) {
#     #     yMatTot[, phat := predict(model, data = .SD, type = "response")$predictions[, 2]]
#     # } else {
#     #     yMatTot[, phat := unname(predict(model, data = .SD, type = "response"))]
#     # }
# }

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
                       trainData,
                       testData,
                       nSamples,
                       p_default,
                       exact = TRUE,
                       nRows,
                       sigma = NULL,
                       model) {

    ## Get all combinations ----------------
    X <- get_combinations(m = m, exact = exact)

    ## Add weights ----------------
    X <- get_weights(X)

    ## Get weighted matrix ----------------
    W <- get_weighted_matrix(X)

    ## Transform to data table and scale data ----------------
    S <- scale_data(trainData, testData)

    ## Get predictions ----------------
    X <- get_predictions(
        X = X,
        model = model,
        trainData = S$Xtrain,
        testData = S$Xtest,
        W = W,
        sigma = sigma,
        p_default = p_default,
        nSamples = nSamples
    )

    return(X)
}

# get_weights <- function(x, d, trainData) {
#
#     N <- length(x)
#     if (N == 0) {
#         distK <- rep(0, nrow(trainData))
#     } else {
#
#         inds <- x
#         d <- unlist(d[, .SD, .SDcols = inds])
#         nms <- colnames(trainData)[inds]
#         if (N == 1) {
#             distK <- (trainData[, .SD, .SDcols = nms] - d) ^ 2
#             distK <- distK[, 1]
#         } else {
#             distK <- trainData[, .SD, .SDcols = nms] - d
#             distK[, (nms) := lapply(.SD, function(i)(i)^2), .SDcols = nms]
#             distK <- distK[, rowSums(.SD), .SDcols = nms]
#         }
#     }
#
#     obsWeights <- sqrt(exp(-distK/(2*sigma^2)))
#
#     if (max(obsWeights) < 0.00001) {
#         obsWeights[] <- 1/trainData[, .N]
#     }
#     return(obsWeights)
# }
#
#
# impute_train <- function(prob, comb, d) {
#     ind <- sample(
#         x = nrow(trainData),
#         nSamples,
#         replace = TRUE,
#         # prob = prob
#         prob = prob[[1]]
#     )
#     comb <- comb[[1]]
#     nms <- colnames(trainData)[comb]
#     yMat <- copy(trainData[ind])
#     if (length(nms) > 0) {
#         yMat[, (nms) := d[, .SD, .SDcols = nms]]
#     }
#
#     return(yMat)
# }
