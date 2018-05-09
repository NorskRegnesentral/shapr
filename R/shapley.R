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
get_prediction_data <- function(model, D, S, Xtrain, Xtest, sigma, w_threshold = .7, n_threshold = 1e3, verbose = FALSE) {

    ## Find weights for all combinations and training data
    if (verbose) print("Calculate weights for all traning - and feature combinations ")
    DT = as.data.table(weights_train_comb_cpp(D, S, sigma))
    DT[, ID := .I]
    DT = data.table::melt(data = DT, id.vars = "ID", variable.name = "comb", value.name = "w", variable.factor = FALSE)

    ## Remove training data with small weight
    if (verbose) print("Sort data.table by combination and weights")
    setkey(DT, comb, w)
    DT[, w := w/sum(w), comb]
    DT[, wcum := cumsum(w), comb]
    DT <- DT[wcum > 1 - w_threshold][, wcum := NULL]
    DT <- DT[, tail(.SD, n_threshold), comb]
    DT[, comb := gsub(comb, pattern = "V", replacement = ""), comb]
    DT[, wcomb := as.integer(comb), comb][, comb := NULL]

    ## Generate data used for prediction
    if (verbose) print("Create imputed prediction data")
    DTp <- impute_cpp(
        ID = DT[["ID"]],
        Comb = DT[["wcomb"]],
        Xtrain = Xtrain,
        Xtest = Xtest,
        S = S
    )

    if (verbose) print("Calculate predictions")
    DTp <- as.data.table(DTp)
    setnames(DTp, colnames(Xtrain))
    DTp[, wcomb := DT[["wcomb"]]]
    DTp[, w := DT[["w"]]]
    DTp[, p_hat := predict(model, .SD, num.threads = 5)$predictions[, 2]]
    DTres <- DTp[, .(k = sum((p_hat * w) / sum(w))), wcomb]
    setkey(DTres, wcomb)

    return(DTres)
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
                       exact = TRUE,
                       nrows = NULL,
                       scale = FALSE) {

    ## Get all combinations ----------------
    X <- get_combinations(m = m, exact = exact, nrows = nrows)

    ## Add weights ----------------
    X <- get_weights(X = X)

    ## Get weighted matrix ----------------
    W <- get_weighted_matrix(X)

    ## Transform to data table and scale data ----------------
    l <- scale_data(Xtrain, Xtest, scale = scale)

    ## Get distance ---------
    D <- distance_cpp(as.matrix(l$Xtrain), as.matrix(l$Xtest))

    ## Get feature matrix ---------
    S <- feature_matrix_cpp(features = X[["features"]], nfeatures = ncol(Xtrain))

    return(list(D = D, S = S, W = W, X = X, Xtrain = l$Xtrain, Xtest = l$Xtest))
}
