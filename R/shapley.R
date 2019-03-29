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
#' @param replace Logical Whether to do sampling with replacement (if exact==FALSE)
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
#' @author Nikolai Sellereite, Martin Jullum
get_combinations <- function(m, exact = TRUE, nrows = 200, replace = TRUE, shapley_weight_inf_replacement = 10^6,reduce_dim = T) {

    if (!exact && nrows>(2^m-2) && !replace ){
        nrows = 2^m-2
        cat(paste0("nrows is larger than 2^m = ",2^m,". Using exact instead."))
    }
    if (exact == TRUE) {
        N <- 2 ^ m
        X <- data.table(ID = 1:N)
        combinations <- lapply(0:m, utils::combn, x = m, simplify = FALSE)
        X[, features := unlist(combinations, recursive = FALSE)]
        X[, nfeatures := length(features[[1]]), ID]
        X[, N := .N, nfeatures]
        X[!(nfeatures%in%c(0,m)), shapley_weight := w_shapley(m = m, N = N, s = nfeatures)]
        X[nfeatures%in% c(0,m), shapley_weight := shapley_weight_inf_replacement]
        X[, no := 1]
    } else {
        if (replace){
            ## Find weights for given number of features ----------
            DT0 <- data.table(nfeatures = head(1:m, -1))
            DT0[, N := unlist(lapply(nfeatures, choose, n = m))]
            DT0[, shapley_weight := w_shapley(m = m, N = N, s = nfeatures)]
            DT0[, samp_weight := shapley_weight*N]
            DT0[, samp_weight := samp_weight/sum(samp_weight)]

            ## Sample number of features ----------
            X <- data.table(
                nfeatures = sample(
                    x = DT0[["nfeatures"]],
                    size = nrows,
                    replace = TRUE,
                    prob = DT0[["samp_weight"]]
                )
            )

            ## Sample specific set of features # Not optimal, as it is a bit slow for nrows -------
            setkey(X, nfeatures)
            Samp <- sapply(X=X$nfeatures,FUN = function(x){aa <- rep(NA,m);aa[1:x]=sample(x=1:m,size = x);aa})
            Samp <- t(apply(X = Samp,MARGIN = 2,FUN=sort,na.last=T))
            Samp.list <- apply(X=Samp,MARGIN=1,FUN=function(x){x[!is.na(x)]})

            X <- cbind(X,Samp)
            X[,no:=.N,by=mget(paste0("V",1:m))] # Counting repetitions of the same sample

            if(reduce_dim){
                isDup <- duplicated(X)
                X[,features:=Samp.list]
                X <- X[!isDup,]
            } else {
                X[,no:=1]
                X[,features:=Samp.list]
            }

            X[,paste0("V",1:m):= NULL]
            X[,ID:=.I]

            nms <- c("ID", "nfeatures", "features", "no")
            setcolorder(X, nms)

            ## Add zero features and m features ----------
            X_zero_all <- data.table(
                ID = seq(X[, max(ID)] + 1, length.out = 2),
                num_var = c(0, m),
                comb = c(list(numeric(0)), list(1:m)),
                no = 1
            )
            X <- rbindlist(list(X, X_zero_all))
            setkey(X, nfeatures)

            ## Add number of combinations
            X <- merge(x = X, y = DT0[, .(nfeatures, N,shapley_weight)], all.x = TRUE, on = "nfeatures")
            nms <- c("ID", "features", "nfeatures", "N","shapley_weight","no")
            setcolorder(X, nms)
            X[,ID:=.I]
            X[nfeatures%in% c(0,m), ':='(shapley_weight = shapley_weight_inf_replacement,
                                         N = 1)]

        } else {
            cat(paste0("replace= FALSE no long supported..."))
        }
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
#' @author Nikolai Sellereite, Martin Jullum
get_weighted_matrix <- function(X,use_shapley_weights_in_W = T, normalize_W_weights = T) {
    if (use_shapley_weights_in_W){
        w <- X[["shapley_weight"]]*X[["no"]]
    } else {
        w <- X[["no"]]
        w[c(1,length(w))] <- X[["shapley_weight"]][c(1,length(w))]
    }

if (normalize_W_weights){
    w[-c(1,length(w))] <- w[-c(1,length(w))]/sum(w[-c(1,length(w))])
}

    W <- weighted_matrix(
        features = X[["features"]],
        m = X[.N][["nfeatures"]],
        n = X[, .N],
        w = w
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


#' Transforms new data to standardized normal (dimension 1) based on other datas transformation
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param zx Vector where the first part is the Gaussian data, and last part is
#' the data with the original transformation
#' @param n_y How many elements of the yx vector that belongs to the y-part (new data)
#' @param type The quantile type used when back-transforming. 7 (default) is the default in quantile().
#'
#' @return Vector of transformed new data
#' @export
#'
#' @author Martin Jullum
inv_Gauss_trans_func <- function(zx,n_z,type=7){
    z <- zx[1:n_z]
    x <- zx[-(1:n_z)]
    u <- pnorm(z)
    xNew <- quantile(x,u,type=type)
    return(xNew)
}


#' Transforms new data to standardized normal (dimension 1) based on other datas transformation
#'
#' Handled in this way in order to allow using the apply function over this function
#' @param yx Vector where the first part is the new data to transform,
#' and last part is the data with the original transformation
#' @param n_z How many elements of the zx vector that belongs to the z-part (Gaussian data)
#'
#' @return Vector of back-transformed Gaussian data
#' @export
#'
#' @author Martin Jullum
Gauss_trans_func_seperate <- function(yx,n_y){
    y <- yx[1:n_y]
    x <- yx[-(1:n_y)]
    tmp <- rank(c(y,x))[1:length(y)]
    tmp <- tmp -rank(tmp)+0.5
    u.y <- tmp/(length(x)+1)
    z.y <- qnorm(u.y)
    return(z.y)
}

#' Transforms a sample to standardized normal (dimension 1)
#'
#' @param x Vector of data to transform
#'
#' @return Vector of transformed data
#' @export
#'
#' @author Martin Jullum
Gauss_trans_func <- function(x){
    u <- rank(x)/(length(x)+1)
    z <- qnorm(u)
    return(z)
}


#' Sample conditional variables using the Gaussian copula approach
#'
#' @param given_ind Vector
#' @param p Positive integer
#'
#' @inheritParams global_arguments
#'
#' @return data.table with n_threshold (conditional) Gaussian samples
#'
#' @import condMVNorm
#' @export
#'
#' @author Martin Jullum
samp_copula_func <- function(given_ind, n_threshold, mu, Sigma, p, Xtest_Gauss_trans,Xtrain,Xtest) {
    # Handles the unconditional and full conditional separtely when predicting
    if (length(given_ind) %in% c(0, p)) {
        ret <- matrix(Xtest, ncol = p, nrow = 1)
    } else {
        dependent_ind <- (1:length(mu))[-given_ind]
        X_given <- Xtest_Gauss_trans[given_ind]
        X_given_orig <- Xtest[given_ind]
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
        ret0_z <- rmvnorm(n = n_threshold, mean = tmp$condMean, sigma = tmp$condVar, method = "chol")

        ret0_x <- apply(X = rbind(ret0_z,Xtrain[,dependent_ind,drop=F]),MARGIN=2,FUN=inv_Gauss_trans_func,n_z = n_threshold)

        ret <- matrix(NA, ncol = p, nrow = n_threshold)
        ret[, given_ind] <- rep(X_given_orig,each=n_threshold)
        ret[, dependent_ind] <- ret0_x
    }
    colnames(ret) <- colnames(Xtest)
    return(as.data.table(ret))
}


#' Sample conditional Gaussian variables
#'
#' @param given_ind Vector
#' @param p Positive integer
#'
#' @inheritParams global_arguments
#'
#' @return data.table with n_threshold (conditional) Gaussian samples
#'
#' @import condMVNorm
#' @export
#'
#' @author Martin Jullum
samp_Gauss_func <- function(given_ind, n_threshold, mu, Sigma, p, Xtest,ensure_condcov_symmetry=F) {
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
        if (!ensure_condcov_symmetry){
            tmp <- condMVNorm::condMVN(
                mean = mu,
                sigma = Sigma,
                dependent.ind = dependent_ind,
                given.ind = given_ind,
                X.given = X_given)

        } else {
            tmp <- condMVN_modified(
                mean = mu,
                sigma = Sigma,
                dependent.ind = dependent_ind,
                given.ind = given_ind,
                X.given = X_given)
        }
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
                            D,
                            h_optim_vec,
                            kernel_metric,
                            S,
                            Xtrain,
                            Xtest,
                            w_threshold = .7,
                            n_threshold = 1e3,
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
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(S)))]

        samp_list <- lapply(
            X = feature_list[these_wcomb],
            FUN = samp_Gauss_func,
            n_threshold = n_threshold,
            mu = mu,
            Sigma = Sigma,
            p = p,
            Xtest = Xtest,
            ensure_condcov_symmetry = ensure_condcov_symmetry
        )

        DTp.Gaussian <- rbindlist(samp_list, idcol = "wcomb")
        DTp.Gaussian[,wcomb:=these_wcomb[wcomb]]  # Correcting originally assigned wcomb
        DTp.Gaussian[, w := 1 / n_threshold]
    }
    if ("copula" %in% names(cond_approach_list)){
        these_wcomb <- cond_approach_list$copula
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(S)))]


        samp_list <- lapply(
            X = feature_list[these_wcomb],
            FUN = samp_copula_func,
            n_threshold = n_threshold,
            mu = mu_Gauss_trans,
            Sigma = Sigma_Gauss_trans,
            p = p,
            Xtest_Gauss_trans = Xtest_Gauss_trans,
            Xtrain = Xtrain,
            Xtest = Xtest
        )

        DTp.copula <- rbindlist(samp_list, idcol = "wcomb")
        DTp.copula[,wcomb:=these_wcomb[wcomb]]  # Correcting originally assigned wcomb
        DTp.copula[, w := 1 / n_threshold]

    }

    if ("empirical" %in% names(cond_approach_list)){
        these_wcomb <- cond_approach_list$empirical
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(S)))]

        no_wcomb <- length(these_wcomb)

        # Handle the computation of all training-test weights for ALL combinations here, before looping
        if (kernel_metric == "independence"){
            W_kernel <- array(runif(no_wcomb*nrow(Xtrain)),dim=c(nrow(Xtrain),no_wcomb)) # Just random noise to "fake" a distance between observations
        }
        if (kernel_metric =="Gaussian"){
            val <- t(t(-0.5*D)/h_optim_vec^2)
            W_kernel <- exp(val) # To avoid numerical problems for small sigma values, we need to substract some constant from val here. Check if it is possible to do this per column/row of l$D[,i,]
        }

        ## Get imputed data
        DTp.empirical <- impute_data(
            W_kernel = W_kernel,
            S = S[these_wcomb,],
            Xtrain = Xtrain,
            Xtest = Xtest,
            w_threshold = w_threshold,
            n_threshold = n_threshold
        )
        DTp.empirical[,wcomb:=these_wcomb[wcomb]]  # Correcting originally assigned wcomb

    }

    ## Performing prediction
    nms <- colnames(Xtest)

    DTp <- rbind(DTp.Gaussian,DTp.copula,DTp.empirical)
    DTp <- merge(DTp,data.table(wcomb=c(1,2^p),w=1),all=T)
    setkey(DTp,wcomb)


    DTp[!(wcomb %in% c(1, 2 ^ p)), p_hat := pred_vector(model = model, data = .SD), .SDcols = nms]
    DTp[wcomb == 2 ^ p, p_hat := pred_vector(model = model, data = as.data.frame(Xtest))]
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
                              w_threshold = 0.95,
                              n_threshold = 1e3,
                              verbose = FALSE,
                              cond_approach = "empirical", # When being a list, the elements in the list refers to the rows in l$X that ought to be included in each of the approaches!
                              empirical_settings = list(type = "fixed_sigma", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                                                        fixed_sigma_vec = 0.1, # May be a vector of length nrow(S), or a single number used for all
                                                        AICc_no_samp_per_optim = 1000,
                                                        AIC_optim_max_eval = 20,
                                                        AIC_optim_startval = 0.1),
                              pred_zero,
                              mu = NULL,
                              Sigma = NULL,
                              ensure_condcov_symmetry = F) {
    tt <- proc.time()

    ll = list()

    if(is.character(cond_approach)){
        cond_approach_list <- list(1:nrow(l$S))
        names(cond_approach_list) <- cond_approach
    }
    if(is.list(cond_approach)){
        cond_approach_list <- cond_approach
    }

    if("empirical" %in% names(cond_approach_list)){


        these_empirical <- cond_approach_list$empirical
        exclude_emp <- (these_empirical %in% c(1,nrow(l$S)))

        these_empirical <- these_empirical[!exclude_emp]

        no_empirical <- length(these_empirical)
        h_optim_mat <- matrix(NA,ncol=nrow(l$Xtest),nrow=no_empirical) # Each test observation has one column

        # Checking whether any of the distance are not pre-computed.
        if(any(!(these_empirical %in% l$D_for_these_varcomb))){
            paste0("Distance not pre-computed for varcomb ",paste0(these_empirical[!(these_empirical %in% l$D_for_these_varcomb)],collapse=", "))
        }

        # Reducing and re-ordering the D-array
        l$D <- l$D[,,match(these_empirical, l$D_for_these_varcomb)] # Now the D-array corresponds to exactly the covariate combinations specified in these_empirical


        if(empirical_settings$type == "independence"){
            kernel_metric <- "independence"
        }  else {
            kernel_metric <- "Gaussian"

            if (empirical_settings$type == "fixed_sigma"){
                if(length(empirical_settings$fixed_sigma_vec)==1){
                    empirical_settings$fixed_sigma_vec <- rep(empirical_settings$fixed_sigma_vec,no_empirical)
                } else {
                    empirical_settings$fixed_sigma_vec <- empirical_settings$fixed_sigma_vec[!exclude_emp]
                }

                h_optim_mat[,] <- empirical_settings$fixed_sigma_vec

            } else {

                #### Procedure for sampling a combination of an index in the training and the test sets ####
                optimsamp <- samp_train_test_comb(nTrain = nrow(l$Xtrain),
                                                  nTest = nrow(l$Xtest),
                                                  nosamp = empirical_settings$AICc_no_samp_per_optim,
                                                  separate = T)

                empirical_settings$AICc_no_samp_per_optim <- nrow(optimsamp) # Updating this parameter (if larger than nTrain*nTest)

                nloops <- nrow(l$Xtest)

                ### Include test here that empirical settings is defined as it should be

                if (empirical_settings$type == "AICc_each_k"){ # This means doing optimization only once for all distributions which conditions on exactly k variables

                    these_k <- unique(l$X$nfeatures[these_empirical])

                    for (i in these_k){

                        these_cond <- l$X[ID%in% these_empirical][nfeatures==i,ID]

                        cutters <- 1:empirical_settings$AICc_no_samp_per_optim
                        no_cond <- length(these_cond)

                        cond_samp <- cut(cutters,quantile(cutters,(0:no_cond)/no_cond), include.lowest=TRUE, labels=these_cond)
                        cond_samp <- as.numeric(levels(cond_samp))[cond_samp]


                        for (loop in 1:nloops){
                            this.optimsamp <- optimsamp
                            this.optimsamp$samp_test <- loop

                            j <- 1
                            X_list <- X.pred.list <- mcov_list <- list()
                            for (this_cond in unique(cond_samp)){

                                these_inds <- which(cond_samp==this_cond)
                                these_train <- this.optimsamp$samp_train[these_inds]
                                these_test <- this.optimsamp$samp_test[these_inds]

                                # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
                                these_train <- 1:nrow(l$Xtrain)
                                these_test <- sample(x = these_test,size = nrow(l$Xtrain),replace = T)
                                current_cond_samp <- rep(unique(cond_samp),each=nrow(l$Xtrain))

                                S <- l$S[this_cond,]

                                S.cols <- which(as.logical(S))
                                Sbar.cols <- which(as.logical(1-S))

                                X_list[[j]] <- as.matrix(subset(l$Xtrain,select=S.cols)[these_train,])
                                mcov_list[[j]] <- cov(X_list[[j]])

                                Xtrain.Sbar <- subset(l$Xtrain,select=Sbar.cols)[these_train,]
                                Xtest.S <- subset(l$Xtest,select=S.cols)[these_test,]
                                X.pred.list[[j]] <- cbind(Xtrain.Sbar,Xtest.S)

                                j <- j + 1
                            }

                            # Combining the X's for doing prediction
                            X.pred <- rbindlist(X.pred.list,use.names=T)
                            X.nms <- colnames(l$Xtrain)
                            setcolorder(X.pred,X.nms)
                            # Doing prediction jointly (for speed), and then splitting them back into the y_list
                            pred <- pred_vector(model=model,data=X.pred)
                            y_list = split(pred,current_cond_samp)
                            names(y_list) = NULL


                            ## Doing the numerical optimization -------
                            nlm.obj <- suppressWarnings(nlminb(start = empirical_settings$AIC_optim_startval,
                                                               objective = AICc_full_cpp_alt,
                                                               X_list = X_list,
                                                               mcov_list = mcov_list,
                                                               S_scale_dist = T,
                                                               y_list = y_list,
                                                               negative = F,
                                                               lower = 0,
                                                               control=list(eval.max=empirical_settings$AIC_optim_max_eval,
                                                                            trace=verbose)))



                            h_optim_mat[match(these_cond,these_empirical),loop] <- nlm.obj$par


                        }

                    }
                }

                if (empirical_settings$type == "AICc_full"){

                    for (i in these_empirical){

                        S <- l$S[i,]

                        S.cols <- which(as.logical(S))
                        Sbar.cols <- which(as.logical(1-S))

                        for (loop in 1:nloops){
                            this.optimsamp <- optimsamp
                            this.optimsamp$samp_test <- loop

                            these_train <- this.optimsamp$samp_train
                            these_test <- this.optimsamp$samp_test

                            # Hacky way to handle the situation when optimizing in the usual way. Needs to be improved!
                            these_train <- 1:nrow(l$Xtrain)
                            these_test <- sample(x = these_test,size = nrow(l$Xtrain),replace = T)

                            X_list <- list(as.matrix(subset(l$Xtrain,select=S.cols)[these_train,]))
                            mcov_list <- list(cov(X_list[[1]]))

                            Xtrain.Sbar <- subset(l$Xtrain,select=Sbar.cols)[these_train,]
                            Xtest.S <- subset(l$Xtest,select=S.cols)[these_test,]
                            X.pred <- cbind(Xtrain.Sbar,Xtest.S)

                            X.nms <- colnames(l$Xtrain)
                            setcolorder(X.pred,X.nms)

                            pred <- pred_vector(model=model,data=X.pred)
                            y_list <- list(pred)

                            ## Running the nonlinear optimization

                            nlm.obj <- suppressWarnings(nlminb(start = empirical_settings$AIC_optim_startval,
                                                               objective = AICc_full_cpp_alt,
                                                               X_list = X_list,
                                                               mcov_list = mcov_list,
                                                               S_scale_dist = T,
                                                               y_list = y_list,
                                                               negative = F,
                                                               lower = 0,
                                                               control=list(eval.max=empirical_settings$AIC_optim_max_eval,
                                                                            trace=verbose)))


                            h_optim_mat[match(i,these_empirical),loop] <- nlm.obj$par
                        }

                    }

                }

            }

        }

        h_optim_DT <- data.table(varcomb=these_empirical,h_optim_mat)
        colnames(h_optim_DT)[-1] <- paste0("Testobs_",1:nrow(l$Xtest))
    } else {
        h_optim_mat <- NULL
        h_optim_DT <- NULL
    }

    #    if(sum(grepl("AICc",names(cond_approach_list)))>0){}



    if(is.null(mu)){ # Using the mean of the training data in the Gaussian approach if not provided directly
        mu <- colMeans(l$Xtrain)
    }
    if(is.null(Sigma)){ # Using the sample covariance of the training data in the Gaussian approach if not provided directly
        Sigma <- stats::cov(l$Xtrain)
    }

    if (any(eigen(Sigma)$values <= 1e-06)) { # Make matrix positive definite if not, or close to not.
        Sigma <- as.matrix(Matrix::nearPD(Sigma)$mat)
        #Sigma <- solve(symmpart(solve(Sigma))) # No point apparently
    }
    Xtest.mat <- as.matrix(l$Xtest)
    Xtrain.mat <- as.matrix(l$Xtrain)

    # Only needed for copula method, but is not time consuming anyway
    Xtrain_Gauss_trans <- apply(X = l$Xtrain,MARGIN = 2,FUN=Gauss_trans_func)
    Xtest_Gauss_trans <- apply(X = rbind(l$Xtest,l$Xtrain),MARGIN = 2,FUN=Gauss_trans_func_seperate,n_y = nrow(l$Xtest))

    mu_Gauss_trans <- rep(0,ncol(l$Xtrain))
    Sigma_Gauss_trans <- stats::cov(Xtrain_Gauss_trans)
    if (any(eigen(Sigma_Gauss_trans)$values <= 1e-06)) {
        Sigma_Gauss_trans <- as.matrix(Matrix::nearPD(Sigma_Gauss_trans)$mat)
    }

    for (i in l$Xtest[, .I]) { # This may be parallelized when the prediction function is not parallelized.
        if(verbose>0){
            print(sprintf("%d out of %d", i, l$Xtest[, .N]))
        }

        ll[[i]] <- get_predictions(
            model = model,
            D = l$D[,i,],
            h_optim_vec = h_optim_mat[,i],
            kernel_metric = kernel_metric,
            S = l$S,
            Xtrain = Xtrain.mat,
            Xtest = Xtest.mat[i, , drop = FALSE],
            w_threshold = w_threshold,
            n_threshold = n_threshold,
            verbose = verbose,
            cond_approach_list = cond_approach_list,
            feature_list = l$X$features,
            pred_zero = pred_zero,
            mu = mu,
            Sigma = Sigma,
            mu_Gauss_trans = mu_Gauss_trans,
            Sigma_Gauss_trans = Sigma_Gauss_trans,
            Xtest_Gauss_trans = Xtest_Gauss_trans[i,,drop=FALSE],
            ensure_condcov_symmetry = ensure_condcov_symmetry)
        ll[[i]][, id := i]
    }

    DT <- rbindlist(ll)

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }



    tt <- proc.time()-tt

    ret_list = list(Kshap = Kshap, other_objects = list(ll = ll, DT = DT,h_optim_DT=h_optim_DT,comp_time = tt))
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
                               shapley_weight_inf_replacement = 10^6,
                               compute_distances_for_no_var = 0:m){ # Set to NULL if no distances are to be computed

    ## Convert data to data.table format --------------
    if (!is.data.table(Xtrain)) {
        Xtrain <- as.data.table(Xtrain)
    }
    if (!is.data.table(Xtest)) {
        Xtest <- as.data.table(Xtest)
    }

    ## Get all combinations ----------------
    X <- get_combinations(m = m, exact = exact, nrows = nrows, replace = T, shapley_weight_inf_replacement = shapley_weight_inf_replacement, reduce_dim = T)

    ## Get weighted matrix ----------------
    W <- get_weighted_matrix(X, use_shapley_weights_in_W = ifelse(exact,T,F), normalize_W_weights = T)

    mcov <- cov(Xtrain) # Move distance_metric if-test here and replace by diag(m) if "Euclidean" once you see everything works fine

    if(!is.null(compute_distances_for_no_var[1])){ # Only compute the distances if the empirical approach is used
        D <- gen_Mahlanobis_dist_cpp(featureList = X[nfeatures%in%compute_distances_for_no_var,features],
                                     Xtrain = as.matrix(Xtrain),
                                     Xtest = as.matrix(Xtest),
                                     mcov=mcov,
                                     S_scale_dist = T) # This is D_S(,)^2 in the paper

        ## Normalize the distance rows to ensure numerical stability in later operations
        colmin <- apply(X = D,MARGIN = c(2,3),FUN=min)
        for(i in 1:dim(D)[3]){
            D[,,i] <- t(t(D[,,i])-colmin[,i])
        }
    } else {
        D <- NULL
    }


    ## Get feature matrix ---------
    S <- feature_matrix_cpp(features = X[["features"]],
                            nfeatures = ncol(Xtrain))

    return(list(D = D, S = S, W = W, X = X, Xtrain = Xtrain, Xtest = Xtest,
                D_for_these_varcomb = X[nfeatures%in%compute_distances_for_no_var,which=TRUE]))
}




