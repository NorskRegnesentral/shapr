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

    if (!exact && nrows>2^m){
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
                            Xtest_Gauss_trans) {
    p <- ncol(Xtrain)

    DTp.Gaussian <- DTp.copula <- DTp.empirical <- NULL

    if ("Gaussian" %in% names(cond_approach_list)) {
        ## Assume Gaussian distributed variables and sample from the various conditional distributions
        these_wcomb <- cond_approach_list$Gaussian
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(l$S)))]

        samp_list <- lapply(
            X = feature_list[these_wcomb],
            FUN = samp_Gauss_func,
            n_threshold = n_threshold,
            mu = mu,
            Sigma = Sigma,
            p = p,
            Xtest = Xtest
        )

        DTp.Gaussian <- rbindlist(samp_list, idcol = "wcomb")
        DTp.Gaussian[,wcomb:=these_wcomb[wcomb]]  # Correcting originally assigned wcomb
        DTp.Gaussian[, w := 1 / n_threshold]
    }
    if ("copula" %in% names(cond_approach_list)){
        these_wcomb <- cond_approach_list$copula
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(l$S)))]


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
        these_wcomb <- these_wcomb[!(these_wcomb %in% c(1,nrow(l$S)))]


        # Handle the computation of all training-test weights for ALL combinations here, before looping
        if (kernel_metric == "independence"){
            W_kernel <- array(runif(length(these_wcomb)*nrow(Xtrain)),dim=c(nrow(Xtrain),length(these_wcomb))) # Just random noise to "fake" a distance between observations
        }
        if (kernel_metric =="Gaussian"){
            val <- t(t(-0.5*D[,these_wcomb])/h_optim_vec[these_wcomb]^2)
            W_kernel <- exp(val) # To avoid numerical problems for small sigma values, we need to substract some constant from val here. Check if it is possible to do this per column/row of l$D[,i,]
        }
        if (kernel_metric =="Gaussian_old"){
            val <- t(t(-0.5*D[,these_wcomb])/h_optim_vec[these_wcomb]^2)
            W_kernel <- sqrt(exp(val))
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
                              cond_approach = "empirical",
                              empirical_settings = list(type = "fixed_sigma", # May in the future allow a vector of length nrow(S) here as well to specify fixed for some and optimiziation for others
                                                        fixed_sigma_vec = 0.1, # May be a vector of length nrow(S), or a single number used for all
                                                        AICc_no_samp_per_optim = NULL,
                                                        AICc_optimize_every_testobs = F,
                                                        AIC_optim_func = "nlminb", # only "nlminb" allowed for now
                                                        AIC_optim_max_eval = 20,
                                                        AIC_optim_startval = 0.1,
                                                        kernel_metric = "Gaussian"),
                              pred_zero,
                              mu = NULL,
                              Sigma = NULL) {
    ll = list()

    if(is.character(cond_approach)){
        cond_approach_list <- list(1:nrow(l$S))
        names(cond_approach_list) <- cond_approach
    }
    if(is.list(cond_approach)){
        cond_approach_list <- cond_approach
    }

    h_optim_mat <- matrix(NA,ncol=nrow(l$Xtest),nrow=nrow(l$S)) # Each test observation has one column


    if("empirical" %in% names(cond_approach_list)){

        these_empirical <- cond_approach_list$empirical
        these_empirical <- these_empirical[!(these_empirical %in% c(1,nrow(l$S)))]

        if(empirical_settings$type == "independence"){
            empirical_settings$kernel_metric <- "independence" # Overriding the kernel_metric setting if type is set to "independence"
        }  else if (empirical_settings$type == "fixed_sigma"){
            if(length(empirical_settings$fixed_sigma_vec)==1){
                empirical_settings$fixed_sigma_vec = rep(empirical_settings$fixed_sigma_vec,nrow(l$S))
            }

            h_optim_mat[these_empirical,] <- empirical_settings$fixed_sigma_vec[these_empirical]
        } else {

            #### Procedure for sampling a combination of an index in the training and the test sets ####
            optimsamp <- samp_train_test_comb(nTrain = nrow(l$Xtrain),
                                              nTest = nrow(l$Xtest),
                                              nosamp = empirical_settings$AICc_no_samp_per_optim)

            nloops <- ifelse(empirical_settings$AICc_optimize_every_testobs,yes = nrow(l$Xtest),no = 1)

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
                        if (empirical_settings$AICc_optimize_every_testobs){
                            this.optimsamp$samp_test <- loop
                        }

                        j <- 1
                        Xtrain.S.list <- X.pred.list <- list()
                        for (this_cond in unique(cond_samp)){

                            these_inds <- which(cond_samp==this_cond)

                            S <- l$S[this_cond,]

                            S.cols <- which(as.logical(S))
                            Sbar.cols <- which(as.logical(1-S))

                            Xtrain.S.list[[j]] <- subset(l$Xtrain,select=S.cols)[this.optimsamp$samp_train[these_inds],]
                            Xtrain.Sbar <- subset(l$Xtrain,select=Sbar.cols)[this.optimsamp$samp_train[these_inds],]
                            Xtest.S <- subset(l$Xtest,select=S.cols)[this.optimsamp$samp_test[these_inds],]

                            X.pred.list[[j]] <- cbind(Xtrain.Sbar,Xtest.S)

                            j <- j + 1
                        }

                        X.pred <- rbindlist(X.pred.list,use.names=T)
                        X.nms <- colnames(Xtrain)
                        setcolorder(X.pred,X.nms)

                        Xtrain.S <- rbindlist(Xtrain.S.list,use.names=F,idcol = T) # Including .id column such that
                        colnames(Xtrain.S)[-1] <-  rep("",i) # Removing variable names as these are incorrect anyway.

                        pred <- pred_vector(model=model,data=X.pred)

                        if (empirical_settings$AIC_optim_func == "nlminb"){ # May implement the version which just evaluates on a grid
                            nlm.obj <- suppressWarnings(nlminb(start = empirical_settings$AIC_optim_startval,
                                                               objective = AICc.func.new,
                                                               y = pred,
                                                               X = Xtrain.S,
                                                               kernel="Mahalanobis",
                                                               scale_var=F,
                                                               S_scale_dist = T,
                                                               idcol = T,
                                                               lower = 0,
                                                               control=list(eval.max=empirical_settings$AIC_optim_max_eval,
                                                                            trace=0)))
                            if (empirical_settings$AICc_optimize_every_testobs){
                                h_optim_mat[these_cond,loop] <- nlm.obj$par
                            } else {
                                h_optim_mat[these_cond,] <- nlm.obj$par
                            }
                        }

                    }

                    #print(paste0("Optimized ", i ))

                }
            }

            if (empirical_settings$type == "AICc_full"){

                for (i in these_empirical){

                    S <- l$S[i,]

                    S.cols <- which(as.logical(S))
                    Sbar.cols <- which(as.logical(1-S))

                    for (loop in 1:nloops){
                        this.optimsamp <- optimsamp
                        if (empirical_settings$AICc_optimize_every_testobs){
                            this.optimsamp$samp_test <- loop
                        }


                    Xtrain.S <- subset(l$Xtrain,select=S.cols)[this.optimsamp$samp_train,]
                    Xtrain.Sbar <- subset(l$Xtrain,select=Sbar.cols)[this.optimsamp$samp_train,]
                    Xtest.S <- subset(l$Xtest,select=S.cols)[this.optimsamp$samp_test,]

                    Xtrain.S[,.id:=1]
                    setcolorder(Xtrain.S,".id") # moves the .id column to the front

                    X.pred <- cbind(Xtrain.Sbar,Xtest.S)
                    X.nms <- colnames(Xtrain)
                    setcolorder(X.pred,X.nms)

                    pred <- pred_vector(model=model,data=X.pred)

                    if (empirical_settings$AIC_optim_func == "nlminb"){ # May implement the version which just evaluates on a grid
                        nlm.obj <- suppressWarnings(nlminb(start = empirical_settings$AIC_optim_startval,
                                                           objective = AICc.func.new,
                                                           y = pred+rnorm(50),
                                                           X = Xtrain.S,
                                                           kernel="Mahalanobis",
                                                           scale_var=F,
                                                           S_scale_dist = T,
                                                           idcol = T,
                                                           lower = 0,
                                                           control=list(eval.max=empirical_settings$AIC_optim_max_eval,
                                                                        trace=0)))
                        if (empirical_settings$AICc_optimize_every_testobs){
                            h_optim_mat[i,loop] <- nlm.obj$par
                        } else {
                            h_optim_mat[i,] <- nlm.obj$par
                        }
                    }
                    # print(paste0("Optimized ", i ))
                    }

                }

            }


        }

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
        print(sprintf("%d out of %d", i, l$Xtest[, .N]))

        ll[[i]] <- get_predictions(
            model = model,
            D = l$D[,i,],
            h_optim_vec = h_optim_mat[,i],
            kernel_metric = empirical_settings$kernel_metric,
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
            Xtest_Gauss_trans = Xtest_Gauss_trans[i,,drop=FALSE])
        ll[[i]][, id := i]
    }

    DT <- rbindlist(ll)

    Kshap <- matrix(0, nrow = nrow(l$Xtest), ncol = nrow(l$W))
    for (i in l$Xtest[, .I]) {
        Kshap[i, ] = l$W %*% DT[id == i, k]
    }

    ret_list = list(Kshap = Kshap, other_objects = list(ll = ll, DT = DT, h_optim_mat = h_optim_mat))
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
                               compute_distances = TRUE,
                               normalize_distance_rows = TRUE) {

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

    if(compute_distances){ # Only compute the distances if the empirical approach is used
        D <- gen_Mahlanobis_dist_cpp(X$features,as.matrix(Xtrain),as.matrix(Xtest),mcov=mcov,S_scale_dist = S_scale_dist) # This is D_S(,)^2 in the paper
        if (normalize_distance_rows){
            colmin <- apply(X = D,MARGIN = c(2,3),FUN=min)
            for(i in 1:2^m){
                D[,,i] <- t(t(D[,,i])-colmin[,i])
            }
        }

    } else {
        D <- NULL
    }

    ## Get feature matrix ---------
    S <- feature_matrix_cpp(features = X[["features"]], nfeatures = ncol(Xtrain)) # The scaling of S influence onyl the matrix product with D, as we only care about the elements of S being zero/nonzero elsewhere.

    return(list(D = D, S = S, W = W, X = X, Xtrain = l$Xtrain, Xtest = l$Xtest))
}




