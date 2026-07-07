# data.R — synthetic datasets and (cached) prediction models.
#
# Each dataset is generated once into a large pool (train + explain) and cached
# under data/. A single run subsamples rows (n_train / n_explain) and, for the
# numeric data, columns (n_features). Models are trained per (dataset,
# n_features, n_train) and cached. Model training time is NOT part of the
# measured explain() time.

suppressMessages(library(data.table))

# AR(1) covariance: cov(i, j) = rho^|i - j|.
.ar1_features <- function(n, p, rho, seed) {
  set.seed(seed)
  idx <- seq_len(p)
  sigma <- rho^abs(outer(idx, idx, "-"))
  chol_sigma <- chol(sigma)
  z <- matrix(rnorm(n * p), n, p)
  x <- z %*% chol_sigma
  colnames(x) <- paste0("num_", idx)
  return(x)
}

.numeric_response <- function(x) {
  p <- ncol(x)
  beta <- rep_len(c(1, -1, 0.5), p)
  lin <- as.vector(x %*% beta)
  inter <- x[, 1] * x[, min(2, p)]
  return(lin + sin(x[, 1]) + inter + rnorm(nrow(x), 0, 0.5))
}

.make_factors <- function(n, n_factor, levels, seed) {
  set.seed(seed)
  lev <- paste0("L", seq_len(levels))
  facs <- lapply(seq_len(n_factor), function(j) {
    factor(sample(lev, n, replace = TRUE), levels = lev)
  })
  names(facs) <- paste0("fac_", seq_len(n_factor))
  return(as.data.table(facs))
}

# Generate one pool (`n` rows) for a dataset spec. `family` is the dataset
# family (numeric / mixed / categorical); `spec` is the concrete dataset spec.
# Returns list(x = data.table, y = numeric).
.generate_pool_part <- function(family, spec, n, seed) {
  if (family == "numeric") {
    x <- .ar1_features(n, spec$n_features_max, spec$rho, seed)
    y <- .numeric_response(x)
    return(list(x = as.data.table(x), y = y))
  }
  if (family == "mixed") {
    xnum <- .ar1_features(n, spec$n_numeric, spec$rho, seed)
    y_num <- .numeric_response(xnum)
    facs <- .make_factors(n, spec$n_factor, spec$factor_levels, seed + 1L)
    fac_effect <- rowSums(sapply(facs, function(f) as.integer(f) - 1L))
    y <- y_num + 0.5 * fac_effect
    return(list(x = cbind(as.data.table(xnum), facs), y = y))
  }
  if (family == "categorical") {
    facs <- .make_factors(n, spec$n_factor, spec$factor_levels, seed)
    fac_effect <- rowSums(sapply(facs, function(f) as.integer(f) - 1L))
    y <- fac_effect + rnorm(n, 0, 0.5)
    return(list(x = facs, y = y))
  }
  stop("Unknown dataset family: ", family)
}

# Load (or build + cache) the full pool for a concrete dataset. Train and
# explain rows are generated with different seeds so explicands are
# out-of-sample. The cache file is keyed by the concrete dataset name so the
# several `mixed_*` settings each get their own pool.
get_pool <- function(cfg, dataset) {
  spec <- cfg$datasets[[dataset]]
  if (is.null(spec)) {
    stop("No dataset spec for '", dataset, "' in config$datasets")
  }
  # Key the cache by the dataset spec + seed so changing e.g. n_features_max or
  # rho automatically regenerates the pool instead of reusing a stale one.
  key <- digest_key(list(spec = spec, seed = cfg$seed))
  cache <- file.path(cfg$dir$data, paste0("pool_", dataset, "_", key, ".rds"))
  if (file.exists(cache)) {
    return(readRDS(cache))
  }
  family <- dataset_family(dataset)
  train <- .generate_pool_part(family, spec, spec$n_train_max, cfg$seed)
  explain <- .generate_pool_part(family, spec, spec$n_explain_max, cfg$seed + 9973L)
  pool <- list(train = train, explain = explain, dataset = dataset, family = family)
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(pool, cache)
  return(pool)
}

# Build x_train/x_explain/y_train for a specific run by subsetting the pool.
# n_features only subsets columns for the numeric family; for mixed/categorical
# all features are used.
build_run_data <- function(cfg, dataset, n_features, n_train, n_explain) {
  pool <- get_pool(cfg, dataset)
  x_train <- pool$train$x[seq_len(n_train)]
  x_explain <- pool$explain$x[seq_len(n_explain)]
  y_train <- pool$train$y[seq_len(n_train)]
  y_explain <- pool$explain$y[seq_len(n_explain)]

  if (dataset_family(dataset) == "numeric") {
    cols <- paste0("num_", seq_len(n_features))
    x_train <- x_train[, ..cols]
    x_explain <- x_explain[, ..cols]
  }
  return(list(
    x_train = x_train, x_explain = x_explain,
    y_train = y_train, y_explain = y_explain
  ))
}

# Partition feature names into groups of `group_size` consecutive features.
# Returns a named list suitable for explain(group = ...). The last group may be
# smaller if the feature count is not a multiple of group_size.
build_groups <- function(feature_names, group_size = 2L) {
  n <- length(feature_names)
  idx <- split(seq_len(n), ceiling(seq_len(n) / group_size))
  groups <- lapply(idx, function(ii) feature_names[ii])
  names(groups) <- paste0("grp_", seq_along(groups))
  return(groups)
}

# Train (or load from cache) the prediction model for a run. The model type is
# chosen by dataset FAMILY (xgboost for numeric, ranger for mixed/categorical).
get_model <- function(cfg, dataset, x_train, y_train) {
  model_cfg <- cfg$models[[dataset_family(dataset)]]
  key <- digest_key(list(
    dataset = dataset, model = model_cfg, cols = colnames(x_train),
    n_train = nrow(x_train), seed = cfg$seed
  ))
  cache <- file.path(cfg$dir$data, paste0("model_", key, ".rds"))
  if (file.exists(cache)) {
    return(readRDS(cache))
  }

  model <- switch(model_cfg$type,
    # Use the stable xgb.train + xgb.DMatrix API (consistent across xgboost
    # versions) to avoid the high-level API's deprecation warnings.
    xgboost = xgboost::xgb.train(
      params = list(
        max_depth = model_cfg$max_depth, eta = model_cfg$eta,
        nthread = 1, objective = "reg:squarederror"
      ),
      data = xgboost::xgb.DMatrix(data = as.matrix(x_train), label = y_train, nthread = 1),
      nrounds = model_cfg$nrounds, verbose = 0
    ),
    ranger = ranger::ranger(
      y = y_train, x = as.data.frame(x_train),
      num.trees = model_cfg$num_trees, max.depth = model_cfg$max_depth,
      num.threads = 1
    ),
    stop("Unknown model type: ", model_cfg$type)
  )
  dir.create(dirname(cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(model, cache)
  return(model)
}

# Short deterministic key for cache file names.
digest_key <- function(obj) {
  substr(rlang_hash(serialize(obj, NULL)), 1, 16)
}

# Minimal hash without adding a dependency: fall back to digest if available,
# else a simple checksum of the serialized bytes.
rlang_hash <- function(raw) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(raw, algo = "xxhash64"))
  }
  return(sprintf("%08x%08x", sum(as.integer(raw)) %% .Machine$integer.max,
    length(raw)))
}
