# registry.R — named "variant" recipes for approaches whose interesting
# settings are complex R objects (regression model specs, tuning grids) that
# cannot be encoded as scalar key=value approach_args.
#
# A grid row references a variant by name via approach_args "variant=NAME";
# run_one.R looks it up here and merges the returned explain() arguments.
# shapr accepts parsnip model specs / tune grids / vfold lists directly, so we
# build real R objects (no string eval).
#
# Each variant is list(deps = <extra packages>, args = <named explain() args>).
# If a variant's deps are missing, the run is recorded as skipped_missing_dep.

# Natural-spline pre-processing on the numeric predictors only. Applied per
# coalition; recipes only touches the features present in that coalition. This
# is the vignette's documented GAM-like smoothing (shapr does not support
# parsnip::gen_additive_mod directly because of its non-standard formula).
.spline_recipe <- function(rec) {
  recipes::step_ns(rec, recipes::all_numeric_predictors(), deg_free = 3)
}

# Registry of regression variants. Tuning levels are "none" (fixed
# hyperparameters), "light" (small grid + 2-fold CV) and "cv" (larger grid +
# 5-fold CV). shapr only tunes MODEL-spec hyperparameters (not recipe steps),
# so the smooth framework varies the glmnet penalty/mixture, with fixed splines.
regression_variants <- function() {
  list(
    # ---- smooth / GAM-like: natural splines + (penalized) linear regression --
    smooth_none = list(
      deps = character(0),
      args = list(
        regression.model = parsnip::linear_reg(),
        regression.recipe_func = .spline_recipe
      )
    ),
    smooth_light = list(
      deps = "glmnet",
      args = list(
        regression.model = parsnip::linear_reg(
          penalty = hardhat::tune(), mixture = 0, engine = "glmnet"
        ),
        regression.recipe_func = .spline_recipe,
        regression.tune_values = data.frame(penalty = c(0.001, 0.01, 0.1)),
        regression.vfold_cv_para = list(v = 2)
      )
    ),
    smooth_cv = list(
      deps = "glmnet",
      args = list(
        regression.model = parsnip::linear_reg(
          penalty = hardhat::tune(), mixture = hardhat::tune(), engine = "glmnet"
        ),
        regression.recipe_func = .spline_recipe,
        regression.tune_values = expand.grid(
          penalty = c(0.0001, 0.001, 0.01, 0.1, 1), mixture = c(0, 0.5, 1)
        ),
        regression.vfold_cv_para = list(v = 5)
      )
    ),
    # ---- xgboost (boost_tree) -----------------------------------------------
    xgb_none = list(
      deps = "xgboost",
      args = list(
        regression.model = parsnip::boost_tree(
          trees = 50, tree_depth = 3, engine = "xgboost", mode = "regression"
        )
      )
    ),
    xgb_light = list(
      deps = "xgboost",
      args = list(
        regression.model = parsnip::boost_tree(
          trees = hardhat::tune(), tree_depth = hardhat::tune(),
          engine = "xgboost", mode = "regression"
        ),
        regression.tune_values = expand.grid(trees = c(25, 50), tree_depth = c(2, 4)),
        regression.vfold_cv_para = list(v = 2)
      )
    ),
    xgb_cv = list(
      deps = "xgboost",
      args = list(
        regression.model = parsnip::boost_tree(
          trees = hardhat::tune(), tree_depth = hardhat::tune(),
          learn_rate = hardhat::tune(), engine = "xgboost", mode = "regression"
        ),
        regression.tune_values = expand.grid(
          trees = c(25, 50, 100), tree_depth = c(2, 4, 6), learn_rate = c(0.05, 0.3)
        ),
        regression.vfold_cv_para = list(v = 5)
      )
    ),
    # ---- surrogate: single model, no tuning ---------------------------------
    surrogate_none = list(
      deps = "xgboost",
      args = list(
        regression.model = parsnip::boost_tree(
          trees = 50, tree_depth = 3, engine = "xgboost", mode = "regression"
        )
      )
    )
  )
}

# Look up a variant by name; returns NULL if absent.
get_variant <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(name)) {
    return(NULL)
  }
  regression_variants()[[name]]
}
