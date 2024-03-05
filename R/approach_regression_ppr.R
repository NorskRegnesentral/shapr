make_ppr_reg = function() {
  # Add the projection pursuit regression model from `stats` as new `tidymodels` regression model.
  # We follow the guide given provided by `tidymodels` for how to add new models and refer to
  # https://www.tidymodels.org/learn/develop/models/ for more details and explanations of the code bellow.

  # Step 1: register the model, modes, and arguments
  parsnip::set_new_model(model = "ppr_reg")
  parsnip::set_model_mode(model = "ppr_reg", mode = "regression")
  parsnip::set_model_engine(model = "ppr_reg", mode = "regression", eng = "ppr")
  parsnip::set_dependency("ppr_reg", eng = "ppr", pkg = "stats")

  # If your function has several parameters, then we add one of these functions for each parameter
  parsnip::set_model_arg(
    model = "ppr_reg",
    eng = "ppr",
    original = "nterms", # The original parameter name used in stats::ppr
    parsnip = "num_terms", # Change parameter name to match tidymodels' name convention
    func = list(pkg = "dials", fun = "num_terms"),#list(pkg = "stats", fun = "ppr"),
    has_submodel = FALSE
  )

  # Step 2: create the model function
  ppr_reg <- function(mode = "regression", num_terms = NULL) {
    # Check for correct mode
    if (mode  != "regression") rlang::abort("`mode` should be 'regression'")

    # Capture the arguments in quosures
    args <- list(num_terms = rlang::enquo(num_terms))

    # Save some empty slots for future parts of the specification
    parsnip::new_model_spec(
      "ppr_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

  # Step 3: add a fit module
  parsnip::set_fit(
    model = "ppr_reg",
    eng = "ppr",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "stats", fun = "ppr"),
      defaults = list()
    )
  )

  parsnip::set_encoding(
    model = "ppr_reg",
    eng = "ppr",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  # Step 4: add modules for prediction
  parsnip::set_pred(
    model = "ppr_reg",
    eng = "ppr",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        newdata = quote(new_data),
        type = "numeric"
      )
    )
  )

  # Step 5: add tuning function (used by tune::tune_grid())
  tunable.ppr_reg <- function(x, ...) {
    tibble::tibble(
      name = c("num_terms"),
      call_info = list(list(pkg = NULL, fun = "num_terms")),
      source = "model_spec",
      component = "ppr_reg",
      component_id = "main"
    )
  }

  # Step 6: add updating function (used by tune::finalize_workflow())
  update.ppr_reg <- function(object, parameters = NULL, num_terms = NULL, ...) {
    rlang::check_installed("parsnip")
    eng_args <- parsnip::update_engine_parameters(object$eng_args, fresh = TRUE, ...)
    args <- list(num_terms = rlang::enquo(num_terms))
    args <- parsnip::update_main_parameters(args, parameters)
    parsnip::new_model_spec(
      "ppr_reg",
      args = args,
      eng_args = eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }
}



# Some examples:
#
#
#
#
# ppr_reg() %>% translate(engine = "ppr")
# ppr_reg(num_terms = 2) %>% translate(engine = "ppr")
# ppr_reg(num_terms = tune()) %>%  translate(engine = "ppr")
# ppr_reg()
#
#
# x_train = data.table::data.table(matrix(rnorm(100), 25))
# x_train$y = rowSums(x_train) + rnorm(25)
#
#
# recipe = recipes::recipe(y ~ ., data = x_train)
# ppr_mod = ppr_reg(num_terms = 2) %>% set_engine("ppr") %>% set_mode("regression")
# grid = grid_regular(num_terms(c(1L, 3)), levels = 3)
# wf <- workflow() %>% add_model(ppr_mod) %>% add_recipe(recipe)
# model = fit(wf, data = x_train)
# predict(model, new_data = x_train)
# class(model)
#
#
#
#
# ppr_mod = ppr_reg(num_terms = tune()) %>% set_engine("ppr") %>% set_mode("regression")
# folds <- vfold_cv(x_train)
# ppr_mod %>% tune_grid(y ~ ., resamples = folds, grid = 4)
# wf <- workflow() %>% add_model(ppr_mod) %>% add_recipe(recipe)
# res <- wf %>% tune_grid(resamples = folds, grid = grid)
# res %>% show_best(metric = "rmse")
# best = res %>% select_best("rmse")
#
# # Update the workflow by finalizing it using the hyperparameters that attained the best rmse
# regression_workflow <- tune::finalize_workflow(wf, tune::select_best(res, "rmse"))
#
# # Fit the model to the augmented training data
# regression_fit <- fit(regression_workflow, data = x_train)
# predict(regression_fit, x_train)
#
# class(regression_workflow)
# class(regression_fit)
#
#
# summary(lm_fit)
#
# lm_fit
#
# library(readr)       # for importing data
# urchins <-
#   # Data were assembled for a tutorial
#   # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
#   read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
#   # Change the names to be a little more verbose
#   setNames(c("food_regime", "initial_volume", "width")) %>%
#   # Factors are very helpful for modeling, so we convert one column
#   mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))
#
#
# ppr_mod = ppr_reg(num_terms = tune()) %>% set_engine("ppr") %>% set_mode("regression")
# ppr_mod = ppr_reg(num_terms = 2) %>% set_engine("ppr") %>% set_mode("regression")
# ppr_mod = decision_tree(tree_depth = 2) %>% set_engine("rpart") %>% set_mode("regression")
# grid = grid_regular(num_terms(c(1L, 3)), levels = 3)
# grid = dials::grid_regular(dials::tree_depth(), levels = 4)
# folds <- vfold_cv(urchins)
# recipe = recipes::recipe(width ~ initial_volume + food_regime, data = urchins) %>% step_dummy(all_factor())
#
# recipe = recipes::recipe(width ~ initial_volume + food_regime, data = urchins)
# wf <- workflow() %>%
#   add_model(ppr_mod) %>%
#   add_recipe(recipe)
# model = fit(wf, data = urchins)
# model
#
#
#
# res <-
#   wf %>%
#   tune_grid(
#     resamples = folds,
#     grid = grid
#   )
# res %>% collect_metrics()
# res %>% show_best(metric = "rmse")
#
# best <- res %>% select_best("rmse")
# best
# final_wf <-
#   wf %>%
#   finalize_workflow(best)
#
# wf
#
#
# # step_interact(terms = ~ x1 * x2, scale = FALSE, role = "predictor",
# #               keep_orig = TRUE, order = 2, mode = "both",
# #               skip = FALSE, method = "none",
# #               estimate = "partial", indicators = "full")
#
# ppr_mod %>%
#   fit(formula = formula("width ~ initial_volume * food_regime"), data = urchins)
#
# #
# # ppr() %>%
# #   translate(engine = "stats")
# #
# #
# # x_train = matrix(rnorm(12), nrow = 3)
# # y_train = seq(3)
# # kk = stats::ppr(x = x_train, y = y_train, nterms = 1)
# # kk
# # predict(kk, x_train*2)
# #
# #
# #
# # ## S3 method for class 'formula'
# # ppr(formula, data, weights, subset, na.action,
# #     contrasts = NULL, ..., model = FALSE)
# #
# # ## Default S3 method:
# # ppr(x, y, weights = rep(1, n),
# #     ww = rep(1, q), nterms, max.terms = nterms, optlevel = 2,
# #     sm.method = c("supsmu", "spline", "gcvspline"),
# #     bass = 0, span = 0, df = 5, gcvpen = 1, trace = FALSE, ...)
# #
# #
# # dials::num_terms()
