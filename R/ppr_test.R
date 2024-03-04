# Libraries
library(modeldata)
library(tidymodels)

# Add the ppr
parsnip::set_new_model(model = "ppr_reg")
parsnip::set_model_mode(model = "ppr_reg", mode = "regression")
parsnip::set_model_engine(model = "ppr_reg", mode = "regression", eng = "ppr")
parsnip::set_dependency("ppr_reg", eng = "ppr", pkg = "stats")

# Look at what has been registrated
parsnip::show_model_info("ppr_reg")

# Add the model arguments
set_model_arg(
  model = "ppr_reg",
  eng = "ppr",
  parsnip = "num_terms", # What we call the parameter in parsnip. Match what tidymodels use
  original = "nterms", # The original paramter name in stats::ppr
  func = list(pkg = "stats", fun = "ppr"),
  has_submodel = FALSE
)

# Create the model function
ppr_reg <- function(mode = "regression", num_terms = NULL) {
    # Check for correct mode
    if (mode  != "regression") rlang::abort("`mode` should be 'regression'")

    # Capture the arguments in quosures
    args <- list(num_terms = rlang::enquo(num_terms))

    # Save some empty slots for future parts of the specification
    new_model_spec(
      "ppr_reg",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

# Add a fit module
set_fit(
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

set_encoding(
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
parsnip::show_model_info("ppr_reg")

set_pred(
  model = "ppr_reg",
  eng = "ppr",
  mode = "regression",
  type = "numeric",
  value = list(
    pre = NULL,
    post = NULL,
    func = c(fun = "predict"),
    args =
      list(
        object = quote(object$fit),
        newdata = quote(new_data)
      )
  )
)
parsnip::show_model_info("ppr_reg")

ppr_reg() %>%  translate(engine = "ppr")
ppr_reg(num_terms = 2) %>%  translate(engine = "ppr")
ppr()



ppr_reg(num_terms = tune())



library(readr)       # for importing data
urchins <-
  # Data were assembled for a tutorial
  # at https://www.flutterbys.com.au/stats/tut/tut7.5a.html
  read_csv("https://tidymodels.org/start/models/urchins.csv") %>%
  # Change the names to be a little more verbose
  setNames(c("food_regime", "initial_volume", "width")) %>%
  # Factors are very helpful for modeling, so we convert one column
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))




ppr_mod = ppr_reg(num_terms = tune()) %>% set_engine("ppr") %>% set_mode("regression")
grid = grid_regular(num_terms(c(1L, 3)), levels = 3)
folds <- vfold_cv(urchins)
wf <- workflow() %>%
  add_model(ppr_mod) %>%
  add_formula(width ~ initial_volume * food_regime)
res <-
  wf %>%
  tune_grid(
    resamples = folds,
    grid = grid
  )


ppr_mod %>%
  fit(formula = formula("width ~ initial_volume * food_regime"), data = urchins)

#
# ppr() %>%
#   translate(engine = "stats")
#
#
# x_train = matrix(rnorm(12), nrow = 3)
# y_train = seq(3)
# kk = stats::ppr(x = x_train, y = y_train, nterms = 1)
# kk
# predict(kk, x_train*2)
#
#
#
# ## S3 method for class 'formula'
# ppr(formula, data, weights, subset, na.action,
#     contrasts = NULL, ..., model = FALSE)
#
# ## Default S3 method:
# ppr(x, y, weights = rep(1, n),
#     ww = rep(1, q), nterms, max.terms = nterms, optlevel = 2,
#     sm.method = c("supsmu", "spline", "gcvspline"),
#     bass = 0, span = 0, df = 5, gcvpen = 1, trace = FALSE, ...)
#
#
# dials::num_terms()
