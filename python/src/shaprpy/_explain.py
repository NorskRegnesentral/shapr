import warnings
import numpy as np
import pandas as pd
from typing import Callable
from datetime import datetime
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface import NULL, NA
from shaprpy.utils import r2py, py2r, recurse_r_tree
from rpy2.robjects.vectors import StrVector, ListVector, BoolVector
from shaprpy.explanation import Shapr
from shaprpy._rutils import _importr

data_table = _importr('data.table')
shapr = _importr('shapr')
utils = _importr('utils')
base = _importr('base')
stats = _importr('stats')

def maybe_null(val):
  return val if val is not None else NULL

def explain(
    model,
    x_explain: pd.DataFrame,
    x_train: pd.DataFrame,
    approach: str | list[str],
    phi0: float,
    iterative: bool | None = None,
    max_n_coalitions: int | None = None,
    group: dict | None = None,
    n_MC_samples: int = 1000,
    seed: int | None = None,
    verbose: str | list[str] | None = "basic",
    predict_model: Callable | None = None,
    get_model_specs: Callable | None = None,
    asymmetric: bool = False,
    causal_ordering: dict | None = None,
    confounding: bool | None = None,
    extra_computation_args: dict | None = None,
    iterative_args: dict | None = None,
    output_args: dict | None = None,
    **kwargs,
  ):
    """
    Explain the output of machine learning models with more accurately estimated Shapley values.

    Computes dependence-aware Shapley values for observations in `x_explain` from the specified
    `model` by using the method specified in `approach` to estimate the conditional expectation.

    Parameters
    ----------
    model: The model whose predictions we want to explain.
      `shaprpy` natively supports `sklearn`, `xgboost` and `keras` models.
      Unsupported models can still be explained by passing `predict_model` and (optionally) `get_model_specs`.
    x_explain: pd.DataFrame
      Contains the features whose predictions ought to be explained.
    x_train: pd.DataFrame
      Contains the data used to estimate the (conditional) distributions for the features
      needed to properly estimate the conditional expectations in the Shapley formula.
    approach: str or list[str]
      The method(s) to estimate the conditional expectation. All elements should,
      either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"categorical"`, `"timeseries"`, `"independence"`,
      `"regression_separate"`, or `"regression_surrogate"`.
    phi0: float
      The prediction value for unseen data, i.e. an estimate of the expected prediction without conditioning on any
      features. Typically we set this value equal to the mean of the response variable in our training data, but other
      choices such as the mean of the predictions in the training data are also reasonable.
    iterative: bool or None, optional
      If `None` (default), the argument is set to `True` if there are more than 5 features/groups, and `False` otherwise.
      If `True`, the Shapley values are estimated iteratively in an iterative manner.
    max_n_coalitions: int or None, optional
      The upper limit on the number of unique feature/group coalitions to use in the iterative procedure
      (if `iterative = True`). If `iterative = False` it represents the number of feature/group coalitions to use directly.
      `max_n_coalitions = None` corresponds to `max_n_coalitions=2^n_features`.
    group: dict or None, optional
      If `None` regular feature wise Shapley values are computed.
      If provided, group wise Shapley values are computed. `group` then contains lists of unique feature names with the
      features included in each of the different groups.
    n_MC_samples: int, optional
      Indicating the maximum number of samples to use in the Monte Carlo integration for every conditional expectation.
    seed: int or None, optional
      Specifies the seed before any randomness based code is being run.
      If `None` (default) the seed will be inherited from the calling environment.
    verbose: str or list[str] or None, optional
      Specifies the verbosity (printout detail level) through one or more of the strings `"basic"`, `"progress"`,
      `"convergence"`, `"shapley"`  and `"vS_details"`. `None` means no printout.
    predict_model: Callable, optional
      The prediction function used when `model` is not natively supported. The function must have two arguments, `model` and `newdata`
      which specify, respectively, the model and a pandas.DataFrame to compute predictions for. The function must give the prediction as a numpy.Array.
    get_model_specs: Callable, optional
      An optional function for checking model/data consistency when `model` is not natively supported. The function takes `model` as argument
      and provides a `dict` with 3 elements: `labels`, `classes`, and `factor_levels`.
    asymmetric: bool, optional
      If `False` (default), `explain` computes regular symmetric Shapley values. If `True`, then `explain` computes asymmetric Shapley values
      based on the (partial) causal ordering given by `causal_ordering`.
    causal_ordering: dict or None, optional
      An unnamed list of vectors specifying the components of the partial causal ordering that the coalitions must respect.
    confounding: bool or None, optional
      A vector of logicals specifying whether confounding is assumed or not for each component in the `causal_ordering`.
    extra_computation_args: dict or None, optional
      Specifies extra arguments related to the computation of the Shapley values.
    iterative_args: dict or None, optional
      Specifies the arguments for the iterative procedure.
    output_args: dict or None, optional
      Specifies certain arguments related to the output of the function.
    **kwargs: Further arguments passed to specific approaches.

    Returns
    -------
    dict
      A dictionary containing the following items:
      - "shapley_values_est": pd.DataFrame with the estimated Shapley values.
      - "shapley_values_sd": pd.DataFrame with the standard deviation of the Shapley values.
      - "pred_explain": numpy.Array with the predictions for the explained observations.
      - "MSEv": dict with the values of the MSEv evaluation criterion.
      - "iterative_results": dict with the results of the iterative estimation.
      - "saving_path": str with the path where intermediate results are stored.
      - "internal": dict with the different parameters, data, functions and other output used internally.
      - "timing": dict containing timing information for the different parts of the computation.
    """

    init_time = base.Sys_time() # datetime.now()


    if seed is not None:
      base.set_seed(seed)

    # Gets and check feature specs from the model
    rfeature_specs = get_feature_specs(get_model_specs, model)

    # Fixes the conversion from dict to a named list of vectors in R
    r_group = NULL if group is None else ListVector({key: StrVector(value) for key, value in group.items()})

    # Fixes the conversion from dict to a named list of vectors in R
    r_causal_ordering = NULL if causal_ordering is None else ListVector({key: StrVector(value) for key, value in causal_ordering.items()})

    # Convert confounding list to R logical vector
    r_confounding = NULL if confounding is None else BoolVector(confounding)

    # Fixes method specific argument names by replacing first occurrence of "_" with "."
    if len(kwargs) > 0:
      kwargs = change_first_underscore_to_dot(kwargs)

      # Convert from dict to a named list of vectors in R if `regression.vfold_cv_para` is provided by the user
      if 'regression.vfold_cv_para' in kwargs:
        kwargs['regression.vfold_cv_para'] = ListVector(kwargs['regression.vfold_cv_para'])

    # Convert from None or dict to a named list in R
    if iterative_args is None:
      iterative_args = ro.ListVector({})
    else:
      iterative_args = ListVector(iterative_args)

    if output_args is None:
      output_args = ro.ListVector({})
    else:
      output_args = ListVector(output_args)

    if extra_computation_args is None:
      extra_computation_args = ro.ListVector({})
    else:
      extra_computation_args = ListVector(extra_computation_args)

    model_class = f"{type(model).__module__}.{type(model).__name__}"

    # Sets up and organizes input parameters
    # Checks the input parameters and their compatability
    # Checks data/model compatability

    if isinstance(approach, str):
      approach = [approach]

    if isinstance(verbose, str):
      verbose = [verbose]
    if isinstance(verbose, list):
      verbose = StrVector(verbose)
    else:
      verbose = maybe_null(verbose)


    rinternal = shapr.setup(
      x_train = py2r(x_train),
      x_explain = py2r(x_explain),
      approach = StrVector(approach),
      phi0 = phi0,
      max_n_coalitions = maybe_null(max_n_coalitions),
      group = r_group,
      n_MC_samples = n_MC_samples,
      seed = maybe_null(seed),
      feature_specs = rfeature_specs,
      verbose = verbose,
      iterative = maybe_null(iterative),
      iterative_args = iterative_args,
      asymmetric = asymmetric,
      causal_ordering = r_causal_ordering,
      confounding = r_confounding,
      output_args = output_args,
      extra_computation_args = extra_computation_args,
      init_time = init_time,
      is_python = True,
      model_class = model_class,
      **kwargs
    )

    # Gets predict_model (if not passed to explain) and checks that predict_model gives correct format
    predict_model = get_predict_model(x_test=x_train.head(2), predict_model=predict_model, model=model)

    rinternal.rx2['timing_list'].rx2['test_prediction'] = base.Sys_time()

    rinternal = additional_regression_setup(
      rinternal,
      model,
      predict_model,
      x_train,
      x_explain)

    # Not called for approach %in% c("regression_surrogate","vaeac")
    rinternal = shapr.setup_approach(internal = rinternal) # model and predict_model are not supported in Python

    rinternal.rx2['main_timing_list'] = rinternal.rx2['timing_list']

    converged = False
    iter = len(rinternal.rx2('iter_list'))

    if seed is not None:
      base.set_seed(seed)

    shapr.cli_startup(rinternal, verbose)

    rinternal.rx2['iter_timing_list'] = ro.ListVector({})

    while not converged:
      shapr.cli_iter(verbose, rinternal, iter)

      rinternal.rx2['timing_list'] = ro.ListVector({'init': base.Sys_time()})

      # Setup the Shapley framework
      rinternal = shapr.shapley_setup(rinternal)

      # Only actually called for approach in ["regression_surrogate", "vaeac"]
      rinternal = shapr.setup_approach(rinternal)

      # Compute the vS
      vS_list = compute_vS(rinternal, model, predict_model)

      # Compute Shapley value estimates and bootstrapped standard deviations
      rinternal = shapr.compute_estimates(rinternal, vS_list)

      # Check convergence based on estimates and standard deviations (and thresholds)
      rinternal = shapr.check_convergence(rinternal)

      # Save intermediate results
      shapr.save_results(rinternal)

      # Preparing parameters for next iteration (does not do anything if already converged)
      rinternal = shapr.prepare_next_iteration(rinternal)

      # Printing iteration information
      shapr.print_iter(rinternal)

      # Setting globals to simplify the loop
      converged = rinternal.rx2('iter_list')[iter-1].rx2('converged')[0]

      rinternal.rx2['timing_list'] = ro.ListVector({**dict(rinternal.rx2['timing_list'].items()), 'postprocess_res': base.Sys_time()})

      # Add the current timing_list to the iter_timing_list
      rinternal.rx2['iter_timing_list'] = ro.ListVector({**dict(rinternal.rx2['iter_timing_list'].items()), f'element_{iter}': rinternal.rx2['timing_list']})

      iter += 1

    rinternal.rx2['main_timing_list'] = ro.ListVector({**dict(rinternal.rx2['main_timing_list'].items()), 'main_computation': base.Sys_time()})

    # Rerun after convergence to get the same output format as for the non-iterative approach
    routput = shapr.finalize_explanation(rinternal)

    rinternal.rx2['main_timing_list'] = ro.ListVector({**dict(rinternal.rx2['main_timing_list'].items()), 'finalize_explanation': base.Sys_time()})


    routput.rx2['timing'] = shapr.compute_time(rinternal)

    # Some cleanup when doing testing
    testing = rinternal.rx2('parameters').rx2('testing')[0]
    if testing:
      routput = shapr.testing_cleanup(routput)

    # Convert R objects to Python objects
    shapley_values_est = recurse_r_tree(routput.rx2('shapley_values_est'))
    shapley_values_sd = recurse_r_tree(routput.rx2('shapley_values_sd'))
    pred_explain = recurse_r_tree(routput.rx2('pred_explain'))
    MSEv = recurse_r_tree(routput.rx2('MSEv'))
    iterative_results = recurse_r_tree(routput.rx2('iterative_results'))
    saving_path = recurse_r_tree(routput.rx2('saving_path'))
    internal = recurse_r_tree(routput.rx2('internal'))
    timing = recurse_r_tree(routput.rx2('timing'))

    explanation_dict = {
      "shapley_values_est": shapley_values_est,
      "shapley_values_sd": shapley_values_sd,
      "pred_explain": pred_explain,
      "MSEv": MSEv,
      "iterative_results": iterative_results,
      "saving_path": saving_path,
      "internal": internal,
      "timing": timing,
    }

    # Return the new Shapr class instance with both Python dict and R object
    return Shapr(explanation_dict, r_object=routput)


def compute_vS(rinternal, model, predict_model):

  iter = len(rinternal.rx2('iter_list'))

  S_batch = rinternal.rx2('iter_list')[iter-1].rx2('S_batch')

  # verbose
  shapr.cli_compute_vS(rinternal)

  stats.rnorm(1) # Perform a single sample to forward the RNG state one step. This is done to ensurie consistency with
                # future.apply::future_lapply in R which does this to to guarantee consistency for parallellization.
                # See ?future.apply::future_lapply for details

  vS_list = ro.ListVector({})
  for i, S in enumerate(S_batch):
    vS_list.rx2[i+1] = batch_compute_vS(S=S, rinternal=rinternal, model=model, predict_model=predict_model)

  #### Adds v_S output above to any vS_list already computed ####
  vS_list = shapr.append_vS_list(vS_list,rinternal)

  return vS_list


def batch_compute_vS(S, rinternal, model, predict_model):
  regression = rinternal.rx2('parameters').rx2('regression')[0]

  # Check if we are to use regression or Monte Carlo integration to compute the contribution function values
  if regression:
    dt_vS = shapr.batch_prepare_vS_regression(S=S, internal=rinternal)
  else:
    # dt_vS is either only dt_vS or a list containing dt_vS and dt if internal$parameters$output_args$keep_samp_for_vS = TRUE
    dt_vS = batch_prepare_vS_MC(S=S, rinternal=rinternal, model=model, predict_model=predict_model)

  return dt_vS


def batch_prepare_vS_MC_old(S, rinternal, model, predict_model):
  keep_samp_for_vS = rinternal.rx2('parameters').rx2('keep_samp_for_vS')[0]
  feature_names = list(rinternal.rx2('parameters').rx2('feature_names'))

  dt = shapr.batch_prepare_vS_MC_auxiliary(S=S, internal=rinternal)

  dt = compute_preds(dt=dt, feature_names=feature_names, predict_model=predict_model, model=model)

  dt_vS = shapr.compute_MCint(dt)

  if keep_samp_for_vS:
    return ro.ListVector({'dt_vS':dt_vS, 'dt_samp_for_vS':dt})
  else:
    return dt_vS

def batch_prepare_vS_MC(S, rinternal, model, predict_model):
  feature_names = list(rinternal.rx2('parameters').rx2('feature_names'))
  keep_samp_for_vS = rinternal.rx2('parameters').rx2('output_args').rx2('keep_samp_for_vS')[0]
  causal_sampling = rinternal.rx2('parameters').rx2('causal_sampling')[0]
  output_size = int(rinternal.rx2('parameters').rx2('output_size')[0])

  dt = shapr.batch_prepare_vS_MC_auxiliary(S=S, internal=rinternal, causal_sampling=causal_sampling)

  pred_cols = [f"p_hat{i+1}" for i in range(output_size)]
  type_ = rinternal.rx2('parameters').rx2('type')[0]

  if type_ == "forecast":
    horizon = rinternal.rx2('parameters').rx2('horizon')[0]
    n_endo = rinternal.rx2('data').rx2('n_endo')[0]
    explain_idx = rinternal.rx2('parameters').rx2('explain_idx')[0]
    explain_lags = rinternal.rx2('parameters').rx2('explain_lags')[0]
    y = rinternal.rx2('data').rx2('y')
    xreg = rinternal.rx2('data').rx2('xreg')
    dt = compute_preds(
      dt=dt,
      feature_names=feature_names,
      predict_model=predict_model,
      model=model,
      type_=type_,
      horizon=horizon,
      n_endo=n_endo,
      explain_idx=explain_idx,
      explain_lags=explain_lags,
      y=y,
      xreg=xreg
      )
  else:
    dt = compute_preds(
      dt=dt,
      feature_names=feature_names,
      predict_model=predict_model,
      model=model,
      type_=type_
      )

  dt_vS = shapr.compute_MCint(dt)

  if keep_samp_for_vS:
    return ro.ListVector({'dt_vS': dt_vS, 'dt_samp_for_vS': dt})
  else:
    return dt_vS

def compute_preds(
  dt,
  feature_names,
  predict_model,
  model,
  type_,
  horizon=None,
  n_endo=None,
  explain_idx=None,
  explain_lags=None,
  y=None,
  xreg=None
):
  # Predictions
  if type_ == "forecast":
    preds = predict_model(
      model,
      r2py(dt).loc[:,:n_endo],
      r2py(dt).loc[:,n_endo:],
      horizon,
      explain_idx,
      explain_lags,
      y,
      xreg
      )

  else:
    preds = predict_model(
      model,
      r2py(dt).loc[:,feature_names]
      )

  return ro.r.cbind(dt, p_hat=ro.FloatVector(preds.tolist()))



def compute_preds_old(dt, feature_names, predict_model, model):
  preds = predict_model(model, r2py(dt).loc[:,feature_names])
  return ro.r.cbind(dt, p_hat=ro.FloatVector(preds.tolist()))



def get_feature_specs(get_model_specs, model):
  model_class0 = type(model)

  if (get_model_specs is not None) and (not callable(get_model_specs)):
    raise ValueError('`get_model_specs` must be None or callable.')

  if get_model_specs is None:
    get_model_specs = prebuilt_get_model_specs(model)
    if get_model_specs is None:
      # R will issue a warning about this.
      return NULL

  if callable(get_model_specs):
    try:
      feature_specs = get_model_specs(model)
    except Exception as e:
      raise RuntimeError(f'The get_model_specs function of class `{model_class0}` is invalid.\nA basic function test threw the following error:\n{e}')

  if not isinstance(feature_specs, dict):
    raise ValueError(f'`get_model_specs` returned an object of type `{type(feature_specs)}`, but it should be of type `dict`')
  if set(feature_specs.keys()) != set(["labels","classes","factor_levels"]):
    raise ValueError(f'`get_model_specs` should return a `dict` with keys ["labels","classes","factor_levels"], but found keys {list(feature_specs.keys())}')

  if feature_specs is None:
    rfeature_specs = NULL
  else:
    py2r_or_na = lambda v: py2r(v) if v is not None else NA
    def strvec_or_na(v):
      if v is None: return NA
      strvec = StrVector(list(v.values()))
      strvec.names = list(v.keys())
      return strvec
    def listvec_or_na(v):
      if v is None: return NA
      return ro.ListVector({k:list(val) for k,val in v.items()})

    rfeature_specs = ro.ListVector({
      'labels': py2r_or_na(feature_specs['labels']),
      'classes': strvec_or_na(feature_specs['classes']),
      'factor_levels': listvec_or_na(feature_specs['factor_levels']),
    })
  return rfeature_specs


def get_predict_model(x_test, predict_model, model):

  model_class0 = type(model)

  if (predict_model is not None) and (not callable(predict_model)):
    raise RuntimeError(f'The predict_model function of class `{model}` is invalid.\nA basic function test threw the following error:\n{e}')

  if predict_model is None:
    predict_model = prebuilt_predict_model(model)
    if predict_model is None:
      raise ValueError(f'No pre-built predict_model for model of type {type(model)}. Please pass a custom predict_model to shaprpy.explain(...).')

  try:
    tmp = py2r(predict_model(model, x_test))
  except Exception as e:
      raise RuntimeError(f'The predict_model function of class `{model_class0}` is invalid.\nA basic function test threw the following error:\n{e}')
  if not all(base.is_numeric(tmp)):
    raise RuntimeError('The output of predict_model is expected to be numeric.')
  if not (len(tmp) == 2):
    raise RuntimeError('The output of predict_model does not match the length of the input.')
  return predict_model


def prebuilt_get_model_specs(model):

  # Look for sklearn
  try:
    from sklearn.base import BaseEstimator
    if isinstance(model, BaseEstimator):
      return lambda m: {
        'labels': m.feature_names_in_,
        'classes': None, # Not available from model object
        'factor_levels': None, # Not available from model object
      }
  except:
    pass

  # Look for xgboost.core.Booster
  try:
    import xgboost as xgb
    if isinstance(model, xgb.core.Booster):
      return lambda m: {
        'labels': np.array(m.feature_names),
        'classes': None, # Not available from model object
        'factor_levels': None, # Not available from model object
      }
  except:
    pass

  return None


def prebuilt_predict_model(model):

  # Look for sklearn
  try:
    from sklearn.base import is_classifier, is_regressor
    if is_classifier(model): return lambda m, x: m.predict_proba(x)[:,1]
    if is_regressor(model): return lambda m, x: m.predict(x).flatten()
  except:
    pass

  # Look for xgboost.core.Booster
  try:
    import xgboost as xgb
    if isinstance(model, xgb.core.Booster):
      return lambda m, x: m.predict(xgb.DMatrix(x))
  except:
    pass

  # Look for keras
  try:
    from keras.models import Model
    if isinstance(model, Model):
      def predict_fn(m,x):
        pred = m.predict(x)
        return pred.reshape(pred.shape[0],)
      return predict_fn
  except:
    pass

  return None


def compute_time(timing_list):

  timing_secs = {
      f'{key}': (timing_list[key] - timing_list[prev_key]).total_seconds()
      for key, prev_key in zip(list(timing_list.keys())[1:], list(timing_list.keys())[:-1])
  }
  timing_output = {
      'init_time': timing_list['init_time'].strftime("%Y-%m-%d %H:%M:%S"),
      'total_time_secs': sum(timing_secs.values()),
      'timing_secs': timing_secs
  }

  return timing_output


def additional_regression_setup(rinternal, model, predict_model, x_train, x_explain):
  # Add the predicted response of the training and explain data to the internal list for regression-based methods
  regression = rinternal.rx2("parameters").rx2("regression")[0]
  if regression:
    rinternal = regression_get_y_hat(rinternal, model, predict_model, x_train, x_explain)

  return rinternal


def regression_get_y_hat(rinternal, model, predict_model, x_train, x_explain):
  x_train_y_hat = predict_model(model, x_train)
  x_explain_y_hat = predict_model(model, x_explain)

  # Extract data list, add the predicted responses, and then updated rinternal (direct assignment did not work)
  data = rinternal.rx2['data']
  data.rx2['x_train_y_hat'] = ro.FloatVector(x_train_y_hat.tolist())
  data.rx2['x_explain_y_hat'] = ro.FloatVector(x_explain_y_hat.tolist())
  rinternal.rx2['data'] = data

  return rinternal


def regression_remove_objects(routput):
  tmp_internal = routput.rx2("internal")
  tmp_parameters = tmp_internal.rx2("parameters")
  objects = StrVector(("regression", "regression.model", "regression.tune_values", "regression.vfold_cv_para",
                           "regression.recipe_func", "regression.tune", "regression.surrogate_n_comb"))
  tmp_parameters.rx[objects] = NULL
  tmp_internal.rx2["parameters"] = tmp_parameters
  if tmp_parameters.rx2("approach")[0] == "regression_surrogate":
      tmp_objects = tmp_internal.rx2("objects")
      tmp_objects.rx["regression.surrogate_model"] = NULL
      tmp_internal.rx2["objects"] = tmp_objects
  routput.rx2["internal"] = tmp_internal
  return routput


def change_first_underscore_to_dot(kwargs):
  kwargs_tmp = {}
  for k, v in kwargs.items():
    kwargs_tmp[k.replace('_', '.', 1)] = v
  return kwargs_tmp
