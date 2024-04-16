import warnings
import numpy as np
import pandas as pd
from typing import Callable
from datetime import datetime
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface import NULL, NA
from shaprpy.utils import r2py, py2r, recurse_r_tree
from rpy2.robjects.vectors import StrVector, ListVector

data_table = importr('data.table')
shapr = importr('shapr')
utils = importr('utils')
base = importr('base')


def maybe_null(val):
  return val if val is not None else NULL


def explain(
    model,
    x_explain: pd.DataFrame,
    x_train: pd.DataFrame,
    approach: str,
    prediction_zero: float,
    n_combinations: int | None = None,
    group: dict | None = None,
    n_samples: int = 1e3,
    n_batches: int | None = None,
    seed: int | None = 1,
    keep_samp_for_vS: bool = False,
    predict_model: Callable = None,
    get_model_specs: Callable = None,
    MSEv_uniform_comb_weights: bool = True,
    timing: bool = True,
    verbose: int | None = 0,
    **kwargs,
  ):
    '''Explain the output of machine learning models with more accurately estimated Shapley values.

    Computes dependence-aware Shapley values for observations in `x_explain` from the specified
    `model` by using the method specified in `approach` to estimate the conditional expectation.

    Parameters
    ----------
    model: The model whose predictions we want to explain.
      `shaprpy` natively supports `sklearn`, `xgboost` and `keras` models.
      Unsupported models can still be explained by passing `predict_model` and (optionally) `get_model_specs`.
    x_explain: Contains the features whose predictions ought to be explained.
    x_train: Contains the data used to estimate the (conditional) distributions for the features
      needed to properly estimate the conditional expectations in the Shapley formula.
    approach: str or list[str] of length `n_features`.
      `n_features` equals the total number of features in the model. All elements should,
      either be `"gaussian"`, `"copula"`, `"empirical"`, `"ctree"`, `"categorical"`, `"timeseries"`, or `"independence"`.
    prediction_zero: The prediction value for unseen data, i.e. an estimate of the expected prediction without conditioning on any
      features. Typically we set this value equal to the mean of the response variable in our training data, but other
      choices such as the mean of the predictions in the training data are also reasonable.
    n_combinations: If `group = None`, `n_combinations` represents the number of unique feature combinations to sample.
      If `group != None`, `n_combinations` represents the number of unique group combinations to sample.
      If `n_combinations = None`, the exact method is used and all combinations are considered.
      The maximum number of combinations equals `2^m`, where `m` is the number of features.
    group: If `None` regular feature wise Shapley values are computed.
      If a dict is provided, group wise Shapley values are computed. `group` then contains lists of unique feature names with the
      features included in each of the different groups. The length of the dict equals the number of groups.
    n_samples: Indicating the maximum number of samples to use in the
      Monte Carlo integration for every conditional expectation.
    n_batches: Specifies how many batches the total number of feature combinations should be split into when calculating the
      contribution function for each test observation.
      The default value is 1.
      Increasing the number of batches may significantly reduce the RAM allocation for models with many features.
      This typically comes with a small increase in computation time.
    seed: Specifies the seed before any randomness based code is being run.
      If `None` the seed will be inherited from the calling environment.
    keep_samp_for_vS: Indicates whether the samples used in the Monte Carlo estimation of v_S should be returned (in `internal['output']`)
    predict_model: The prediction function used when `model` is not natively supported.
      The function must have two arguments, `model` and `newdata` which specify, respectively, the model
      and a pandas.DataFrame to compute predictions for. The function must give the prediction as a numpy.Array.
      `None` (the default) uses functions specified internally.
      Can also be used to override the default function for natively supported model classes.
    get_model_specs: An optional function for checking model/data consistency when `model` is not natively supported.
      This method has yet to be implemented for keras models.
      The function takes `model` as argument and provides a `dict with 3 elements:
      - labels: list[str] with the names of each feature.
      - classes: list[str] with the classes of each features.
      - factor_levels: dict[str, list[str]] with the levels for any categorical features.
      If `None` (the default) internal functions are used for natively supported model classes, and the checking is
      disabled for unsupported model classes.
      Can also be used to override the default function for natively supported model classes.
    MSEv_uniform_comb_weights: Logical. If `True` (default), then the function weights the combinations
      uniformly when computing the MSEv criterion. If `False`, then the function use the Shapley kernel weights to
      weight the combinations when computing the MSEv criterion. Note that the Shapley kernel weights are replaced by
      the sampling frequency when not all combinations are considered.
    timing: Indicates whether the timing of the different parts of the explain call should be saved and returned.
    verbose:  An integer specifying the level of verbosity. If `0` (default), `shapr` will stay silent.
      If `1`, it will print information about performance. If `2`, some additional information will be printed out.
    kwargs: Further arguments passed to specific approaches. See R-documentation of the function
      `explain_tripledot_docs` for more information about the approach specific arguments
      (https://norskregnesentral.github.io/shapr/reference/explain_tripledot_docs.html). Note that the parameters
      in R are called 'approach.parameter_name', but in Python the equivalent would be 'approach_parameter_name'.

    Returns
    -------
    pandas.DataFrame
      A pandas.DataFrame with the Shapley values.
    numpy.Array
      A numpy.Array with the predictions on `x_explain`.
    dict
      A dictionary of additional information.
    dict
      A dictionary of elapsed time information if `timing` is set to `True`.
    dict
      A dictionary of the MSEv evaluation criterion scores: averaged over both the explicands and coalitions,
      only over the explicands, and only over the coalitions.
    '''

    timing_list = {"init_time": datetime.now()}

    base.set_seed(seed)

    # Gets and check feature specs from the model
    rfeature_specs = get_feature_specs(get_model_specs, model)

    # Fixes the conversion from dict to a named list of vectors in R
    r_group = NULL if group is None else ListVector({key: StrVector(value) for key, value in group.items()})

    # Fixes method specific argument names by replacing first occurrence of "_" with "."
    if len(kwargs) > 0:
      kwargs = change_first_underscore_to_dot(kwargs)

      # Convert from dict to a named list of vectors in R if `regression.vfold_cv_para` is provided by the user
      if 'regression.vfold_cv_para' in kwargs:
        kwargs['regression.vfold_cv_para'] = ListVector(kwargs['regression.vfold_cv_para'])

    # Sets up and organizes input parameters
    # Checks the input parameters and their compatability
    # Checks data/model compatability
    rinternal = shapr.setup(
        x_train = py2r(x_train),
        x_explain = py2r(x_explain),
        approach = approach,
        prediction_zero = prediction_zero,
        n_combinations = maybe_null(n_combinations),
        group = r_group,
        n_samples = n_samples,
        n_batches = maybe_null(n_batches),
        seed = seed,
        keep_samp_for_vS = keep_samp_for_vS,
        feature_specs = rfeature_specs,
        MSEv_uniform_comb_weights = MSEv_uniform_comb_weights,
        timing = timing,
        verbose = verbose,
        is_python=True,
        **kwargs
    )

    timing_list["setup"] = datetime.now()

    # Gets predict_model (if not passed to explain) and checks that predict_model gives correct format
    predict_model = get_predict_model(x_test=x_train.head(2), predict_model=predict_model, model=model)

    timing_list["test_prediction"] = datetime.now()

    # Add the predicted response of the training and explain data to the internal list for regression-based methods
    using_regression_paradigm = rinternal.rx2("parameters").rx2("regression")[0]
    if using_regression_paradigm:
      rinternal = regression_get_y_hat(rinternal, model, predict_model, x_train, x_explain)

    # Sets up the Shapley framework and prepares the conditional expectation computation for the chosen approach
    rinternal = shapr.setup_computation(rinternal, NULL, NULL)

    # Compute the v(S):
    # MC:
    # 1. Get the samples for the conditional distributions with the specified approach
    # 2. Predict with these samples
    # 3. Perform MC integration on these to estimate the conditional expectation (v(S))
    # Regression:
    # 1. Directly estimate the conditional expectation (v(S)) using the fitted regression model(s)
    rvS_list = compute_vS(rinternal, model, predict_model)

    timing_list["compute_vS"] = datetime.now()

    # Compute Shapley values based on conditional expectations (v(S))
    # Organize function output
    routput = shapr.finalize_explanation(vS_list=rvS_list, internal=rinternal)

    timing_list["shapley_computation"] = datetime.now()

    # Compute the elapsed time for the different steps
    timing = compute_time(timing_list) if timing else None

    # If regression, then delete the regression/tidymodels objects in routput as they cannot be converted to python
    if using_regression_paradigm:
        routput = regression_remove_objects(routput)

    # Convert R objects to Python objects
    df_shapley = r2py(base.as_data_frame(routput.rx2('shapley_values')))
    pred_explain = r2py(routput.rx2('pred_explain'))
    internal = recurse_r_tree(routput.rx2('internal'))
    MSEv = recurse_r_tree(routput.rx2('MSEv'))

    return df_shapley, pred_explain, internal, timing, MSEv


def compute_vS(rinternal, model, predict_model):
  S_batch = rinternal.rx2('objects').rx2('S_batch')
  ret = ro.ListVector({})
  for i, S in enumerate(S_batch):
    ret.rx2[i+1] = batch_compute_vS(S=S, rinternal=rinternal, model=model, predict_model=predict_model)
  return ret


def batch_compute_vS(S, rinternal, model, predict_model):
  regression = rinternal.rx2('parameters').rx2('regression')[0]

  # Check if we are to use regression or Monte Carlo integration to compute the contribution function values
  if regression:
    dt_vS = shapr.batch_prepare_vS_regression(S=S, internal=rinternal)
  else:
    # dt_vS is either only dt_vS or a list containing dt_vS and dt if internal$parameters$keep_samp_for_vS = TRUE
    dt_vS = batch_prepare_vS_MC(S=S, rinternal=rinternal, model=model, predict_model=predict_model)

  return dt_vS


def batch_prepare_vS_MC(S, rinternal, model, predict_model):
  keep_samp_for_vS = rinternal.rx2('parameters').rx2('keep_samp_for_vS')[0]
  feature_names = list(rinternal.rx2('parameters').rx2('feature_names'))
  dt = shapr.batch_prepare_vS_MC_auxiliary(S=S, internal=rinternal)
  dt = compute_preds(dt=dt, feature_names=feature_names, predict_model=predict_model, model=model)
  dt_vS = shapr.compute_MCint(dt)

  if keep_samp_for_vS:
    return ro.ListVector({'dt_vS':dt_vS, 'dt_samp_for_vS':dt})
  else:
    return dt_vS


def compute_preds(dt, feature_names, predict_model, model):
  preds = predict_model(model, r2py(dt).loc[:,feature_names])
  return ro.r.cbind(dt, p_hat=ro.FloatVector(preds.tolist()))



def get_feature_specs(get_model_specs, model):
  model_class0 = type(model)

  if (get_model_specs is not None) and (not callable(get_model_specs)):
    raise ValueError('`get_model_specs` must be None or callable.')

  if get_model_specs is None:
    get_model_specs = prebuilt_get_model_specs(model)
    if get_model_specs is None:
      warnings.warn(f'No pre-built get_model_specs for model of type {type(model)}, disabling checks.')
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
      strvec = ro.StrVector(list(v.values()))
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
    if is_regressor(model): return lambda m, x: m.predict(x)
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
  objects = ro.StrVector(("regression", "regression.model", "regression.tune_values", "regression.vfold_cv_para",
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