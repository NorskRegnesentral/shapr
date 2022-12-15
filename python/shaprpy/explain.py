import warnings
import numpy as np
import rpy2.robjects as ro
from rpy2.robjects.packages import importr
from rpy2.rinterface import NULL, NA
from .utils import r2py, py2r, recurse_r_tree
data_table = importr('data.table')
shapr = importr('shapr')
utils = importr('utils')
base = importr('base')


def maybe_null(val):
  return val if val is not None else NULL


def explain(
    model,
    x_explain,
    x_train,
    approach,
    prediction_zero,
    n_combinations = None,
    group = None,
    n_samples = 1e3,
    n_batches = 1,
    seed = 1,
    keep_samp_for_vS = False,
    predict_model = None,
    get_model_specs = None,
  ):

    base.set_seed(seed)

    rfeature_specs = get_feature_specs(get_model_specs, model)

    rinternal = shapr.setup(
        x_train = py2r(x_train),
        x_explain = py2r(x_explain),
        approach = approach,
        prediction_zero = py2r(prediction_zero),
        n_combinations = maybe_null(n_combinations),
        group = maybe_null(group),
        n_samples = n_samples,
        n_batches = n_batches,
        seed = seed,
        keep_samp_for_vS = keep_samp_for_vS,
        feature_specs = rfeature_specs,
        is_python=True,
    )

    predict_model = get_predict_model(
      x_test = x_train.head(2),
      predict_model = predict_model,
      model = model,
    )
    rinternal = shapr.setup_computation(rinternal, NULL, NULL)
    rvS_list = compute_vS(rinternal, model, predict_model)
    routput = shapr.finalize_explanation(
        vS_list = rvS_list,
        internal = rinternal,
    )

    df_shapley = r2py(base.as_data_frame(routput.rx2('shapley_values')))
    pred_explain = r2py(routput.rx2('pred_explain'))
    internal = recurse_r_tree(routput.rx2('internal'))
    return df_shapley, pred_explain, internal


def compute_vS(rinternal, model, predict_model):
  S_batch = rinternal.rx2('objects').rx2('S_batch')
  ret = ro.ListVector({})
  for i, S in enumerate(S_batch):
    ret.rx2[i+1] = batch_compute_vS(
      S=S,
      rinternal=rinternal,
      model=model,
      predict_model=predict_model,
    )
  return ret


def batch_compute_vS(S, rinternal, model, predict_model):
  keep_samp_for_vS = rinternal.rx2('parameters').rx2('keep_samp_for_vS')[0]
  feature_names = list(rinternal.rx2('parameters').rx2('feature_names'))
  dt = shapr.batch_prepare_vS(S=S, internal=rinternal)
  dt = compute_preds(
    dt, 
    feature_names=feature_names, 
    predict_model=predict_model,
    model=model)
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

  return None