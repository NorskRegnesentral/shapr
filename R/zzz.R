.onLoad <- function(libname = find.package("shapr"), pkgname = "shapr") {
  # CRAN Note avoidance
  utils::globalVariables(
    c(
      ".",
      ".N",
      ".I",
      ".GRP",
      ".SD",
      "joint_prob",
      "N",
      "id_all",
      "id",
      "id_combination",
      "w",
      "id_all",
      "joint_prob",
      "cond_prob",
      "marg_prob",
      "n_features",
      "p_hat",
      "k",
      "model_class",
      "rn",
      "get_model_specs",
      "native_get_model_specs",
      "from",
      "predict_model",
      "native_predict_model",
      "native",
      "rank_waterfall",
      "end",
      "start",
      "phi_significant",
      "y_text",
      "hjust_text",
      "arrow_color",
      "sign",
      "y_text_bar",
      "hjust_text_bar",
      "feature_value",
      "positive",
      "feature_value_scaled",
      "text_color_bar",
      "unique_label",
      "pred_label",
      "pred_x",
      "element_rect",
      "element_line",
      "guide_colourbar",
      "x_start",
      "x_end",
      "y_start",
      "y_end",
      "phi0_x",
      "phi0_label",
      "phi",
      "header",
      "variable",
      "pred",
      "description",
      "min",
      "max",
      "features",
      "shapley_weight",
      "features_tmp",
      "sample_frequence",
      "is_duplicate",
      "groups",
      "n_groups",
      "groups_tmp",
      "approach",
      "n_leftover_first_batch",
      "n_S_per_approach",
      "n_batches_per_approach",
      "randomorder",
      "batch",
      "type",
      "feature_value_factor",
      "horizon_id_combination",
      "tmp_features",
      "Method",
      "MSEv",
      "MSEv_sd",
      "error",
      ".header",
      ".id",
      ".pred",
      ".only_these_features_wo_none",
      ".only_these_columns",
      "Epoch",
      ".description",
      ".phi",
      ".method",
      "Value",
      "Criterion",
      "checkpoint",
      "..col_cont_names",
      "n_train",
      "one_hot_max_sizes",
      "train_dataloader",
      "vaeac_model_best_listmodel",
      "vaeac_save_file_names",
      "val_dataloader",
      "x_train",
      "x_train_preprocessed",
      "x_train_torch"
    )
  )

  if (requireNamespace("torch", quietly = TRUE)) {
    skip_connection <- torch::nn_module(
      classname = "skip_connection", # field classname Name of the of torch::nn_module object
      # description Initialize a new skip_connection module
      initialize = function(...) self$inner_net <- torch::nn_sequential(...),
      # description What to do when a skip_connection module is called
      forward = function(input) {
        return(input + self$inner_net(input))
      }
    )
  } else {
    skip_connection <- R6::R6Class(
      "skip_connection",
      public = list(
        initialize = function() {
          stop("The 'torch' package is required for 'skip_connection'. Please install 'torch'.", call. = FALSE)
        }
      )
    )
  }
  assign("skip_connection", skip_connection, envir = .GlobalEnv)

  invisible()

}
