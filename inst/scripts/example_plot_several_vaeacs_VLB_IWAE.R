library(xgboost)
data("airquality")
data <- data.table::as.data.table(airquality)
data <- data[complete.cases(data), ]

x_var <- c("Solar.R", "Wind", "Temp", "Month")
y_var <- "Ozone"

ind_x_explain <- 1:6
x_train <- data[-ind_x_explain, ..x_var]
y_train <- data[-ind_x_explain, get(y_var)]
x_explain <- data[ind_x_explain, ..x_var]

# Fitting a basic xgboost model to the training data
model <- xgboost(
  data = as.matrix(x_train),
  label = y_train,
  nround = 100,
  verbose = FALSE
)

# Specifying the phi_0, i.e. the expected prediction without any features
p0 <- mean(y_train)

# Train several different NN
explanation_paired_sampling_TRUE <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = p0,
  n_batches = 2,
  n_samples = 1, #' As we are only interested in the training of the vaeac
  vaeac.epochs = 25, #' Should be higher in applications.
  vaeac.n_vaeacs_initialize = 5,
  vaeac.extra_parameters = list(
    vaeac.paired_sampling = TRUE,
    vaeac.verbose = TRUE
  )
)

explanation_paired_sampling_FALSE <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = p0,
  n_batches = 2,
  n_samples = 1, #' As we are only interested in the training of the vaeac
  vaeac.epochs = 25, #' Should be higher in applications.
  vaeac.n_vaeacs_initialize = 5,
  vaeac.extra_parameters = list(
    vaeac.paired_sampling = FALSE,
    vaeac.verbose = TRUE
  )
)

# Other networks have 4.76 times more parameters.
explanation_paired_sampling_FALSE_small <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = p0,
  n_batches = 2,
  n_samples = 1, #' As we are only interested in the training of the vaeac
  vaeac.epochs = 25, #' Should be higher in applications.
  vaeac.width = 16, #' Default is 32
  vaeac.depth = 2, #' Default is 3
  vaeac.latent_dim = 4, #' Default is 8
  vaeac.n_vaeacs_initialize = 5,
  vaeac.extra_parameters = list(
    vaeac.paired_sampling = FALSE,
    vaeac.verbose = TRUE
  )
)

explanation_paired_sampling_TRUE_small <- explain(
  model = model,
  x_explain = x_explain,
  x_train = x_train,
  approach = approach,
  prediction_zero = p0,
  n_batches = 2,
  n_samples = 1, #' As we are only interested in the training of the vaeac
  vaeac.epochs = 25, #' Should be higher in applications.
  vaeac.width = 16, #' Default is 32
  vaeac.depth = 2, #' Default is 3
  vaeac.latent_dim = 4, #' Default is 8
  vaeac.n_vaeacs_initialize = 5,
  vaeac.extra_parameters = list(
    vaeac.paired_sampling = TRUE,
    vaeac.verbose = TRUE
  )
)

# Collect the explanation objects in an unnamed list
explanation_list_unnamed <- list(
  explanation_paired_sampling_FALSE,
  explanation_paired_sampling_FALSE_small,
  explanation_paired_sampling_TRUE,
  explanation_paired_sampling_TRUE_small
)

# Collect the explanation objects in an named list
explanation_list_named <- list(
  "Regular samp. & large NN" = explanation_paired_sampling_FALSE,
  "Regular samp. & small NN" = explanation_paired_sampling_FALSE_small,
  "Paired samp. & large NN" = explanation_paired_sampling_TRUE,
  "Paired samp. & small NN" = explanation_paired_sampling_TRUE_small
)

# Call the function with the unnamed list, will create names
vaeac_plot_eval_crit(explanation_list = explanation_list_unnamed)

# Call the function with the named list, will use the provided names
# See that the paired samplign often produce more stable results
vaeac_plot_eval_crit(explanation_list = explanation_list_named)

# The function also works if we have only one method, but then one should only look at the method plot
vaeac_plot_eval_crit(explanation_list = list("Paired samp. & large NN" = explanation_paired_sampling_TRUE),
                     plot_type = "method")

# Can alter the plot
vaeac_plot_eval_crit(
  explanation_list = explanation_list_named,
  plot_from_nth_epoch = 5,
  plot_every_nth_epoch = 3,
  facet_wrap_scales = "free"
)

# If we want only want the criterion version
tmp_fig_criterion = vaeac_plot_eval_crit(
  explanation_list = explanation_list_named,
  plot_type = "criterion")

# We can add points
tmp_fig_criterion + ggplot2::geom_point(shape = "circle", size = 1, ggplot2::aes(col = Method))

# If we rather want smooths with se bands
tmp_fig_criterion$layers[[1]] = NULL
tmp_fig_criterion + ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE) +
  ggplot2::scale_color_brewer(palette = "Set1") +
  ggplot2::theme_minimal()

# If we only want the VLB
vaeac_plot_eval_crit(
  explanation_list = explanation_list_named,
  criteria = "VLB",
  plot_type = "criterion")
