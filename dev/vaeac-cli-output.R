args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 4L) {
  cli::cli_abort("Usage: Rscript dev/vaeac-cli-output.R <R-library> [messages|progress] [workers] [short|long]")
}
library_path <- normalizePath(args[[1]], mustWork = TRUE)
mode <- if (length(args) >= 2L) args[[2]] else "messages"
workers <- if (length(args) >= 3L) suppressWarnings(as.numeric(args[[3]])) else 1
preset <- if (length(args) == 4L) args[[4]] else "short"
if (!preset %in% c("short", "long")) {
  cli::cli_abort("Preset must be short or long.")
}
settings <- if (preset == "long") {
  list(n_train = 1024, n_explain = 8, epochs = 60, width = 64, depth = 3, n_MC_samples = 500)
} else {
  list(n_train = 128, n_explain = 2, epochs = 12, width = 16, depth = 2, n_MC_samples = 10)
}
if (!is.finite(workers) || workers < 1 || workers != floor(workers)) {
  cli::cli_abort("Workers must be a positive integer.")
}
if (!mode %in% c("messages", "progress")) {
  cli::cli_abort("Mode must be messages or progress.")
}
if (!file.exists(file.path(library_path, "shapr", "DESCRIPTION"))) {
  cli::cli_abort("No installed shapr package found in {.path {library_path}}.")
}
.libPaths(c(library_path, .libPaths()))
namespace <- loadNamespace("shapr", lib.loc = library_path)
loaded_path <- normalizePath(getNamespaceInfo(namespace, "path"))
if (loaded_path != file.path(library_path, "shapr")) {
  cli::cli_abort("R loaded an unexpected installation: {.path {loaded_path}}.")
}
options(width = 100, cli.width = 100, cli.num_colors = 1)
cli::cli_text("Loaded shapr: {.path {loaded_path}}")
for (function_name in c("vaeac_print_train_summary", "print_iter", "print.summary.shapr")) {
  function_body <- deparse(body(get(function_name, envir = namespace)))
  output_call <- grep("rlang::inform|cli::cli_verbatim", function_body, value = TRUE)
  cli::cli_verbatim(paste(function_name, paste(trimws(output_call), collapse = " "), sep = ": "))
}

torch::torch_set_num_threads(1)
torch::torch_set_num_interop_threads(1)
set.seed(17)
x_train <- data.frame(
  first = rnorm(settings$n_train), second = rnorm(settings$n_train), third = rnorm(settings$n_train)
)
y_train <- with(x_train, first + 2 * second - third)
model <- stats::lm(y_train ~ ., data = x_train)

run_example <- function() {
  previous_plan <- future::plan()
  on.exit(future::plan(previous_plan), add = TRUE)
  if (workers > 1) {
    if (!parallelly::supportsMulticore()) {
      cli::cli_abort("VAEAC requires multicore for this parallel example. Use a Linux Rscript terminal.")
    }
    future::plan(future::multicore, workers = workers)
  } else {
    future::plan(future::sequential)
  }
  cli::cli_text("Future workers: {future::nbrOfWorkers()}; Torch threads per process: 1.")
  cli::cli_text("Preset: {preset}; training rows: {settings$n_train}; epochs: {settings$epochs}.")
  explanation <- shapr::explain(
    model = model,
    x_train = x_train,
    x_explain = x_train[seq_len(settings$n_explain), ],
    approach = "vaeac",
    phi0 = mean(y_train),
    n_MC_samples = settings$n_MC_samples,
    iterative = FALSE,
    seed = 1,
    verbose = c("basic", "vS_details", "shapley"),
    extra_computation_args = list(vS_batching_method = "future"),
    vaeac.epochs = settings$epochs,
    vaeac.n_vaeacs_initialize = 2,
    vaeac.width = settings$width,
    vaeac.depth = settings$depth,
    vaeac.extra_parameters = list(vaeac.cuda = FALSE)
  )
  return(explanation)
}

if (mode == "progress") {
  explanation <- progressr::with_progress(
    run_example(),
    handlers = progressr::handler_cli(enable = TRUE, clear = FALSE),
    enable = TRUE,
    interval = 0
  )
} else {
  options(progressr.enable = FALSE)
  explanation <- run_example()
}
print(explanation)
print(summary(explanation))