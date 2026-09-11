args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args[[1]])) {
  cli::cli_abort("Usage: Rscript dev/summary-cli-output.R <R-library>")
}
library_path <- normalizePath(args[[1]], mustWork = TRUE)
.libPaths(c(library_path, .libPaths()))
namespace <- loadNamespace("shapr", lib.loc = library_path)
if (normalizePath(getNamespaceInfo(namespace, "path")) != file.path(library_path, "shapr")) {
  cli::cli_abort("R did not load shapr from the requested library.")
}
options(width = 100, cli.width = 100, cli.num_colors = 1, progressr.enable = FALSE)

set.seed(17)
x_train <- data.frame(first = rnorm(30), second = rnorm(30), third = rnorm(30))
y_train <- with(x_train, first + 2 * second - third)
model <- stats::lm(y_train ~ ., data = x_train)

explanation <- shapr::explain(
  model = model,
  x_train = x_train,
  x_explain = x_train,
  approach = "independence",
  phi0 = mean(y_train),
  n_MC_samples = 10,
  iterative = FALSE,
  seed = 1,
  verbose = NULL,
  testing = TRUE,
  extra_computation_args = list(vS_batching_method = "forloop")
)

print(summary(explanation))