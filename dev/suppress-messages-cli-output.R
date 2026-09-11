args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L || !nzchar(args[[1]])) {
  cli::cli_abort("Usage: Rscript dev/suppress-messages-cli-output.R <R-library>")
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

run_example <- function() {
  explanation <- shapr::explain(
    model = model,
    x_train = x_train,
    x_explain = x_train[1:2, ],
    approach = "independence",
    phi0 = mean(y_train),
    n_MC_samples = 10,
    iterative = FALSE,
    seed = 1,
    verbose = c("basic", "shapley"),
    testing = TRUE,
    extra_computation_args = list(vS_batching_method = "forloop")
  )
  print(summary(explanation))
  return(explanation)
}

cat("Loaded shapr:", getNamespaceInfo(namespace, "path"), "\n")
cat("\n=== Without suppressMessages: explanation and summary should appear ===\n")
visible_result <- run_example()

cat("\n=== With suppressMessages: nothing should appear before END ===\n")
silent_result <- suppressMessages(run_example())
cat("=== END suppressed section ===\n")

cat("\nSame Shapley estimates:", isTRUE(all.equal(
  visible_result$shapley_values_est,
  silent_result$shapley_values_est
)), "\n")