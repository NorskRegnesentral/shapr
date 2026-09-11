skip_on_cran()

test_that("explanation and summary messages retain their spacing when knitted", {
  skip_if_not_installed("knitr")

  previous_options <- options(cli.width = 120, cli.num_colors = 1)
  on.exit(options(previous_options), add = TRUE)
  run_explanation <- function(verbose = c("basic", "shapley")) {
    return(explain(
      model = model_lm_numeric,
      x_train = x_train_numeric,
      x_explain = x_explain_numeric[1],
      approach = "independence",
      phi0 = p0,
      n_MC_samples = 10,
      iterative = FALSE,
      verbose = verbose,
      seed = 1,
      testing = TRUE
    ))
  }

  messages <- list()
  explanation <- withCallingHandlers(run_explanation(), message = function(condition) {
    messages[[length(messages) + 1L]] <<- condition
    invokeRestart("muffleMessage")
  })
  expect_true(any(grepl("max_n_coalitions", vapply(messages, conditionMessage, character(1)))))
  expect_true(any(grepl("Final estimated Shapley values", vapply(messages, conditionMessage, character(1)))))
  expect_true(all(vapply(messages, inherits, logical(1), "cliMessage")))
  expect_true(all(endsWith(vapply(messages, conditionMessage, character(1)), "\n")))
  expect_equal(explanation$shapley_values, run_explanation(NULL)$shapley_values)
  expect_message(suppressMessages(run_explanation()), NA)

  summary_object <- summary(explanation)
  table_text <- "  feature    value\n  {literal}   1.25\n  second      2.50"
  attr(summary_object, "print_data")$formatted_shapley_info <- table_text
  summary_messages <- character()
  invisible(withCallingHandlers(print(summary_object), message = function(condition) {
    summary_messages <<- c(summary_messages, conditionMessage(condition))
    invokeRestart("muffleMessage")
  }))
  expect_true(paste0(table_text, "\n") %in% summary_messages)
  expect_message(suppressMessages(print(summary_object)), NA)

  rendered <- knitr::knit(
    text = c(
      "```{r, echo=FALSE, collapse=TRUE, comment='#>', error=FALSE}",
      "invisible(run_explanation())",
      "print(summary_object)",
      "cli::cli_ul(c('Spacing first', 'Spacing second'))",
      "```"
    ),
    envir = environment(),
    quiet = TRUE
  )
  expect_match(rendered, "Model class:[^\n]+\n#> [^\n]+v\\(S\\) estimation class:")
  expect_match(rendered, "Spacing first\n#> [^\n]+Spacing second")
  expect_match(rendered, "#>   feature    value\n#>   {literal}   1.25\n#>   second      2.50", fixed = TRUE)
})
