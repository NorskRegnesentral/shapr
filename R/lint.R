#' Configuration of linting
#'
#' @description This function is used when linting the package. We're modifying some of
#' the default settings. Note that the tests, i.e when running \code{devtools::test}
#' will fail if the package contains lints. At the moment \code{devtools::test} will not
#' report the different lints, so you'll need to run
#' \code{lintr::lint_package(linters = linters_config())} to get information of what lines
#' contains lints.
#'
#' @return List
#' @export
linters_config <- function() {
    lintr::with_defaults(
        object_usage_linter = NULL,
        absolute_paths_linter = NULL,
        assignment_linter = NULL,
        line_length_linter = NULL,
        multiple_dots_linter = NULL,
        commented_code_linter = NULL,
        open_curly_linter = NULL,
        object_name_linter = NULL,
        object_length_linter = lintr::object_length_linter(40)
    )
}
