# Rebuild long-running precomputed vignettes.
#
# The `.Rmd.orig` files contain executable code for vignettes that are too slow
# to run during routine package checks/builds. This helper knits those sources
# into the checked-in `.Rmd` files. Optionally, it converts generated PNG figures
# to WebP and rewrites image references to reduce repository and package size.
#
# Usage from the repository root:
#   Rscript dev/rebuild-long-running-vignettes.R
#   Rscript dev/rebuild-long-running-vignettes.R --webp
#   Rscript dev/rebuild-long-running-vignettes.R --webp --no-cache
#   Rscript dev/rebuild-long-running-vignettes.R --vignette general_usage --webp
#   SHAPR_OUTPUT_WIDTH=85 Rscript dev/rebuild-long-running-vignettes.R --webp

args <- commandArgs(trailingOnly = TRUE)

output_width <- suppressWarnings(as.integer(Sys.getenv("SHAPR_OUTPUT_WIDTH", unset = NA_character_)))

if (!is.na(output_width)) {
  output_width <- max(output_width, 20L)
  options(width = output_width, cli.width = output_width, crayon.width = output_width)
  Sys.setenv(COLUMNS = output_width, CLI_WIDTH = output_width)
}

convert_webp <- "--webp" %in% args
disable_cache <- "--no-cache" %in% args

vignette_arg_index <- match("--vignette", args)
selected_vignettes <- character()

if (!is.na(vignette_arg_index)) {
  if (vignette_arg_index == length(args)) {
    stop("`--vignette` must be followed by one or more vignette names.", call. = FALSE)
  }

  selected_vignettes <- args[(vignette_arg_index + 1):length(args)]
  selected_vignettes <- selected_vignettes[!grepl("^--", selected_vignettes)]
}

all_vignettes <- c(
  "general_usage",
  "vaeac",
  "regression",
  "asymmetric_causal"
)

vignettes <- if (length(selected_vignettes) == 0) all_vignettes else selected_vignettes
unknown_vignettes <- setdiff(vignettes, all_vignettes)

if (length(unknown_vignettes) > 0) {
  stop(
    "Unknown vignette(s): ",
    paste(unknown_vignettes, collapse = ", "),
    call. = FALSE
  )
}

repo_root <- tryCatch(
  system2("git", c("rev-parse", "--show-toplevel"), stdout = TRUE, stderr = FALSE),
  error = function(error) getwd()
)

if (length(repo_root) == 0 || is.na(repo_root[[1]])) {
  repo_root <- getwd()
}

repo_root <- repo_root[[1]]
vignette_dir <- file.path(repo_root, "vignettes")

if (!dir.exists(vignette_dir)) {
  stop("Could not find `vignettes/` from repository root: ", repo_root, call. = FALSE)
}

if (!requireNamespace("knitr", quietly = TRUE)) {
  stop("The `knitr` package is required to rebuild vignettes.", call. = FALSE)
}

if (convert_webp) {
  if (!requireNamespace("png", quietly = TRUE)) {
    stop("The `png` package is required when using `--webp`.", call. = FALSE)
  }
  if (!requireNamespace("webp", quietly = TRUE)) {
    stop("The `webp` package is required when using `--webp`.", call. = FALSE)
  }
}

# knitr can occasionally concatenate consecutive cli info messages in the rendered `.Rmd` output.
fix_cli_info_newlines <- function(file) {
  info_symbol <- intToUtf8(0x2139)
  bad_boundary <- paste0(".", info_symbol, " ")
  fixed_boundary <- paste0(".\n#> ", info_symbol, " ")

  lines <- readLines(file, warn = FALSE)
  fixed_lines <- lines
  changed <- startsWith(lines, "#>") & grepl(bad_boundary, lines, fixed = TRUE)

  if (any(changed)) {
    fixed_lines[changed] <- gsub(
      bad_boundary,
      fixed_boundary,
      fixed_lines[changed],
      fixed = TRUE
    )
    writeLines(fixed_lines, file)
  }

  return(sum(changed))
}

scan_rendered_vignette_errors <- function(file) {
  error_pattern <- "^(#>\\s*)?(Error in|Error:|Execution halted|Quitting from lines|Backtrace:|Traceback)"
  lines <- readLines(file, warn = FALSE)
  line_numbers <- grep(error_pattern, lines)

  if (length(line_numbers) == 0) {
    return(character())
  }

  findings <- paste0(file, ":", line_numbers, ": ", lines[line_numbers])
  message("Potential rendered execution error(s) in ", file, ":")
  message(paste0("  ", findings, collapse = "\n"))

  return(findings)
}

message("Rebuilding long-running vignette(s): ", paste(vignettes, collapse = ", "))

if (disable_cache) {
  message("Disabling knitr cache for this rebuild.")
}

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(vignette_dir)

old_cache_hook <- knitr::opts_hooks$get("cache")

if (disable_cache) {
  knitr::opts_hooks$set(cache = function(options) {
    options$cache <- FALSE
    return(options)
  })

  on.exit(knitr::opts_hooks$set(cache = old_cache_hook), add = TRUE)
}

rendered_error_findings <- character()

for (vignette in vignettes) {
  input <- paste0(vignette, ".Rmd.orig")
  output <- paste0(vignette, ".Rmd")

  message("Knitting ", input, " -> ", output)
  knitr::knit(input = input, output = output, quiet = FALSE)

  n_cli_newlines_fixed <- fix_cli_info_newlines(output)
  if (n_cli_newlines_fixed > 0) {
    message("Fixed ", n_cli_newlines_fixed, " missing CLI newline(s) in ", output)
  }

  rendered_error_findings <- c(rendered_error_findings, scan_rendered_vignette_errors(output))

  if (!convert_webp) {
    next
  }

  message("Converting generated PNG figures to WebP for ", vignette, ".")

  figure_dir <- paste0("figure_", vignette)
  png_files <- list.files(figure_dir, pattern = "\\.png$", full.names = TRUE)

  for (png_file in png_files) {
    image <- png::readPNG(png_file)
    webp_file <- sub("\\.png$", ".webp", png_file)

    webp::write_webp(image, webp_file, quality = 80)
    unlink(png_file)
  }

  rmd_file <- paste0(vignette, ".Rmd")
  rmd <- readLines(rmd_file, warn = FALSE)
  rmd <- gsub(".png", ".webp", rmd, fixed = TRUE)
  writeLines(rmd, rmd_file)
}

if (!convert_webp) {
  message("Skipping PNG to WebP conversion. Re-run with `--webp` to convert generated vignette figures.")
}

if (length(rendered_error_findings) > 0) {
  message("\nPotential rendered vignette execution error(s) found:")
  message(paste0("  ", rendered_error_findings, collapse = "\n"))
}
