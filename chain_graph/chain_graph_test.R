# Verification of chain_graph_to_shapr() from vignettes/asymmetric_causal.Rmd.orig.
# Run from the chain_graph folder: Rscript chain_graph_test.R
#
# 1. The function in chain_graph.R is the same as in the vignette.
# 2. Fixed examples: the bike sharing graph (igraph and dagitty) gives the vignette's arguments.
# 3. Random valid chain graphs: the output equals the components and order used to generate the graph.
# 4. Random invalid graphs: partial orders, cycles, directed edges within a component, and mixed
#    edge types give the right error, and a partial-order error lists exactly the unordered pairs.
# 5. Invalid input (wrong type values, missing attribute, unnamed vertices, ...) gives an error.
# 6. Random graphs written in dagitty syntax give the same output after conversion.
# 7. shapr accepts the output: explain() runs and gives the same values for names and indices.
suppressMessages({
  library(igraph)
  library(dagitty)
  library(shapr)
})

#### Load the function from the vignette and from chain_graph.R ####
vignette_lines <- readLines("../vignettes/asymmetric_causal.Rmd.orig")
chunk_start <- grep("^```\\{r graph_function", vignette_lines)
chunk_end <- chunk_start + which(vignette_lines[(chunk_start + 1):length(vignette_lines)] == "```")[1]
eval(parse(text = vignette_lines[(chunk_start + 1):(chunk_end - 1)]))

root_env <- new.env()
for (e in parse("chain_graph.R")) {
  if (is.call(e) && identical(e[[2]], as.name("chain_graph_to_shapr"))) eval(e, root_env)
}

results <- list()
check <- function(group, ok, info = "") {
  results[[group]] <<- c(results[[group]], isTRUE(ok))
  if (!isTRUE(ok) && sum(!results[[group]]) <= 3) cat("FAIL [", group, "]", info, "\n")
}
error_message <- function(expr) {
  tryCatch({
    expr
    NA_character_
  }, error = function(e) conditionMessage(e))
}

check("same function in chain_graph.R and vignette",
      identical(deparse(chain_graph_to_shapr), deparse(root_env$chain_graph_to_shapr)))

#### Fixed examples ####
x_var <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
season <- c("cosyear", "sinyear")
weather <- c("temp", "atemp", "windspeed", "hum")
weather_pairs <- combn(weather, 2)
bike_edges <- rbind(
  data.frame(from = "trend", to = c(season, weather), type = "directed"),
  data.frame(from = rep(season, each = 4), to = weather, type = "directed"),
  data.frame(from = "cosyear", to = "sinyear", type = "bidirected"),
  data.frame(from = weather_pairs[1, ], to = weather_pairs[2, ], type = "undirected")
)
bike_graph <- graph_from_data_frame(bike_edges, directed = TRUE, vertices = data.frame(name = x_var))
bike_expected <- list(
  causal_ordering = list("trend", season, weather),
  confounding = c(FALSE, TRUE, FALSE)
)
check("fixed examples", identical(chain_graph_to_shapr(bike_graph, x_var), bike_expected), "bike igraph")

dagitty_to_igraph <- function(dagitty_graph) {
  dagitty_edges <- dagitty::edges(dagitty_graph)
  dagitty_type <- c("->" = "directed", "<->" = "bidirected", "--" = "undirected")
  return(graph_from_data_frame(
    data.frame(from = dagitty_edges$v, to = dagitty_edges$w, type = unname(dagitty_type[as.character(dagitty_edges$e)])),
    directed = TRUE,
    vertices = data.frame(name = names(dagitty_graph))
  ))
}
bike_dagitty <- dagitty("mag {
  trend -> {cosyear sinyear temp atemp windspeed hum}
  {cosyear sinyear} -> {temp atemp windspeed hum}
  cosyear <-> sinyear
  temp -- {atemp windspeed hum}
  atemp -- {windspeed hum}
  windspeed -- hum
}")
check("fixed examples", identical(chain_graph_to_shapr(dagitty_to_igraph(bike_dagitty), x_var), bike_expected),
      "bike dagitty")

# Group-level graph from the vignette's groups section
group_graph <- graph_from_data_frame(rbind(
  data.frame(from = "trend", to = c("cosyear", "sinyear", "temp_group", "windspeed", "hum"), type = "directed"),
  data.frame(from = rep(c("cosyear", "sinyear"), each = 3), to = c("temp_group", "windspeed", "hum"), type = "directed"),
  data.frame(from = "cosyear", to = "sinyear", type = "bidirected"),
  data.frame(from = c("temp_group", "temp_group", "windspeed"), to = c("windspeed", "hum", "hum"), type = "undirected")
), directed = TRUE)
check("fixed examples", identical(
  chain_graph_to_shapr(group_graph, c("trend", "cosyear", "sinyear", "temp_group", "windspeed", "hum")),
  list(causal_ordering = list("trend", c("cosyear", "sinyear"), c("temp_group", "windspeed", "hum")),
       confounding = c(FALSE, TRUE, FALSE))
), "groups")

#### Random chain graph generator ####
# Returns the graph and the truth used to make it. With `total = TRUE`, every pair of neighboring
# components gets a directed edge, which makes the order total.
name_pool <- c(
  paste0("x", 1:40), paste0("feature_", 1:20), "a.b", "A", "B", "z9",
  "1", "2", "type", "from", "to", "name", "X 1", "long_feature_name_with_underscores"
)
random_chain_graph <- function(n_features_max = 12, p_within = 0.3, p_between = 0.2, safe_names = FALSE,
                               total = TRUE) {
  pool <- if (safe_names) paste0("v", 1:60) else name_pool
  n_features <- sample(1:n_features_max, 1)
  features <- sample(pool, n_features)
  n_components <- sample(1:n_features, 1)
  # Split the features into n_components non-empty components (in random sizes)
  cut_points <- sort(sample(seq_len(n_features - 1), n_components - 1))
  components <- unname(split(features, findInterval(seq_len(n_features), cut_points + 1)))
  is_confounded <- sample(c(TRUE, FALSE), n_components, replace = TRUE)

  edges <- list()
  for (k in seq_len(n_components)) {
    members <- components[[k]]
    if (length(members) < 2) next
    type <- if (is_confounded[k]) "bidirected" else "undirected"
    # A spanning tree makes the component connected; then random extra edges
    for (i in 2:length(members)) edges[[length(edges) + 1]] <- c(members[i], sample(members[1:(i - 1)], 1), type)
    pairs <- combn(members, 2)
    for (j in seq_len(ncol(pairs))) {
      if (runif(1) < p_within) edges[[length(edges) + 1]] <- c(pairs[, j], type)
    }
  }
  for (k in seq_len(n_components - 1)) {
    if (total) {
      edges[[length(edges) + 1]] <- c(sample(components[[k]], 1), sample(components[[k + 1]], 1), "directed")
    }
    for (l in (k + 1):n_components) {
      for (from in components[[k]]) {
        for (to in components[[l]]) {
          # Without a total order, no direct edges between neighbors (so they may be unordered)
          if ((total || l > k + 1) && runif(1) < p_between) edges[[length(edges) + 1]] <- c(from, to, "directed")
        }
      }
    }
  }
  if (length(edges) > 0) {
    edges <- as.data.frame(do.call(rbind, edges))
  } else {
    edges <- data.frame(V1 = character(), V2 = character(), V3 = character())
  }
  names(edges) <- c("from", "to", "type")
  # Random edge order, random direction of the non-directed edges, and some duplicated edges
  flip <- edges$type != "directed" & runif(nrow(edges)) < 0.5
  edges[flip, c("from", "to")] <- edges[flip, c("to", "from")]
  if (nrow(edges) > 0) {
    edges <- edges[c(seq_len(nrow(edges)), sample(seq_len(nrow(edges)), rbinom(1, nrow(edges), 0.05))), ]
    edges <- edges[sample(nrow(edges)), ]
  }
  graph <- graph_from_data_frame(edges, directed = TRUE, vertices = data.frame(name = sample(features)))
  return(list(graph = graph, edges = edges, components = components, is_confounded = is_confounded,
              feature_names = sample(features)))
}

expected_output <- function(x) {
  return(list(
    causal_ordering = lapply(x$components, function(members) x$feature_names[x$feature_names %in% members]),
    confounding = x$is_confounded & lengths(x$components) > 1
  ))
}

# Pairs of components without a directed path between them, from a transitive closure (Warshall)
unordered_pairs <- function(x) {
  n <- length(x$components)
  component_of <- setNames(rep(seq_len(n), lengths(x$components)), unlist(x$components))
  reach <- diag(n) > 0
  directed <- x$edges[x$edges$type == "directed", ]
  reach[cbind(component_of[directed$from], component_of[directed$to])] <- TRUE
  for (m in seq_len(n)) reach <- reach | outer(reach[, m], reach[m, ], "&")
  pairs <- which(!reach & !t(reach) & upper.tri(reach), arr.ind = TRUE)
  if (nrow(pairs) == 0) return(character())
  return(sort(apply(pairs, 1, function(ij) {
    labels <- vapply(ij, function(k) paste(sort(x$components[[k]]), collapse = ","), character(1))
    return(paste(sort(labels), collapse = " | "))
  })))
}
pairs_in_message <- function(message) {
  listed <- sub("^.*No directed path links (.*)\\. Add directed edges.*$", "\\1", message)
  listed <- strsplit(listed, "; ", fixed = TRUE)[[1]]
  return(sort(vapply(listed, function(pair) {
    sides <- gsub("[{}]", "", strsplit(pair, "} and {", fixed = TRUE)[[1]])
    labels <- vapply(sides, function(s) paste(sort(strsplit(s, ", ", fixed = TRUE)[[1]]), collapse = ","), character(1))
    return(paste(sort(labels), collapse = " | "))
  }, character(1), USE.NAMES = FALSE)))
}

#### Random valid chain graphs ####
n_random <- 3000
for (i in seq_len(n_random)) {
  set.seed(i)
  x <- random_chain_graph(p_within = runif(1, 0, 0.8), p_between = runif(1, 0, 0.6))
  out <- tryCatch(chain_graph_to_shapr(x$graph, x$feature_names), error = function(e) conditionMessage(e))
  check("random valid graphs", identical(out, expected_output(x)), paste("seed", i))
  # Default feature_names (the vertex order of the graph) gives the same components and order
  out_default <- tryCatch(chain_graph_to_shapr(x$graph), error = function(e) list())
  check("random valid graphs, default feature_names",
        identical(lapply(out_default$causal_ordering, sort), lapply(x$components, sort)) &&
          identical(out_default$confounding, expected_output(x)$confounding), paste("seed", i))
}

#### Random invalid graphs ####
for (i in seq_len(n_random)) {
  set.seed(100000 + i)
  # Partial order: no direct edges between neighboring components
  x <- random_chain_graph(p_between = runif(1, 0, 0.6), total = FALSE)
  truth <- unordered_pairs(x)
  message <- error_message(chain_graph_to_shapr(x$graph, x$feature_names))
  if (length(truth) == 0) {
    check("partial order: total orders still accepted", is.na(message), paste("seed", 100000 + i))
  } else {
    check("partial order: error", grepl("only gives a partial order", message), paste("seed", 100000 + i))
    check("partial order: exactly the unordered pairs are listed",
          identical(pairs_in_message(message), truth), paste("seed", 100000 + i))
  }

  # Cycle: an edge from a later component back to an earlier one
  x <- random_chain_graph(n_features_max = 10)
  if (length(x$components) >= 2) {
    k <- sort(sample(seq_along(x$components), 2))
    graph <- add_edges(x$graph, c(sample(x$components[[k[2]]], 1), sample(x$components[[k[1]]], 1)), type = "directed")
    check("cycle: error", grepl("cycle", error_message(chain_graph_to_shapr(graph, x$feature_names))),
          paste("seed", 100000 + i))
  }

  # Directed edge within a component, and mixed edge types within a component
  multi <- which(lengths(x$components) > 1)
  if (length(multi) > 0) {
    k <- multi[sample.int(length(multi), 1)]
    graph <- add_edges(x$graph, sample(x$components[[k]], 2), type = "directed")
    check("directed edge within a component: error",
          grepl("same chain component", error_message(chain_graph_to_shapr(graph, x$feature_names))),
          paste("seed", 100000 + i))

    other <- if (x$is_confounded[k]) "undirected" else "bidirected"
    graph <- add_edges(x$graph, sample(x$components[[k]], 2), type = other)
    check("mixed edge types: error",
          grepl("both bidirected and undirected", error_message(chain_graph_to_shapr(graph, x$feature_names))),
          paste("seed", 100000 + i))
  }
}

#### Invalid input ####
g_ab <- graph_from_data_frame(data.frame(from = "A", to = "B", type = "directed"))
invalid_inputs <- list(
  "unknown edge type" = graph_from_data_frame(data.frame(from = "A", to = "B", type = "causes")),
  "NA edge type" = graph_from_data_frame(data.frame(from = "A", to = "B", type = NA)),
  "missing type attribute" = graph_from_data_frame(data.frame(from = "A", to = "B")),
  "unnamed vertices" = set_edge_attr(make_graph(c(1, 2)), "type", value = "directed"),
  "undirected igraph object" = graph_from_data_frame(data.frame(from = "A", to = "B", type = "directed"), directed = FALSE)
)
for (case in names(invalid_inputs)) {
  message <- error_message(chain_graph_to_shapr(invalid_inputs[[case]]))
  check("invalid input gives an error", !is.na(message), case)
}
check("invalid input gives an error", !is.na(error_message(chain_graph_to_shapr(g_ab, c("A", "X")))),
      "feature_names not the vertex names")
check("invalid input gives an error", !is.na(error_message(chain_graph_to_shapr(g_ab, c("A", "B", "B")))),
      "duplicated feature_names")

#### Random graphs in dagitty syntax ####
for (i in seq_len(300)) {
  set.seed(200000 + i)
  x <- random_chain_graph(safe_names = TRUE)
  # A single feature gives a graph without edges, for which dagitty::edges() returns no columns
  if (length(x$feature_names) < 2) next
  arrow <- c(directed = "->", bidirected = "<->", undirected = "--")
  lines <- c(unlist(x$components), paste(x$edges$from, arrow[x$edges$type], x$edges$to))
  dagitty_graph <- dagitty(paste0("mag {\n", paste(lines, collapse = "\n"), "\n}"))
  out <- tryCatch(chain_graph_to_shapr(dagitty_to_igraph(dagitty_graph), x$feature_names),
                  error = function(e) conditionMessage(e))
  check("random graphs via dagitty", identical(out, expected_output(x)), paste("seed", 200000 + i))
}

#### shapr accepts the output ####
for (i in seq_len(40)) {
  set.seed(300000 + i)
  x <- random_chain_graph(n_features_max = 6, safe_names = TRUE)
  if (length(x$feature_names) < 2) next
  args <- chain_graph_to_shapr(x$graph, x$feature_names)
  p <- length(x$feature_names)
  x_data <- as.data.frame(matrix(rnorm(500 * p), 500, p) %*% chol(crossprod(matrix(rnorm(p * p), p))))
  names(x_data) <- x$feature_names
  y <- as.vector(as.matrix(x_data) %*% rnorm(p) + x_data[[1]] * x_data[[p]] + rnorm(500, sd = 0.1))
  model <- lm(y ~ .^2, data = cbind(x_data, y = y))
  asymmetric <- sample(c(TRUE, FALSE), 1)
  run_explain <- function(causal_ordering) {
    explanation <- suppressMessages(suppressWarnings(explain(
      model = model, x_explain = x_data[1:5, ], x_train = x_data, approach = "gaussian", phi0 = mean(y),
      seed = 1, n_MC_samples = 100, iterative = FALSE, asymmetric = asymmetric,
      causal_ordering = causal_ordering, confounding = args$confounding, verbose = NULL
    )))
    return(explanation$shapley_values_est)
  }
  by_name <- tryCatch(run_explain(args$causal_ordering), error = function(e) conditionMessage(e))
  by_index <- tryCatch(run_explain(lapply(args$causal_ordering, match, table = x$feature_names)),
                       error = function(e) conditionMessage(e))
  check("shapr explain() accepts the output", is.data.frame(by_name), paste("seed", 300000 + i, by_name))
  check("shapr: same values for names and indices", isTRUE(all.equal(by_name, by_index)), paste("seed", 300000 + i))
}

#### Summary ####
cat("\n")
for (group in names(results)) {
  cat(sprintf("%-60s %5d / %5d passed\n", group, sum(results[[group]]), length(results[[group]])))
}
n_failed <- sum(!unlist(results))
cat(if (n_failed == 0) "\nAll checks passed.\n" else sprintf("\n%d check(s) failed.\n", n_failed))
