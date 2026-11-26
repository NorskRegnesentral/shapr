library(igraph)

# Convert a chain graph to the format used by the shapr package. This is the same
# function as in ../vignettes/asymmetric_causal.Rmd.orig (chain_graph_test.R checks this).
#
# graph: igraph graph with an edge attribute `type`:
#   "directed"   = causal arrow between chain components,
#   "bidirected" = dependence inside a component from a hidden confounder,
#   "undirected" = mutual interaction inside a component (no confounding).
# Stops if the graph only gives a partial order of the components, as shapr's
# causal_ordering must order all components.
# Helper written for this vignette; not part of shapr and not as thoroughly
# tested. Check the output against the graph before using it.
chain_graph_to_shapr <- function(graph, feature_names = igraph::V(graph)$name) {
  has_type <- igraph::ecount(graph) == 0 || "type" %in% igraph::edge_attr_names(graph)
  if (!igraph::is_directed(graph) || is.null(igraph::V(graph)$name) || !has_type) {
    stop("`graph` must be a directed igraph graph with vertex names and an edge attribute `type`.", call. = FALSE)
  }
  if (anyDuplicated(feature_names) > 0 || !setequal(feature_names, igraph::V(graph)$name)) {
    stop("`feature_names` must contain each vertex name of `graph` exactly once.", call. = FALSE)
  }
  edges <- igraph::as_data_frame(graph, what = "edges")
  if (!all(edges$type %in% c("directed", "bidirected", "undirected"))) {
    stop("The edge attribute `type` must be 'directed', 'bidirected', or 'undirected'.", call. = FALSE)
  }
  is_directed <- edges$type == "directed"

  # Step 1: the chain components
  within_graph <- igraph::graph_from_data_frame(
    edges[!is_directed, ],
    directed = FALSE,
    vertices = data.frame(name = feature_names)
  )
  component <- igraph::components(within_graph)$membership
  n_components <- max(component)

  # Step 2: order the components along the directed edges
  component_edges <- unique(data.frame(
    from = component[edges$from[is_directed]],
    to = component[edges$to[is_directed]]
  ))
  if (any(component_edges$from == component_edges$to)) {
    stop("A directed edge connects two features in the same chain component.", call. = FALSE)
  }
  component_graph <- igraph::graph_from_data_frame(
    component_edges,
    directed = TRUE,
    vertices = data.frame(name = seq_len(n_components))
  )
  if (!igraph::is_dag(component_graph)) {
    stop("The directed edges form a cycle between the chain components.", call. = FALSE)
  }

  # shapr needs a total order: every pair of components must be linked by a directed path
  is_reachable <- is.finite(igraph::distances(component_graph, mode = "out"))
  unordered <- which(!is_reachable & !t(is_reachable) & upper.tri(is_reachable), arr.ind = TRUE)
  if (nrow(unordered) > 0) {
    unordered <- unordered[order(unordered[, 1], unordered[, 2]), , drop = FALSE]
    component_label <- function(k) {
      return(vapply(k, function(i) {
        return(paste0("{", paste(feature_names[component[feature_names] == i], collapse = ", "), "}"))
      }, character(1)))
    }
    stop(
      "The graph only gives a partial order of the chain components, but `causal_ordering` must ",
      "order all components. No directed path links ",
      paste(component_label(unordered[, 1]), "and", component_label(unordered[, 2]), collapse = "; "),
      ". Add directed edges that order these components, or merge them into one chain component.",
      call. = FALSE
    )
  }
  component_order <- as.integer(igraph::topo_sort(component_graph, mode = "out"))
  causal_ordering <- lapply(component_order, function(k) feature_names[component[feature_names] == k])

  # Step 3: a component is confounded if its edges are bidirected
  confounding <- vapply(component_order, function(k) {
    types <- unique(edges$type[!is_directed & component[edges$from] == k])
    if (length(types) > 1) {
      stop("A chain component has both bidirected and undirected edges.", call. = FALSE)
    }
    return(identical(types, "bidirected"))
  }, logical(1))

  return(list(causal_ordering = causal_ordering, confounding = confounding))
}

# Plot a chain graph in the style of the shapr vignette (asymmetric_causal):
# a grey box per chain component, one arrow per pair of components, and the
# edges inside each component.
# style = "ordering": edges inside components as plain lines (no assumption
#                     about where the dependence comes from).
# style = "chain":    a confounded component (bidirected edges) gets a hidden
#                     common cause Z drawn above it; mutual interactions
#                     (undirected edges) are drawn as two-way arrows.
# layout: matrix with one row per feature (row names = feature names).
plot_chain_graph <- function(g, layout, style = c("chain", "ordering"), main = NULL,
                             node_radius = 0.55, label_cex = 0.8) {
  style <- match.arg(style)
  res <- chain_graph_to_shapr(g)
  comps <- res$causal_ordering
  n_comp <- length(comps)
  lay <- layout[V(g)$name, , drop = FALSE]
  comp_of <- setNames(rep(seq_len(n_comp), lengths(comps)), unlist(comps))

  # Hidden confounders: one per confounded component, drawn above it
  latent <- vector("list", n_comp)
  if (style == "chain") {
    for (k in which(res$confounding)) {
      m <- lay[comps[[k]], , drop = FALSE]
      latent[[k]] <- c(mean(m[, 1]), max(m[, 2]) + 2.2 * node_radius)
    }
  }

  # Component boxes
  pad <- node_radius + 0.25
  boxes <- t(sapply(seq_len(n_comp), function(k) {
    pts <- rbind(lay[comps[[k]], , drop = FALSE], latent[[k]])
    c(xmin = min(pts[, 1]) - pad, xmax = max(pts[, 1]) + pad,
      ymin = min(pts[, 2]) - pad, ymax = max(pts[, 2]) + pad)
  }))
  rounded_rect <- function(b, r = 0.3) {
    a <- seq(0, pi / 2, length.out = 10)
    x <- c(b["xmax"] - r + r * cos(a), b["xmin"] + r - r * sin(a),
           b["xmin"] + r - r * cos(a), b["xmax"] - r + r * sin(a))
    y <- c(b["ymax"] - r + r * sin(a), b["ymax"] - r + r * cos(a),
           b["ymin"] + r - r * sin(a), b["ymin"] + r - r * cos(a))
    polygon(x, y, col = "grey90", border = NA)
  }

  # Point where the line from the box centre in direction d leaves the box
  box_exit <- function(b, d) {
    ctr <- c(mean(b[c("xmin", "xmax")]), mean(b[c("ymin", "ymax")]))
    half <- c(b["xmax"] - b["xmin"], b["ymax"] - b["ymin"]) / 2
    ctr + d * min(half / abs(d))
  }
  # Line between two circles, shortened so it starts and ends at the circle edges
  node_line <- function(p1, p2, r1 = node_radius, r2 = node_radius) {
    u <- (p2 - p1) / sqrt(sum((p2 - p1)^2))
    c(p1 + u * r1, p2 - u * r2)
  }
  circle <- function(p, lty = 1) {
    a <- seq(0, 2 * pi, length.out = 100)
    polygon(p[1] + node_radius * cos(a), p[2] + node_radius * sin(a), col = "white", lty = lty, lwd = 1.5)
  }

  par(mar = c(0, 0, if (is.null(main)) 0 else 2, 0))
  plot.new()
  plot.window(xlim = range(boxes[, c("xmin", "xmax")]),
              ylim = range(boxes[, c("ymin", "ymax")]) + c(0, 0.5), asp = 1)
  if (!is.null(main)) title(main)

  for (k in seq_len(n_comp)) {
    rounded_rect(boxes[k, ])
    text(mean(boxes[k, c("xmin", "xmax")]), boxes[k, "ymax"] + 0.25, bquote(tau[.(k)]), cex = 1.3)
  }

  # Arrows between components (one per pair of components with a directed edge)
  el <- as_edgelist(g)
  type <- E(g)$type
  dir <- unique(cbind(comp_of[el[type == "directed", 1]], comp_of[el[type == "directed", 2]]))
  for (i in seq_len(nrow(dir))) {
    b1 <- boxes[dir[i, 1], ]
    b2 <- boxes[dir[i, 2], ]
    y_overlap <- c(max(b1["ymin"], b2["ymin"]), min(b1["ymax"], b2["ymax"]))
    x_overlap <- c(max(b1["xmin"], b2["xmin"]), min(b1["xmax"], b2["xmax"]))
    if (diff(y_overlap) > 0.5) {
      # Boxes side by side: horizontal arrow in the middle of their common height
      y <- mean(y_overlap)
      right <- b2["xmin"] > b1["xmax"]
      p1 <- c(if (right) b1["xmax"] + 0.15 else b1["xmin"] - 0.15, y)
      p2 <- c(if (right) b2["xmin"] - 0.15 else b2["xmax"] + 0.15, y)
    } else if (diff(x_overlap) > 0.5) {
      # Boxes above each other: vertical arrow in the middle of their common width
      x <- mean(x_overlap)
      down <- b2["ymax"] < b1["ymin"]
      p1 <- c(x, if (down) b1["ymin"] - 0.15 else b1["ymax"] + 0.15)
      p2 <- c(x, if (down) b2["ymax"] + 0.15 else b2["ymin"] - 0.15)
    } else {
      c1 <- c(mean(b1[c("xmin", "xmax")]), mean(b1[c("ymin", "ymax")]))
      c2 <- c(mean(b2[c("xmin", "xmax")]), mean(b2[c("ymin", "ymax")]))
      d <- (c2 - c1) / sqrt(sum((c2 - c1)^2))
      p1 <- box_exit(b1, d) + 0.15 * d
      p2 <- box_exit(b2, -d) - 0.15 * d
    }
    arrows(p1[1], p1[2], p2[1], p2[2], lwd = 2, length = 0.12)
  }

  # Edges inside the components
  for (i in which(type != "directed")) {
    p <- node_line(lay[el[i, 1], ], lay[el[i, 2], ])
    if (style == "ordering") {
      segments(p[1], p[2], p[3], p[4])
    } else if (type[i] == "undirected") {
      arrows(p[1], p[2], p[3], p[4], code = 3, length = 0.07)
    } # bidirected edges are drawn as a hidden confounder below
  }
  z <- 0 # confounders are numbered Z_1, Z_2, ... in component order
  for (k in which(!sapply(latent, is.null))) {
    z <- z + 1
    for (v in comps[[k]]) {
      p <- node_line(latent[[k]], lay[v, ])
      arrows(p[1], p[2], p[3], p[4], length = 0.07)
    }
    circle(latent[[k]], lty = 2)
    text(latent[[k]][1], latent[[k]][2], bquote(Z[.(z)]), cex = 1.2)
  }

  # Feature nodes
  for (v in rownames(lay)) {
    circle(lay[v, ])
    text(lay[v, 1], lay[v, 2], v, cex = label_cex)
  }
  invisible(res)
}

# ------------------------------------------------------------------
# The bike sharing example from the vignette
# ------------------------------------------------------------------
feat <- c("trend", "cosyear", "sinyear", "temp", "atemp", "windspeed", "hum")
season <- c("cosyear", "sinyear")
weather <- c("temp", "atemp", "windspeed", "hum")

pairs <- function(v) { p <- t(combn(v, 2)); data.frame(from = p[, 1], to = p[, 2]) }
cross <- function(a, b) expand.grid(from = a, to = b, stringsAsFactors = FALSE)
arrows_between <- rbind(
  cbind(cross("trend", c(season, weather)), type = "directed"),   # causal arrows
  cbind(cross(season, weather),            type = "directed")
)

# Causal ordering: only which features belong together and the order of the
# components; nothing is assumed about the dependence inside a component.
g_causal_ordering <- graph_from_data_frame(rbind(
  arrows_between,
  cbind(pairs(season),  type = "undirected"),
  cbind(pairs(weather), type = "undirected")
), directed = TRUE, vertices = data.frame(name = feat))

# Causal chain graph: also says where the dependence inside a component comes from.
g_causal_chain <- graph_from_data_frame(rbind(
  arrows_between,
  cbind(pairs(season),  type = "bidirected"), # common hidden confounder: cosyear <-> sinyear
  cbind(pairs(weather), type = "undirected")  # mutual interactions: temp -- atemp ...
), directed = TRUE, vertices = data.frame(name = feat))
g <- g_causal_chain # name used by chain_graph_plot.R

# Plot both graphs (layout as in the vignette figures)
lay <- rbind(trend = c(0, 3.6), cosyear = c(2.8, 3.6), sinyear = c(4.6, 3.6),
             temp = c(1.4, 1.0), atemp = c(3.2, 1.0), windspeed = c(1.4, -0.8), hum = c(3.2, -0.8))
plot_chain_graph(g_causal_ordering, lay, style = "ordering", main = "Causal ordering")
plot_chain_graph(g_causal_chain, lay, style = "chain", main = "Causal chain graph")

# ------------------------------------------------------------------
# shapr arguments
# ------------------------------------------------------------------
# Asymmetric conditional Shapley values only need the ordering (confounding = NULL):
res_ordering <- chain_graph_to_shapr(g_causal_ordering, feat)
res_ordering$causal_ordering

# Causal Shapley values need both:
res <- chain_graph_to_shapr(g_causal_chain, feat)
str(res)
# Indices instead of names:
print(lapply(res$causal_ordering, match, table = feat))
