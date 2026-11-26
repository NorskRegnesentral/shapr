# ggraph version of the plots in chain_graph.R, for comparison with the base
# graphics version (plot_chain_graph()). Same style as the shapr vignette
# (asymmetric_causal): a grey box per chain component, one arrow per pair of
# components, and the edges inside each component.
# Run from this folder: source("chain_graph_ggraph.R")
# Needs: ggraph, ggforce (geom_shape for rounded boxes), patchwork (side by side).
suppressMessages({
  library(igraph)
  library(ggraph)
  library(ggforce)
  library(patchwork)
})

# chain_graph.R gives chain_graph_to_shapr(), g_causal_ordering, g_causal_chain
# and the layout `lay` (it also draws the base graphics plots).
source("chain_graph.R")

# style = "ordering": edges inside components as plain lines.
# style = "chain":    a confounded component (bidirected edges) gets a hidden
#                     common cause Z drawn above it; mutual interactions
#                     (undirected edges) are drawn as two-way arrows.
# layout: matrix with one row per feature (row names = feature names).
# All sizes are in data units (the plot uses coord_fixed), so the plot can be
# saved at any size.
plot_chain_ggraph <- function(g, layout, style = c("chain", "ordering"), title = NULL,
                              node_radius = 0.55, label_size = 3.2) {
  style <- match.arg(style)
  res <- chain_graph_to_shapr(g)
  comps <- res$causal_ordering
  n_comp <- length(comps)
  comp_of <- setNames(rep(seq_len(n_comp), lengths(comps)), unlist(comps))
  feat <- V(g)$name

  # Nodes: the features, plus one hidden confounder per confounded component
  nodes <- data.frame(name = feat, x = layout[feat, 1], y = layout[feat, 2],
                      component = comp_of[feat], latent = FALSE, label = feat)
  el <- as_edgelist(g)
  type <- E(g)$type
  inside <- type != "directed"
  edges <- data.frame(from = el[inside, 1], to = el[inside, 2],
                      kind = if (style == "ordering") "plain" else ifelse(type[inside] == "undirected", "mutual", "confounded"))
  if (style == "chain") {
    edges <- edges[edges$kind != "confounded", ] # replaced by the hidden confounders below
    z <- 0
    for (k in which(res$confounding)) {
      z <- z + 1
      m <- nodes[nodes$component == k, ]
      zname <- paste0(".Z", z)
      nodes <- rbind(nodes, data.frame(name = zname, x = mean(m$x), y = max(m$y) + 2.2 * node_radius,
                                       component = k, latent = TRUE, label = paste0("Z[", z, "]")))
      edges <- rbind(edges, data.frame(from = zname, to = m$name, kind = "latent"))
    }
  }
  g_plot <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

  # Component boxes in data units
  pad <- node_radius + 0.25
  boxes <- do.call(rbind, lapply(seq_len(n_comp), function(k) {
    m <- nodes[nodes$component == k, ]
    data.frame(component = k, xmin = min(m$x) - pad, xmax = max(m$x) + pad,
               ymin = min(m$y) - pad, ymax = max(m$y) + pad)
  }))
  box_poly <- do.call(rbind, lapply(seq_len(n_comp), function(k) {
    b <- boxes[k, ]
    data.frame(component = k, x = c(b$xmin, b$xmax, b$xmax, b$xmin), y = c(b$ymin, b$ymin, b$ymax, b$ymax))
  }))
  tau <- data.frame(x = (boxes$xmin + boxes$xmax) / 2, y = boxes$ymax + 0.3,
                    label = paste0("tau[", boxes$component, "]"))

  # One arrow per pair of components with a directed edge between them:
  # straight when the boxes overlap vertically or horizontally, else centre to centre
  dir <- unique(cbind(comp_of[el[!inside, 1]], comp_of[el[!inside, 2]]))
  gap <- 0.15
  box_arrow <- function(b1, b2) {
    y_ov <- c(max(b1$ymin, b2$ymin), min(b1$ymax, b2$ymax))
    x_ov <- c(max(b1$xmin, b2$xmin), min(b1$xmax, b2$xmax))
    if (diff(y_ov) > 0.5) {
      y <- mean(y_ov); right <- b2$xmin > b1$xmax
      return(c(if (right) b1$xmax + gap else b1$xmin - gap, y, if (right) b2$xmin - gap else b2$xmax + gap, y))
    }
    if (diff(x_ov) > 0.5) {
      x <- mean(x_ov); down <- b2$ymax < b1$ymin
      return(c(x, if (down) b1$ymin - gap else b1$ymax + gap, x, if (down) b2$ymax + gap else b2$ymin - gap))
    }
    ctr <- function(b) c((b$xmin + b$xmax) / 2, (b$ymin + b$ymax) / 2)
    exit <- function(b, d) ctr(b) + d * min(c(b$xmax - b$xmin, b$ymax - b$ymin) / 2 / abs(d))
    d <- ctr(b2) - ctr(b1); d <- d / sqrt(sum(d^2))
    c(exit(b1, d) + gap * d, exit(b2, -d) - gap * d)
  }
  comp_arrows <- as.data.frame(do.call(rbind, lapply(seq_len(nrow(dir)), function(i) {
    box_arrow(boxes[dir[i, 1], ], boxes[dir[i, 2], ])
  })))
  names(comp_arrows) <- c("x", "y", "xend", "yend")

  cap <- circle(node_radius, "native")
  small_arrow <- function(ends = "last") arrow(length = unit(2, "mm"), type = "open", ends = ends)

  ggraph(g_plot, layout = "manual", x = nodes$x, y = nodes$y) +
    geom_shape(data = box_poly, aes(x, y, group = component), radius = unit(4, "mm"),
               fill = "grey90", colour = NA, inherit.aes = FALSE) +
    geom_text(data = tau, aes(x, y, label = label), parse = TRUE, size = 5, inherit.aes = FALSE) +
    geom_segment(data = comp_arrows, aes(x, y, xend = xend, yend = yend), linewidth = 0.8,
                 arrow = arrow(length = unit(3, "mm"), type = "open"), inherit.aes = FALSE) +
    geom_edge_link(aes(filter = kind == "plain"), start_cap = cap, end_cap = cap) +
    geom_edge_link(aes(filter = kind == "mutual"), start_cap = cap, end_cap = cap, arrow = small_arrow("both")) +
    geom_edge_link(aes(filter = kind == "latent"), start_cap = cap, end_cap = cap, arrow = small_arrow()) +
    geom_node_circle(aes(r = node_radius, linetype = latent), fill = "white", linewidth = 0.6, show.legend = FALSE) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "dashed")) +
    geom_node_text(aes(label = label, size = ifelse(latent, 1.6, 1) * label_size), parse = TRUE) +
    scale_size_identity() +
    coord_fixed(clip = "off") +
    labs(title = title) +
    theme_graph(base_family = "sans", title_size = 14, plot_margin = margin(10, 10, 10, 10)) +
    theme(plot.title = element_text(hjust = 0.5))
}

p_ordering <- plot_chain_ggraph(g_causal_ordering, lay, style = "ordering", title = "Causal ordering")
p_chain <- plot_chain_ggraph(g_causal_chain, lay, style = "chain", title = "Causal chain graph")
print(p_ordering + p_chain)

# ggsave("chain_graph_ggraph.png", p_ordering + p_chain, width = 11, height = 6.5, bg = "white")

# ------------------------------------------------------------------
# shapr arguments: from the same graphs the plots are drawn from
# ------------------------------------------------------------------
res_chain <- chain_graph_to_shapr(g_causal_chain)       # causal Shapley values
res_ordering <- chain_graph_to_shapr(g_causal_ordering) # asymmetric: use causal_ordering only
str(res_chain)
# tidygraph objects (common with ggraph) work too:
# chain_graph_to_shapr(tidygraph::as_tbl_graph(g_causal_chain))
