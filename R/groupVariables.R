#' Group variables according to matrix of Kendall's tau
#'
#' @param x optional data matrix, ignores \code{corMat} if included
#' @param cor_mat optional correlation matrix, ignores x if included. Either \code{X} or
#' \code{corMat} must be included
#' @param alpha optional tuning parameter for optimal number of clusters
#'
#' @return List
#'
#' @export
#'
#' @author Anders Løland
cluster_features <- function(x = NULL, cor_mat = NULL, alpha = 1) {

  ## Kendall's tau
  if (is.null(cor_mat)) {
    cor_mat <- pcaPP::cor.fk(x)
    ## Dimension
    d <- nrow(x)
  }
  else {
    ## Dimension
    d <- nrow(cor_mat)
  }

  ## Plot correlation matrix
  corrplot::corrplot(cor_mat, method = "square")
  ## Histogram of correlations
  hist(
    x = cor_mat[upper.tri(cor_mat)],
    main = "Distribution of Kendall's tau for the data",
    xlab = "",
    xlim = c(-1, 1),
    nclass = 30,
    col = "brown"
  )

  ## Use 1 - correlation as distance matrix
  dissimilarity <- 1 - abs(cor_mat)
  distance <- stats::as.dist(dissimilarity)

  ## Hierarchical clustering
  cluster <- stats::hclust(distance)

  ## Plot dendrogram
  plot(cluster, main = "Dissimilarity = 1 - |Kendall's tau|", xlab = "")

  ## Find optimal number of clusters
  optimal_k <- maptree::kgs(cluster, distance, maxclus = d - 10, alpha = alpha)
  plot(
    x = names(optimal_k),
    y = optimal_k,
    xlab = "# of clusters",
    ylab = "penalty",
    type = "b",
    col = "green",
    lwd = 4,
    pch = 19
  )
  k <- as.numeric(names(optimal_k)[which(optimal_k == min(optimal_k))])

  ## Display variables with optimal cluster number
  cluster_k <- stats::cutree(cluster, k = k)
  cluster_list <- list()
  for (k in 1:k) {
    cluster_list[[k]] <- which(cluster_k == k)
  }

  ## Plot correlation matrix, ordered in clusters
  ord <- cluster$order
  corrplot::corrplot(cor_mat[ord, ord], method = "square")
  ## Add cluster rectangles
  correlation_rectangles(cor_mat, cluster, k = k)

  list(cor_mat = cor_mat[ord, ord], k = k, cluster = cluster)
}

#' Draw rectangles on the correlation matrix graph
#'
#' @param corr correlation matrix
#' @param cluster An object of class \code{hclust}. See \code{\link[stats]{hclust}}
#' @param k number of clusters
#' @param col box color
#' @param lwd box line width
#'
#' @export
#'
#' @author Anders Løland
correlation_rectangles <- function(
                                   corr,
                                   cluster,
                                   k = 2,
                                   col = "yellow",
                                   lwd = 6) {
  n <- nrow(corr)
  hc <- stats::cutree(cluster, k = k)
  clustab <- table(hc)[unique(hc[cluster$order])]
  cu <- c(0, cumsum(clustab))

  ind <- k + 1
  rect(cu[-ind] + 0.5,
    n - cu[-ind] + 0.5,
    cu[-1] + 0.5,
    n - cu[-1] + 0.5,
    border = col, lwd = lwd
  )
}
