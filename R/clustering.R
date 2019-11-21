#' Group variables according to matrix of Kendall's tau
#'
#' @param x_mat Numeric matrix. Ignored if \code{cor_mat} is included.
#' @param cor_mat Numeric matrix. Note that \code{x_mat} or \code{cor_mat} must be included,
#' otherwise the function will return an error message.
#' @param alpha Numeric. Weight for number of clusters. See \code{\link[maptree]{kgs}}.
#'
#' @details The returned list contains the following items
#' \describe{
#'   \item{cor_mat}{TODO: Add information}
#'   \item{which_k}{TODO: Add information}
#'   \item{cluster}{TODO: Add information}
#' }
#'
#' @return List
#'
#' @export
#'
#' @author Anders Løland
cluster_features <- function(x_mat = NULL, cor_mat = NULL, alpha = 1) {

  if (!requireNamespace("pcaPP", quietly = TRUE)) {
    stop("pcaPP is not installed. Please run install.packages('pcaPP')")
  }
  if (!requireNamespace("corrplot", quietly = TRUE)) {
    stop("corrplot is not installed. Please run install.packages('corrplot')")
  }
  if (!requireNamespace("maptree", quietly = TRUE)) {
    stop("maptree is not installed. Please run install.packages('maptree')")
  }

  ## Kendall's tau
  if (is.null(cor_mat)) {
    cor_mat <- pcaPP::cor.fk(x_mat)
    ## Dimension
    d <- nrow(x_mat)
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
  which_k <- as.numeric(names(optimal_k)[which(optimal_k == min(optimal_k))])

  ## Display variables with optimal cluster number
  cluster_k <- stats::cutree(cluster, k = which_k)
  cluster_list <- list()
  for (k in 1:which_k) {
    cluster_list[[k]] <- which(cluster_k == k)
  }

  ## Plot correlation matrix, ordered in clusters
  ord <- cluster$order
  corrplot::corrplot(cor_mat[ord, ord], method = "square")
  ## Add cluster rectangles
  correlation_rectangles(cor_mat, cluster, k = which_k)

  list(cor_mat = cor_mat[ord, ord], which_k = which_k, cluster = cluster)
}

#' Draw rectangles on the correlation matrix graph
#'
#' @param corr Numeric matrix. Needs to satisfy the conditions for a correlation matrix.
#' @param cluster An object of class \code{hclust}. See \code{\link[stats]{hclust}}
#' @param k Integer. Number of clusters
#' @param col Character. Represents the box color.
#' @param lwd Integer. Represents box line width.
#'
#' @keywords internal
#'
#' @author Anders Løland
correlation_rectangles <- function(corr,
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
