#' Group variables according to matrix of Kendall's tau
#'
#' @param X optional data matrix, ignores corMat if included
#' @param corMat optional correlation matrix, ignores X if included. Either X or corMat must be included
#' @param alpha optional tuning parameter for optimal number of clusters
#'
#' @return List
#'
#' @export
#'
#' @author Anders Løland
group_variables <- function(X = NULL, corMat = NULL, alpha = 1) {

    ## Kendall's tau
    if (is.null(corMat)) {
        corMat = pcaPP::cor.fk(X)
        ## Dimension
        d = nrow(X)
    }
    else {
        ## Dimension
        d = nrow(corMat)
    }

    ## Plot correlation matrix
    corrplot::corrplot(corMat, method = "square")
    ## Histogram of correlations
    hist(
        x = corMat[upper.tri(corMat)],
        main = "Distribution of Kendall's tau for the data",
        xlab = "",
        xlim = c(-1, 1),
        nclass = 30,
        col = "brown"
    )

    ## Use 1 - correlation as distance matrix
    dissimilarity = 1 - abs(corMat)
    distance = stats::as.dist(dissimilarity)

    ## Hierarchical clustering
    cluster = stats::hclust(distance)

    ## Plot dendrogram
    plot(cluster, main = "Dissimilarity = 1 - |Kendall's tau|", xlab = "")

    ## Find optimal number of clusters
    optimalK = maptree::kgs(cluster, distance, maxclus = d - 10, alpha = alpha)
    plot(
        x = names(optimalK),
        y = optimalK,
        xlab = "# of clusters",
        ylab = "penalty",
        type = "b",
        col = "green",
        lwd = 4,
        pch = 19
    )
    K = as.numeric(names(optimalK)[which(optimalK == min(optimalK))])

    ## Display variables with optimal cluster number
    clusterK = stats::cutree(cluster, k = K)
    clusterList = list()
    for (k in 1:K) {
        clusterList[[k]] = which(clusterK == k)
    }

    ## Plot correlation matrix, ordered in clusters
    ord = cluster$order
    corrplot::corrplot(corMat[ord, ord], method = "square")
    ## Add cluster rectangles
    corr_rect_hclust(corMat, cluster, k = K)

    list(corMat = corMat[ord, ord], K = K, cluster = cluster)
}

#' Draw rectangles on the correlation matrix graph
#'
#' @param corr correlation matrix
#' @param cluster cluster object from hclust
#' @param k number of clusters
#' @param col box colour
#' @param lwd box line width
#'
#' @export
#'
#' @author Anders Løland
corr_rect_hclust <- function(
                             corr,
                             cluster,
                             k = 2,
                             col = "yellow",
                             lwd = 6) {
    n <- nrow(corr)
    hc <- stats::cutree(cluster, k = k)
    clustab <- table(hc)[unique(hc[cluster$order])]
    cu <- c(0, cumsum(clustab))

    rect(cu[- (k + 1)] + 0.5,
        n - cu[- (k + 1)] + 0.5,
        cu[-1] + 0.5,
        n - cu[-1] + 0.5,
        border = col, lwd = lwd
    )
}
