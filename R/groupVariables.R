#' Group variables according to matrix of Kendall's tau
#'
#' @param X matrix
#'
#' @return List
#'
#' @export
#'
#' @author Anders Løland
group_variables <- function(X) {

    ## Dimension
    d = nrow(X)

    ## Kendall's tau
    corMat = pcaPP::cor.fk(X)

    ## Plot correlation matrix
    corrplot::corrplot(corMat, method = "square")
    ## Histogram of correlations
    hist(corMat[upper.tri(corMat)],main="Distribution of Kendall's tau for  data",xlab="",xlim=c(-1,1),nclass=30,col="brown")

    ## Use 1 - correlation as distance matrix
    dissimilarity = 1 - abs(corMat)
    distance = stats::as.dist(dissimilarity)

    ## Hierarchical clustering
    cluster = stats::hclust(distance)

    ## Plot dendrogram
    plot(cluster, main="Dissimilarity = 1 - |Kendall's tau|", xlab="")

    ## Find optimal number of clusters
    optimalK = maptree::kgs(cluster, distance, maxclus = d-10, alpha = 0.1)
    plot(names(optimalK), optimalK, xlab="# of clusters", ylab="penalty",type="b",col="green",lwd=4,pch=19)
    K = as.numeric(names(optimalK)[which(optimalK==min(optimalK))])

    ## Display variables with optimal cluster number
    clusterK = stats::cutree(cluster,k=K)
    clusterList = list()
    for(k in 1:K){
        clusterList[[k]] = which(clusterK==k)
    }

    ## Plot correlation matrix, ordered in clusters
    ord = cluster$order
    corrplot::corrplot(corMat[ord,ord], method = "square")
    ## Add cluster rectangles
    corr_rect_hclust(corMat,k=K)

    list(corMat = corMat)
}

#' Draw rectangles on the correlation matrix graph
#'
#' @param corr correlation matrix
#' @param k number of clusters
#' @param col box colour
#' @param lwd box line width
#'
#' @export
#'
#' @author Anders Løland
corr_rect_hclust <- function(
    corr,
    k = 2,
    col = "yellow",
    lwd = 6){
    n <- nrow(corr)
    tree <- stats::hclust(stats::as.dist(1 - abs(corr)))
    hc <- stats::cutree(tree, k = k)
    clustab <- table(hc)[unique(hc[tree$order])]
    cu <- c(0, cumsum(clustab))

    rect(cu[-(k + 1)] + 0.5,
        n - cu[-(k + 1)] + 0.5,
        cu[-1] + 0.5,
        n - cu[-1] + 0.5,
        border = col, lwd = lwd)
}
