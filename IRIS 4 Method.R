
library(cluster); library(dbscan); library(mclust)
set.seed(42)

kmeans_minus <- function(data, k, l, max_iter=60, n_restarts=6) {
  n <- nrow(data); best <- NULL; bw <- Inf
  for (r in seq_len(n_restarts)) {
    ce <- data[sample(n,k),,drop=FALSE]; as <- rep(1,n); mk <- rep(TRUE,n)
    for (it in seq_len(max_iter)) {
      dm <- matrix(0,n,k)
      for (j in seq_len(k)) dm[,j] <- rowSums(sweep(data,2,ce[j,],"-")^2)
      md <- apply(dm,1,min); as <- apply(dm,1,which.min)
      oi <- order(md,decreasing=TRUE)[seq_len(l)]; mk <- rep(TRUE,n); mk[oi] <- FALSE
      nc <- ce
      for (j in seq_len(k)) { m <- which(as==j & mk)
      if (length(m)>0) nc[j,] <- colMeans(data[m,,drop=FALSE]) }
      if (max(abs(nc-ce))<1e-6) break
      ce <- nc
    }
    w <- 0
    for (j in seq_len(k)) { m <- which(as==j & mk)
    w <- w + sum(rowSums(sweep(data[m,,drop=FALSE],2,ce[j,],"-")^2)) }
    if (w<bw) { bw <- w; best <- list(assignments=as, outlier_idx=oi, mask=mk) }
  }
  best
}
sil_score <- function(data, labels) {
  keep <- labels != 0
  if (length(unique(labels[keep])) < 2) return(NA)
  mean(silhouette(labels[keep], dist(data[keep,,drop=FALSE]))[,3])
}
tune_dbscan <- function(Xp) {
  best <- list(sil=-2, eps=NA, nc=NA, labels=NULL)
  for (e in c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1.0,1.2,1.5)) {
    lab <- dbscan(Xp, eps=e, minPts=5)$cluster
    nc <- length(unique(lab[lab!=0])); noise <- sum(lab==0)
    if (nc<2 || nc>10 || noise>0.5*length(lab)) next
    s <- sil_score(Xp, lab)
    if (!is.na(s) && s>best$sil) best <- list(sil=s, eps=e, nc=nc, labels=lab)
  }
  best
}

data(iris)
X <- scale(as.matrix(iris[,1:4])); y <- as.integer(iris$Species)
k <- 3; L <- 10; n <- nrow(X)

km <- kmeans(X, centers=k, nstart=25)
kmm <- kmeans_minus(X,k,L); det <- rep(FALSE,n); det[kmm$outlier_idx] <- TRUE
Xp <- prcomp(X)$x[,1:2]; bd <- tune_dbscan(Xp)
gmm <- Mclust(X, G=k, verbose=FALSE)

cat("============================================================\n")
cat("IRIS - four-method clustering (k =", k, ")\n")
cat("============================================================\n")
cat(sprintf("  ARI:  k-means=%.3f  k-means-=%.3f  DBSCAN=%.3f  GMM=%.3f\n",
            adjustedRandIndex(y,km$cluster),
            adjustedRandIndex(y[!det],kmm$assignments[!det]),
            adjustedRandIndex(y[bd$labels!=0], bd$labels[bd$labels!=0]),
            adjustedRandIndex(y,gmm$classification)))
cat(sprintf("  Sil:  k-means=%.3f  k-means-=%.3f  DBSCAN=%.3f  GMM=%.3f\n",
            sil_score(X,km$cluster), sil_score(X[!det,,drop=FALSE],kmm$assignments[!det]),
            bd$sil, sil_score(X,gmm$classification)))
cat("  DBSCAN PCA clusters:", bd$nc, "at eps", bd$eps, "| GMM model:", gmm$modelName, "\n")
cat("============================================================\n")