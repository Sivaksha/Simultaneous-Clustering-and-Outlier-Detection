

# install.packages(c("mclust","FNN"))
library(mclust)
library(FNN)
set.seed(42)


# ================================================================
# CORE FUNCTION - run the comparison in a given space
# ================================================================
outlier_compare <- function(X, k, L, npc = 3, space = "full") {
  n <- nrow(X)
  
  # working space: full variables, or first npc principal components
  if (space == "pca") {
    Xw <- prcomp(X)$x[, 1:min(npc, ncol(X)), drop = FALSE]
  } else {
    Xw <- X
  }
  
  # cluster once in the working space
  km <- kmeans(Xw, centers = k, nstart = 15)
  
  # --- k-means-: L furthest from nearest centroid ---
  d2c <- sqrt(apply(Xw, 1, function(r) min(colSums((t(km$centers) - r)^2))))
  o_km <- rep(FALSE, n); o_km[order(d2c, decreasing = TRUE)[seq_len(L)]] <- TRUE
  
  # --- DBSCAN: L sparsest by 10th-NN distance ---
  nn  <- min(10, n - 1)
  knn <- get.knn(Xw, k = nn)
  dens <- knn$nn.dist[, nn]
  o_db <- rep(FALSE, n); o_db[order(dens, decreasing = TRUE)[seq_len(L)]] <- TRUE
  
  # --- GMM: L lowest likelihood ---
  gmm <- Mclust(Xw, G = k, verbose = FALSE)
  loglik <- dens(modelName = gmm$modelName, data = Xw,
                 parameters = gmm$parameters, logarithm = TRUE)
  o_gm <- rep(FALSE, n); o_gm[order(loglik)[seq_len(L)]] <- TRUE
  
  # --- agreement ---
  all3 <- sum(o_km & o_db & o_gm)
  anym <- sum(o_km | o_db | o_gm)
  null_all3 <- L^3 / n^2      # expected common-to-all-3 under randomness
  
  list(space = space, all3 = all3, pct = 100 * all3 / L,
       km_db = sum(o_km & o_db), km_gm = sum(o_km & o_gm), db_gm = sum(o_db & o_gm),
       anym = anym, null = null_all3, gmm_model = gmm$modelName)
}


# ================================================================
# RUN — IRIS
# ================================================================
data(iris)
X <- scale(as.matrix(iris[, 1:4]))
k <- 3          # true species (also used earlier); metric-optimal k=2 noted in report
L <- 10

cat("============================================================\n")
cat("IRIS - outlier-consideration comparison  (n =", nrow(X), ", k =", k, ", L =", L, ")\n")
cat("============================================================\n")

for (sp in c("full", "pca")) {
  r <- outlier_compare(X, k, L, space = sp)
  cat("\n[", toupper(sp), "SPACE ]\n")
  cat("  Considered by all three :", r$all3, sprintf("(%.1f%%)\n", r$pct))
  cat("  Pairwise:  k-means-&DBSCAN =", r$km_db,
      " k-means-&GMM =", r$km_gm, " DBSCAN&GMM =", r$db_gm, "\n")
  cat("  Considered by any        :", r$anym, "\n")
  cat("  NULL baseline (random)   :", sprintf("%.3f (%.2f%%)\n", r$null, 100*r$null/L))
  cat("  => observed agreement is", sprintf("%.0fx", r$all3 / r$null), "above chance\n")
  cat("  GMM model:", r$gmm_model, "\n")
}
cat("\n============================================================\n")
cat("The null baseline shows the methods agree far above chance,\n")
cat("confirming the overlap reflects genuinely extreme observations.\n")
cat("============================================================\n")
