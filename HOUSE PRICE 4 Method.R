
# install.packages(c("cluster","dbscan","mclust"))
library(cluster)
library(dbscan)
library(mclust)
set.seed(42)

# --- k-means- (Chawla & Gionis 2013) ---
kmeans_minus <- function(data, k, l, max_iter = 60, n_restarts = 4) {
  n <- nrow(data); best <- NULL; best_wcss <- Inf
  for (r in seq_len(n_restarts)) {
    centroids <- data[sample(n, k), , drop = FALSE]
    assignments <- rep(1, n); mask <- rep(TRUE, n)
    for (iter in seq_len(max_iter)) {
      dm <- matrix(0, n, k)
      for (j in seq_len(k)) dm[, j] <- rowSums(sweep(data, 2, centroids[j, ], "-")^2)
      min_d <- apply(dm, 1, min); assignments <- apply(dm, 1, which.min)
      oi <- order(min_d, decreasing = TRUE)[seq_len(l)]
      mask <- rep(TRUE, n); mask[oi] <- FALSE
      nc <- centroids
      for (j in seq_len(k)) {
        mem <- which(assignments == j & mask)
        if (length(mem) > 0) nc[j, ] <- colMeans(data[mem, , drop = FALSE])
      }
      if (max(abs(nc - centroids)) < 1e-6) break
      centroids <- nc
    }
    wcss <- 0
    for (j in seq_len(k)) {
      mem <- which(assignments == j & mask)
      wcss <- wcss + sum(rowSums(sweep(data[mem, , drop=FALSE], 2, centroids[j, ], "-")^2))
    }
    if (wcss < best_wcss) { best_wcss <- wcss
    best <- list(assignments = assignments, outlier_idx = oi, mask = mask) }
  }
  best
}

sil_score <- function(data, labels) {
  keep <- labels != 0
  if (length(unique(labels[keep])) < 2) return(NA)
  mean(silhouette(labels[keep], dist(data[keep, , drop = FALSE]))[, 3])
}

tune_dbscan <- function(Xp) {
  best <- list(sil = -2, eps = NA, nc = NA)
  for (e in c(0.15,0.2,0.25,0.3,0.4,0.5,0.6,0.8,1.0,1.2)) {
    lab <- dbscan(Xp, eps = e, minPts = 10)$cluster
    nc <- length(unique(lab[lab != 0])); noise <- sum(lab == 0)
    if (nc < 2 || nc > 10 || noise > 0.5*length(lab)) next
    s <- sil_score(Xp, lab)
    if (!is.na(s) && s > best$sil) best <- list(sil = s, eps = e, nc = nc)
  }
  best
}

# ---- Load full data ----
house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
n <- nrow(house)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

sets <- list(
  list(name="Set 1: Structural",              cols=struct,                      k=3),
  list(name="Set 2: Structural + Location",   cols=c(struct,"long","lat"),      k=4),
  list(name="Set 3: Structural + log(price)", cols=c(struct,"log_price"),       k=2),
  list(name="Set 4: Location only",           cols=c("long","lat"),             k=3),
  list(name="Set 5: Location + log(price)",   cols=c("long","lat","log_price"), k=2)
)

L <- round(0.01 * n)
cat("FULL house data: n =", n, " | k-means- L =", L, "\n\n")

results <- data.frame()

for (s in sets) {
  cat("============================================================\n")
  cat(s$name, " (k =", s$k, ", p =", length(s$cols), ")\n")
  cat("============================================================\n")
  X <- scale(as.matrix(house[, s$cols])); k <- s$k
  
  # 1. k-means
  cat("  running k-means...\n")
  km <- kmeans(X, centers = k, nstart = 15)
  s_km <- sil_score(X, km$cluster)
  
  # 2. k-means-
  cat("  running k-means-...\n")
  kmm <- kmeans_minus(X, k = k, l = L)
  det <- rep(FALSE, n); det[kmm$outlier_idx] <- TRUE
  s_kmm <- sil_score(X[!det, , drop = FALSE], kmm$assignments[!det])
  
  # 3. DBSCAN raw + PCA
  cat("  running DBSCAN...\n")
  db_raw <- dbscan(X, eps = 1.5, minPts = 10)
  n_raw <- length(unique(db_raw$cluster[db_raw$cluster != 0]))
  npc <- min(3, ncol(X)); Xp <- prcomp(X)$x[, 1:npc, drop = FALSE]
  bd <- tune_dbscan(Xp)
  
  # 4. GMM
  cat("  running GMM...\n")
  gmm <- Mclust(X, G = k, verbose = FALSE)
  s_gm <- sil_score(X, gmm$classification)
  
  cat(sprintf("  RESULT: k-means=%.3f  k-means-=%.3f  DBSCAN=%.3f  GMM=%.3f\n",
              s_km, s_kmm, bd$sil, s_gm))
  cat(sprintf("  DBSCAN raw=%d clusters -> PCA=%d clusters (eps=%.2f) | GMM model=%s\n\n",
              n_raw, bd$nc, bd$eps, gmm$modelName))
  
  results <- rbind(results, data.frame(
    Set = s$name, k = k, p = ncol(X),
    kmeans = round(s_km,3), kmeans_minus = round(s_kmm,3),
    DBSCAN = round(bd$sil,3), GMM = round(s_gm,3),
    DB_raw = n_raw, DB_pca = bd$nc, GMM_model = gmm$modelName))
}

cat("============================================================\n")
cat("SUMMARY - FOUR METHODS, ALL 5 HOUSE SETS (full data)\n")
cat("============================================================\n")
print(results[, c("Set","k","p","kmeans","kmeans_minus","DBSCAN","GMM")], row.names = FALSE)
cat("\n--- DBSCAN & GMM detail ---\n")
print(results[, c("Set","DB_raw","DB_pca","GMM_model")], row.names = FALSE)
cat("\nSilhouette only - house has no ground-truth labels.\n")
cat("============================================================\n")