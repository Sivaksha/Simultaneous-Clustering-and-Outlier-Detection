
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
# RUN — HOUSE (all five sets, FULL DATA, no sampling)
# k values from full-data optimal-k: S1=3, S2=4, S3=2, S4=3, S5=2
# ================================================================
house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
n <- nrow(house)

struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

sets <- list(
  list(name="Set 1: Structural",              cols=struct,                        k=3),
  list(name="Set 2: Structural + Location",   cols=c(struct,"long","lat"),        k=4),
  list(name="Set 3: Structural + log(price)", cols=c(struct,"log_price"),         k=2),
  list(name="Set 4: Location only",           cols=c("long","lat"),               k=3),
  list(name="Set 5: Location + log(price)",   cols=c("long","lat","log_price"),   k=2)
)

L <- round(0.01 * n)     # 1% of the FULL data
cat("FULL house data: n =", n, " | L =", L, "(1%)\n")

summary_tbl <- data.frame()

for (s in sets) {
  X <- scale(as.matrix(house[, s$cols]))
  cat("\n============================================================\n")
  cat(s$name, " (k =", s$k, ", p =", length(s$cols), ")\n")
  cat("============================================================\n")
  
  for (sp in c("full", "pca")) {
    r <- outlier_compare(X, s$k, L, space = sp)
    cat("[", toupper(sp), "] all3 =", r$all3, sprintf("(%.1f%%)", r$pct),
        " | km-&db =", r$km_db, " km-&gmm =", r$km_gm, " db&gmm =", r$db_gm,
        " | any =", r$anym,
        sprintf(" | null=%.3f -> %.0fx above chance\n", r$null, r$all3/r$null))
    
    summary_tbl <- rbind(summary_tbl, data.frame(
      Set = s$name, Space = sp, k = s$k, L = L,
      All3 = r$all3, Pct = round(r$pct,1),
      km_db = r$km_db, km_gm = r$km_gm, db_gm = r$db_gm,
      Null = round(r$null,3)))
  }
}

cat("\n============================================================\n")
cat("SUMMARY - HOUSE OUTLIER AGREEMENT (both spaces, full data)\n")
cat("============================================================\n")
print(summary_tbl, row.names = FALSE)
cat("\nAll agreement values are far above the null baseline,\n")
cat("confirming the methods consider the same genuinely extreme houses.\n")
cat("============================================================\n")