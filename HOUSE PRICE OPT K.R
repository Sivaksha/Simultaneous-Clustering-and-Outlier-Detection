
library(cluster)      # silhouette()
library(clusterSim)   # index.DB()  -> Davies-Bouldin
library(fpc)          # calinhara() -> Calinski-Harabasz
library(ggplot2)
library(gridExtra)

set.seed(42)


# ================================================================
# LOAD + PREPARE  (FULL DATA - no sampling)
# ================================================================
house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3      # typo fix per supervisor
house$log_price <- log(house$price)
n <- nrow(house)
cat("FULL dataset:", n, "houses (no sampling)\n\n")

struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

sets <- list(
  "Set 1: Structural"              = struct,
  "Set 2: Structural + Location"   = c(struct, "long", "lat"),
  "Set 3: Structural + log(price)" = c(struct, "log_price"),
  "Set 4: Location only"           = c("long", "lat"),
  "Set 5: Location + log(price)"   = c("long", "lat", "log_price")
)

# The k grid specified by the supervisor
k_grid <- c(2:50, seq(55, 100, by = 5), seq(125, 300, by = 25))
cat("k grid:", length(k_grid), "values, from", min(k_grid), "to", max(k_grid), "\n")
cat(paste(k_grid, collapse = " "), "\n\n")


# ================================================================
# MAIN LOOP OVER THE FIVE SETS
# ================================================================
all_results <- list()

for (set_name in names(sets)) {
  cat("============================================================\n")
  cat(set_name, "\n")
  cat("============================================================\n")

  X <- scale(as.matrix(house[, sets[[set_name]]]))

  sil_vals <- numeric(length(k_grid))
  db_vals  <- numeric(length(k_grid))
  ch_vals  <- numeric(length(k_grid))
  wss_vals <- numeric(length(k_grid))

  t_start <- Sys.time()

  for (i in seq_along(k_grid)) {
    k  <- k_grid[i]
    km <- kmeans(X, centers = k, nstart = 5, iter.max = 50)

    # Silhouette - the expensive metric (full pairwise distances)
    ss <- silhouette(km$cluster, dist(X))
    sil_vals[i] <- mean(ss[, 3])

    # Davies-Bouldin (fast)
    db_vals[i] <- index.DB(X, km$cluster)$DB

    # Calinski-Harabasz (fast)
    ch_vals[i] <- calinhara(X, km$cluster, cn = k)

    # Within-cluster sum of squares (instant)
    wss_vals[i] <- km$tot.withinss

    # progress every 10 values
    if (i %% 10 == 0 || i == length(k_grid))
      cat(sprintf("  ...k=%d done (%d/%d)\n", k, i, length(k_grid)))
  }

  elapsed <- round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1)
  cat("  [", set_name, "took", elapsed, "minutes ]\n")

  best_sil <- k_grid[which.max(sil_vals)]
  best_db  <- k_grid[which.min(db_vals)]
  best_ch  <- k_grid[which.max(ch_vals)]

  cat("  Optimal k by metric:\n")
  cat("    Silhouette      (max):", best_sil, "\n")
  cat("    Davies-Bouldin  (min):", best_db, "\n")
  cat("    Calinski-Harabasz(max):", best_ch, "\n")
  cat("    (WCSS always decreases - elbow inspected visually)\n\n")

  all_results[[set_name]] <- data.frame(
    k = k_grid, Silhouette = sil_vals, DaviesBouldin = db_vals,
    CalinskiHarabasz = ch_vals, WCSS = wss_vals)
}


# ================================================================
# SUMMARY TABLE
# ================================================================
cat("============================================================\n")
cat("SUMMARY - OPTIMAL k BY METRIC (full data, grid to 300)\n")
cat("============================================================\n")
summ <- data.frame()
for (set_name in names(all_results)) {
  r <- all_results[[set_name]]
  summ <- rbind(summ, data.frame(
    Set = set_name,
    Silhouette_k = r$k[which.max(r$Silhouette)],
    DaviesBouldin_k = r$k[which.min(r$DaviesBouldin)],
    CalinskiHarabasz_k = r$k[which.max(r$CalinskiHarabasz)]))
}
print(summ, row.names = FALSE)
cat("\nIf Davies-Bouldin_k = 300 (the grid maximum), the index never\n")
cat("found a genuine minimum - strong evidence it drifts on this data.\n")
cat("============================================================\n")


# ================================================================
# PLOTS - all four metrics per set
# ================================================================
for (set_name in names(all_results)) {
  df <- all_results[[set_name]]

  mk <- function(col, ylab, better, colour) {
    best_k <- if (better == "max") df$k[which.max(df[[col]])] else df$k[which.min(df[[col]])]
    ggplot(df, aes(k, .data[[col]])) +
      geom_line(colour = colour, linewidth = 0.7) +
      geom_point(colour = colour, size = 1) +
      geom_vline(xintercept = best_k, linetype = "dashed", colour = "red") +
      labs(title = paste0(ylab, " (", ifelse(better=="max","higher","lower"), " = better)"),
           subtitle = paste("best k =", best_k),
           x = "k", y = ylab) +
      theme_minimal(base_size = 9)
  }

  p1 <- mk("Silhouette", "Silhouette", "max", "#2196F3")
  p2 <- mk("DaviesBouldin", "Davies-Bouldin", "min", "#FF9800")
  p3 <- mk("CalinskiHarabasz", "Calinski-Harabasz", "max", "#4CAF50")
  p4 <- mk("WCSS", "Within-cluster SS", "min", "#9C27B0")

  print(grid.arrange(p1, p2, p3, p4, ncol = 2, top = set_name))
}

cat("\nDone - full-data optimal-k analysis complete for all five sets.\n")
