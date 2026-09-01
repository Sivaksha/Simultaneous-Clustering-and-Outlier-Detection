
library(cluster); library(clusterSim); library(fpc)
library(ggplot2); library(gridExtra)
set.seed(42)

url  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(url, header = FALSE)
X <- scale(as.matrix(wine[, 2:14]))
dataset_name <- "Wine"
k_range <- 2:10

sil_vals <- numeric(length(k_range)); db_vals <- numeric(length(k_range))
ch_vals  <- numeric(length(k_range)); wss_vals <- numeric(length(k_range))

for (i in seq_along(k_range)) {
  k  <- k_range[i]
  km <- kmeans(X, centers = k, nstart = 25)
  sil_vals[i] <- mean(silhouette(km$cluster, dist(X))[, 3])
  db_vals[i]  <- index.DB(X, km$cluster)$DB
  ch_vals[i]  <- calinhara(X, km$cluster, cn = k)
  wss_vals[i] <- km$tot.withinss
  cat(sprintf("k=%2d : Sil=%.3f  DB=%.3f  CH=%.0f  WCSS=%.1f\n",
              k, sil_vals[i], db_vals[i], ch_vals[i], wss_vals[i]))
}

best_sil <- k_range[which.max(sil_vals)]
best_db  <- k_range[which.min(db_vals)]
best_ch  <- k_range[which.max(ch_vals)]
cat("\n========================================\n")
cat(dataset_name, "- optimal number of clusters\n")
cat("========================================\n")
cat("Silhouette       (max):", best_sil, "\n")
cat("Davies-Bouldin   (min):", best_db, "\n")
cat("Calinski-Harabasz(max):", best_ch, "\n")
if (best_sil == best_db && best_db == best_ch)
  cat("=> ALL THREE metrics agree: optimal k =", best_sil, "\n") else
    cat("=> Metrics differ - report all three\n")
cat("Note: true number of cultivars = 3\n")
cat("========================================\n")

df <- data.frame(k = k_range, Silhouette = sil_vals, DaviesBouldin = db_vals,
                 CalinskiHarabasz = ch_vals, WCSS = wss_vals)
mk <- function(col, ylab, better, colour) {
  bk <- if (better=="max") df$k[which.max(df[[col]])] else df$k[which.min(df[[col]])]
  ggplot(df, aes(k, .data[[col]])) +
    geom_line(colour=colour, linewidth=1) + geom_point(colour=colour, size=3) +
    geom_point(data=df[df$k==bk,], colour="red", size=5) +
    scale_x_continuous(breaks=k_range) +
    labs(title=paste0(ylab," (",ifelse(better=="max","higher","lower")," = better)"),
         subtitle=paste("best k =",bk), x="k", y=ylab) +
    theme_minimal(base_size=10)
}
grid.arrange(mk("Silhouette","Silhouette","max","#2196F3"),
             mk("DaviesBouldin","Davies-Bouldin","min","#FF9800"),
             mk("CalinskiHarabasz","Calinski-Harabasz","max","#4CAF50"),
             mk("WCSS","Within-cluster SS","min","#9C27B0"),
             ncol=2, top=paste0(dataset_name,": optimal k (three metrics + elbow)"))