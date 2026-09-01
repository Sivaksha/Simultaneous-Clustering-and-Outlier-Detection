
library(ggplot2); library(gridExtra)
set.seed(42)

# ================================================================
# IRIS and WINE - clusters coloured, in PCA space
# ================================================================

## ---- IRIS (k=3 to match species) ----
data(iris)
Xi <- scale(iris[,1:4])
pci <- prcomp(Xi)
kmi <- kmeans(Xi, centers=3, nstart=15)
di  <- data.frame(PC1=pci$x[,1], PC2=pci$x[,2],
                  cluster=factor(kmi$cluster), species=iris$Species)

p_iris <- ggplot(di, aes(PC1, PC2, colour=cluster)) +
  geom_point(size=2, alpha=0.8) +
  scale_colour_manual(values=c("#2196F3","#FF9800","#4CAF50")) +
  labs(title="Iris: k-means clusters (k=3) in PCA space",
       x="First principal component", y="Second principal component",
       colour="Cluster") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold", size=10))

## ---- WINE (k=3) ----
# expects 'wine' data frame with class in column 1 (as you loaded it)
# if your wine object differs, adjust the next 3 lines to your columns
wine <- data(wine)              # <-- your wine file
Xw <- scale(wine[,-1])                      # drop the class label column
pcw <- prcomp(Xw)
kmw <- kmeans(Xw, centers=3, nstart=15)
dw  <- data.frame(PC1=pcw$x[,1], PC2=pcw$x[,2], cluster=factor(kmw$cluster))

## ---- WINE (k=3) ----
## ---- WINE (k=3) ----
url  <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data"
wine <- read.csv(url, header = FALSE)
Xw   <- scale(as.matrix(wine[, 2:14]))
pcw  <- prcomp(Xw)
kmw  <- kmeans(Xw, centers=3, nstart=15)
dw   <- data.frame(PC1=pcw$x[,1], PC2=pcw$x[,2], cluster=factor(kmw$cluster))

p_wine <- ggplot(dw, aes(PC1, PC2, colour=cluster)) +
  geom_point(size=2, alpha=0.8) +
  scale_colour_manual(values=c("#2196F3","#FF9800","#4CAF50")) +
  labs(title="Wine: k-means clusters (k=3) in PCA space",
       x="First principal component", y="Second principal component",
       colour="Cluster") +
  theme_minimal(base_size=10) +
  theme(plot.title=element_text(face="bold", size=10))

png("fig_pca_iriswine.png", width=3000, height=1300, res=300)
grid.arrange(p_iris, p_wine, ncol=2)
dev.off()
cat("saved fig_pca_iriswine.png\n")

# ================================================================
# HOUSE (Set 3: structural + price, k=2) with OUTLIERS marked
# ================================================================
house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

Xh  <- scale(as.matrix(house[, c(struct,"log_price")]))
pch <- prcomp(Xh)
kmh <- kmeans(Xh, centers=2, nstart=15)

# identify the 216 outliers (k-means- rule: furthest from nearest centroid)
d2c <- sqrt(apply(Xh, 1, function(r) min(colSums((t(kmh$centers)-r)^2))))
is_out <- rep(FALSE, nrow(house)); is_out[order(d2c, decreasing=TRUE)[1:216]] <- TRUE

dh <- data.frame(PC1=pch$x[,1], PC2=pch$x[,2],
                 cluster=factor(kmh$cluster), outlier=is_out)

# plot: clusters as colour, outliers as red crosses on top
png("fig_pca_house.png", width=2400, height=1700, res=300)
print(
  ggplot() +
    geom_point(data=dh[!dh$outlier,], aes(PC1, PC2, colour=cluster),
               size=0.6, alpha=0.4) +
    geom_point(data=dh[dh$outlier,], aes(PC1, PC2),
               colour="#E53935", shape=4, size=1.6, stroke=0.7) +
    scale_colour_manual(values=c("#2196F3","#4CAF50"), name="Cluster") +
    labs(title="House data (Set 3, k=2): clusters and the 216 considered outliers",
         subtitle="Blue and green: the two clusters. Red crosses: considered outliers.",
         x="First principal component", y="Second principal component") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold", size=10),
          plot.subtitle=element_text(size=8.5)) +
    guides(colour=guide_legend(override.aes=list(size=3, alpha=1)))
)
dev.off()
cat("saved fig_pca_house.png\n")
cat("\nPCA scatter plots done - 2 files saved.\n")
