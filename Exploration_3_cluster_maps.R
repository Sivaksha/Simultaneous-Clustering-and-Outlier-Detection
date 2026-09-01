# ================================================================
# MATH5872M — Dissertation | Supervisor: Dr. Arief Gusnanto
# ENHANCEMENT 3: CLUSTER MAPS (addresses the map request for Fig 2)
# Plots the King County houses geographically, coloured by cluster,
# for the location-based variable sets. Makes the spatial structure
# visible and interpretable.
# ================================================================
set.seed(42)

house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)

# ---- Map 1: Location-only clusters (Set 4, k=3) ----
Xloc <- scale(as.matrix(house[, c("long","lat")]))
km4  <- kmeans(Xloc, centers = 3, nstart = 15)
house$loc_cluster <- factor(km4$cluster)

cat("Location clusters (Set 4, k=3):\n")
for (c in levels(house$loc_cluster)) {
  sub <- house[house$loc_cluster==c, ]
  cat(sprintf("  Cluster %s: %d houses, centre (%.2f, %.2f), median $%s\n",
      c, nrow(sub), mean(sub$lat), mean(sub$long),
      format(median(sub$price), big.mark=",")))
}

# plot coloured by cluster -- this is the MAP for the report
cols <- c("#E53935","#2196F3","#4CAF50")
plot(house$long, house$lat, col = cols[km4$cluster], pch = 19, cex = 0.3,
     xlab = "Longitude", ylab = "Latitude",
     main = "King County houses by location cluster (Set 4, k=3)")
legend("topright", legend = paste("Cluster", 1:3), col = cols, pch = 19, cex = 0.9)

# ---- Map 2: coloured by price (to show the geography of price) ----
price_col <- colorRampPalette(c("#2196F3","yellow","#E53935"))(10)[
  as.numeric(cut(house$log_price, 10))]
plot(house$long, house$lat, col = price_col, pch = 19, cex = 0.3,
     xlab = "Longitude", ylab = "Latitude",
     main = "King County houses by price (blue=low, red=high)")

# ---- Map 3: where the OUTLIERS are (Set 3 k-means-, L=216) ----
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")
X3 <- scale(as.matrix(house[, c(struct,"log_price")]))
km3 <- kmeans(X3, centers = 2, nstart = 15)
d2c <- sqrt(apply(X3, 1, function(r) min(colSums((t(km3$centers)-r)^2))))
is_out <- rep(FALSE, nrow(house)); is_out[order(d2c, decreasing=TRUE)[1:216]] <- TRUE

plot(house$long, house$lat, col = "grey80", pch = 19, cex = 0.3,
     xlab = "Longitude", ylab = "Latitude",
     main = "Location of the 216 considered outliers (Set 3)")
points(house$long[is_out], house$lat[is_out], col = "#E53935", pch = 19, cex = 0.6)
legend("topright", legend = c("Ordinary","Outlier"),
       col = c("grey80","#E53935"), pch = 19)

cat("\nThree maps produced: by cluster, by price, and outlier locations.\n")
cat("Done.\n")
