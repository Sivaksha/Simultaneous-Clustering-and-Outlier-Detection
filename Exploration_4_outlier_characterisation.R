# ================================================================
# MATH5872M — Dissertation | Supervisor: Dr. Arief Gusnanto
# ENHANCEMENT 4: OUTLIER CHARACTERISATION
# Not just HOW MANY outliers, but WHAT they are and WHY unusual.
# Also addresses #F1 (number removed) and #F2 (definition) with
# concrete evidence for the house data.
# ================================================================
library(dplyr)
set.seed(42)

house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

# Set 3 (structural + log price, k=2), k-means- with L = 1% = 216
X <- scale(as.matrix(house[, c(struct,"log_price")]))
km <- kmeans(X, centers = 2, nstart = 15)
d2c <- sqrt(apply(X, 1, function(r) min(colSums((t(km$centers)-r)^2))))
L <- round(0.01 * nrow(house))
is_out <- rep(FALSE, nrow(house)); is_out[order(d2c, decreasing=TRUE)[1:L]] <- TRUE
house$outlier <- is_out

cat("============================================================\n")
cat("OUTLIER CHARACTERISATION - Set 3, k-means-, L =", L, "(1%)\n")
cat("============================================================\n")

comp <- house %>%
  group_by(outlier) %>%
  summarise(
    n=n(),
    med_price=median(price), med_sqft=median(sqft_living),
    med_bed=median(bedrooms), med_bath=median(bathrooms),
    med_grade=median(grade), med_lot=median(sqft_lot)
  )
cat("\nConsidered outliers vs the rest (medians):\n")
print(as.data.frame(comp))

cat(sprintf("\nMost extreme considered outliers:\n"))
cat(sprintf("  highest price: $%s\n", format(max(house$price[is_out]),big.mark=",")))
cat(sprintf("  largest sqft:  %s\n", format(max(house$sqft_living[is_out]),big.mark=",")))
cat(sprintf("  most bedrooms: %d\n", max(house$bedrooms[is_out])))

cat("\nInterpretation: the considered outliers are the large, expensive,\n")
cat("high-grade properties - unusual on several features at once, which\n")
cat("is why all three methods tend to agree on them (see main results).\n")

# How many outliers overlap between the 2 clusters' membership
cat(sprintf("\nOf the %d outliers: %d were assigned to cluster 1, %d to cluster 2\n",
    L, sum(km$cluster[is_out]==1), sum(km$cluster[is_out]==2)))
cat("Done.\n")
