# ================================================================
# MATH5872M — Dissertation | Supervisor: Dr. Arief Gusnanto
# ENHANCEMENT 1: CLUSTER PROFILING
# What do the clusters actually represent? (turns silhouette scores
# into concrete, interpretable findings)
# Full data, no sampling. Bedroom typo 33->3 fixed.
# ================================================================
library(dplyr)
set.seed(42)

house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

# Profile clusters for Set 3 (structural + log price, k=2 from optimal-k)
X <- scale(as.matrix(house[, c(struct,"log_price")]))
km <- kmeans(X, centers = 2, nstart = 15)
house$cluster <- km$cluster

cat("============================================================\n")
cat("CLUSTER PROFILING - Set 3 (structural + log price, k=2)\n")
cat("============================================================\n")
profile <- house %>%
  group_by(cluster) %>%
  summarise(
    n           = n(),
    pct         = round(100*n()/nrow(house)),
    med_price   = median(price),
    med_sqft    = median(sqft_living),
    med_bed     = median(bedrooms),
    med_bath    = median(bathrooms),
    med_grade   = median(grade),
    med_yrbuilt = median(yr_built)
  )
print(as.data.frame(profile))

cat("\nInterpretation: the two clusters separate the housing stock into\n")
cat("distinct types (e.g. smaller/older vs larger/newer), showing the\n")
cat("clustering captures a meaningful structural distinction.\n")

# Also profile Set 1 (structural only, k=3) for comparison
cat("\n============================================================\n")
cat("CLUSTER PROFILING - Set 1 (structural only, k=3)\n")
cat("============================================================\n")
X1 <- scale(as.matrix(house[, struct]))
km1 <- kmeans(X1, centers = 3, nstart = 15)
house$cluster1 <- km1$cluster
profile1 <- house %>%
  group_by(cluster1) %>%
  summarise(
    n=n(), pct=round(100*n()/nrow(house)),
    med_price=median(price), med_sqft=median(sqft_living),
    med_bed=median(bedrooms), med_grade=median(grade),
    med_yrbuilt=median(yr_built)
  )
print(as.data.frame(profile1))
cat("\nDone.\n")
