
set.seed(42)

house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

# ---- 1. Price distribution and the log transform ----
cat("============================================================\n")
cat("1. PRICE DISTRIBUTION AND THE LOG TRANSFORM\n")
cat("============================================================\n")
cat(sprintf("Price:  min $%s   median $%s   max $%s\n",
    format(min(house$price),big.mark=","),
    format(median(house$price),big.mark=","),
    format(max(house$price),big.mark=",")))
sk  <- function(x){m<-mean(x);s<-sd(x);mean(((x-m)/s)^3)}
cat(sprintf("Skewness of price:      %.2f  (strongly right-skewed)\n", sk(house$price)))
cat(sprintf("Skewness of log(price): %.2f  (approximately symmetric)\n", sk(house$log_price)))
cat("=> the log transform is justified: it removes most of the skew.\n\n")

# histograms
par(mfrow=c(1,2))
hist(house$price/1000, breaks=60, col="steelblue", main="Price (raw)",
     xlab="Price ($000)")
hist(house$log_price, breaks=60, col="darkgreen", main="log(Price)",
     xlab="log(Price)")
par(mfrow=c(1,1))

# ---- 2. Correlation with price ----
cat("============================================================\n")
cat("2. CORRELATION OF FEATURES WITH log(PRICE)\n")
cat("============================================================\n")
corrs <- sort(sapply(struct, function(v) cor(house[[v]], house$log_price)),
              decreasing = TRUE)
for (v in names(corrs)) cat(sprintf("  %-14s %+.2f\n", v, corrs[v]))
cat("\n=> grade and living area are the strongest predictors of price.\n\n")

# correlation heatmap (base R)
cm <- cor(house[, c(struct,"log_price")])
cat("Full correlation matrix computed (see heatmap plot).\n")
image(1:ncol(cm), 1:nrow(cm), t(cm)[, nrow(cm):1], axes=FALSE,
      col=colorRampPalette(c("white","steelblue"))(20),
      main="Correlation heatmap", xlab="", ylab="")
axis(1, 1:ncol(cm), colnames(cm), las=2, cex.axis=0.7)
axis(2, 1:nrow(cm), rev(colnames(cm)), las=2, cex.axis=0.7)

# ---- 3. Summary statistics table ----
cat("\n============================================================\n")
cat("3. SUMMARY STATISTICS\n")
cat("============================================================\n")
summ <- data.frame(
  variable = c(struct,"log_price"),
  mean = round(sapply(c(struct,"log_price"), function(v) mean(house[[v]])),2),
  median = round(sapply(c(struct,"log_price"), function(v) median(house[[v]])),2),
  sd = round(sapply(c(struct,"log_price"), function(v) sd(house[[v]])),2)
)
print(summ, row.names = FALSE)
cat("\nDone.\n")
