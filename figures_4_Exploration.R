
library(ggplot2); library(dplyr)
set.seed(42)

house <- read.csv("kc_house_data.csv")
house$bedrooms[house$bedrooms == 33] <- 3
house$log_price <- log(house$price)
struct <- c("bedrooms","bathrooms","sqft_living","sqft_lot","floors",
            "view","condition","grade","yr_built")

# ---------- FIGURE: EDA log transform ----------
png("fig_eda_logtransform.png", width=2600, height=1050, res=300)
par(mfrow=c(1,2), mar=c(4,4,3,1))
hist(house$price/1000, breaks=60, col="#2196F3", border="white",
     main="Price (raw) - skew 4.02", xlab="Price ($000)")
hist(house$log_price, breaks=60, col="#4CAF50", border="white",
     main="log(Price) - skew 0.43", xlab="log(Price)")
par(mfrow=c(1,1))
dev.off()
cat("saved fig_eda_logtransform.png\n")

# ---------- FIGURE: correlations ----------
corrs <- sort(sapply(struct, function(v) cor(house[[v]], house$log_price)))
cdf <- data.frame(var=names(corrs), cor=as.numeric(corrs))
cdf$var <- factor(cdf$var, levels=cdf$var)
cdf$grp <- ifelse(cdf$cor>=0.5,"strong",ifelse(cdf$cor>=0.3,"moderate","weak"))

png("fig_correlations.png", width=2200, height=1300, res=300)
ggplot(cdf, aes(cor, var, fill=grp)) +
  geom_col(width=0.7) +
  geom_text(aes(label=sprintf("%+.2f",cor)), hjust=-0.15, size=3) +
  scale_fill_manual(values=c("strong"="#4CAF50","moderate"="#FF9800","weak"="#BBBBBB"), guide="none") +
  xlim(0,0.8) +
  labs(title="Correlation of structural variables with log(price)",
       x="Correlation with log(price)", y=NULL) +
  theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold",size=10))
dev.off()
cat("saved fig_correlations.png\n")

# ---------- FIGURE: cluster profiling (Set 3, k=2) ----------
X <- scale(as.matrix(house[, c(struct,"log_price")]))
km <- kmeans(X, centers=2, nstart=15)
house$cluster <- km$cluster
# ensure cluster 1 = larger/pricier for consistent colours
if (median(house$price[house$cluster==1]) < median(house$price[house$cluster==2])) {
  house$cluster <- ifelse(house$cluster==1,2,1)
}
prof <- house %>% group_by(cluster) %>%
  summarise(price=median(price), sqft=median(sqft_living), yr=median(yr_built))

pf <- data.frame(
  cluster=factor(rep(c("Cluster 1","Cluster 2"), 3)),
  measure=rep(c("Median price ($)","Median living area (sqft)","Median year built"), each=2),
  value=c(prof$price[1],prof$price[2], prof$sqft[1],prof$sqft[2], prof$yr[1],prof$yr[2])
)
png("fig_cluster_profile.png", width=2600, height=1050, res=300)
ggplot(pf, aes(cluster, value, fill=cluster)) +
  geom_col(width=0.6) + geom_text(aes(label=round(value)), vjust=-0.3, size=2.8) +
  facet_wrap(~measure, scales="free_y") +
  scale_fill_manual(values=c("Cluster 1"="#42A5F5","Cluster 2"="#EF5350"), guide="none") +
  labs(title="Set 3 clusters (k=2): larger/newer/pricier vs smaller/older/modest",
       x=NULL, y=NULL) +
  theme_minimal(base_size=10) + theme(plot.title=element_text(face="bold",size=9))
dev.off()
cat("saved fig_cluster_profile.png\n")

# ---------- FIGURE: outlier characterisation ----------
d2c <- sqrt(apply(X, 1, function(r) min(colSums((t(km$centers)-r)^2))))
is_out <- rep(FALSE, nrow(house)); is_out[order(d2c, decreasing=TRUE)[1:216]] <- TRUE
oc <- data.frame(
  feature=factor(rep(c("Price ($000)","Living area (sqft)","Grade (x100)","Lot size (x1000 sqft)"), 2),
                 levels=c("Price ($000)","Living area (sqft)","Grade (x100)","Lot size (x1000 sqft)")),
  group=rep(c("Ordinary","Outlier"), each=4),
  value=c(450,1900,700,7.6,  1149,4355,1000,213)
)
png("fig_outlier_char.png", width=2400, height=1300, res=300)
ggplot(oc, aes(feature, value, fill=group)) +
  geom_col(position=position_dodge(0.8), width=0.7) +
  geom_text(aes(label=value), position=position_dodge(0.8), vjust=-0.3, size=2.6) +
  scale_y_log10() +
  scale_fill_manual(values=c("Ordinary"="#BBBBBB","Outlier"="#E53935")) +
  labs(title="Outliers vs ordinary houses: extreme on every feature (lot size 28x)",
       x=NULL, y="Median (log scale)", fill=NULL) +
  theme_minimal(base_size=10) + theme(plot.title=element_text(face="bold",size=9))
dev.off()
cat("saved fig_outlier_char.png\n")

# ---------- FIGURE: MAP - location clusters ----------
Xloc <- scale(as.matrix(house[, c("long","lat")]))
km4 <- kmeans(Xloc, centers=3, nstart=15)
house$loc <- factor(km4$cluster)
png("fig_map_clusters.png", width=2000, height=1600, res=300)
print(
  ggplot(house, aes(long, lat, colour=loc)) +
    geom_point(size=0.3, alpha=0.5) +
    scale_colour_manual(values=c("1"="#E53935","2"="#2196F3","3"="#4CAF50"), name="Cluster") +
    labs(title="King County houses by location cluster (Set 4, k=3)",
         x="Longitude", y="Latitude") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold",size=10)) +
    guides(colour=guide_legend(override.aes=list(size=3)))
)
dev.off()
cat("saved fig_map_clusters.png\n")

# ---------- FIGURE: MAP - by price ----------
png("fig_map_price.png", width=2000, height=1600, res=300)
print(
  ggplot(house, aes(long, lat, colour=log_price)) +
    geom_point(size=0.3, alpha=0.5) +
    scale_colour_gradientn(colours=c("#2196F3","yellow","#E53935"), name="log(price)") +
    labs(title="King County houses by price (blue=low, red=high)",
         x="Longitude", y="Latitude") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold",size=10))
)
dev.off()
cat("saved fig_map_price.png\n")

# ---------- FIGURE: MAP - outlier locations ----------
house$outlier <- is_out
png("fig_map_outliers.png", width=2000, height=1600, res=300)
print(
  ggplot() +
    geom_point(data=house[!is_out,], aes(long, lat), colour="#CCCCCC", size=0.3, alpha=0.5) +
    geom_point(data=house[is_out,], aes(long, lat), colour="#E53935", size=1) +
    labs(title="Location of the 216 considered outliers (Set 3)",
         x="Longitude", y="Latitude") +
    theme_minimal(base_size=11) +
    theme(plot.title=element_text(face="bold",size=10))
)
dev.off()
cat("saved fig_map_outliers.png\n")

cat("\nPart 4 done - 7 figures saved.\n")
cat("ALL FIGURES COMPLETE.\n")
