
library(ggplot2)

datasets <- c("Iris","Wine","House S1","House S2","House S3","House S4","House S5")

# ---------- FIGURE: agreement both spaces ----------
agree <- data.frame(
  dataset = rep(datasets, 2),
  space = rep(c("Full space","PCA space"), each=7),
  pct = c(40,75,18.5,16.2,41.7,21.3,27.8,   50,62.5,71.3,60.2,44.4,21.3,22.7)
)
agree$dataset <- factor(agree$dataset, levels=datasets)

png("fig_outlier_agree.png", width=2700, height=1400, res=300)
ggplot(agree, aes(dataset, pct, fill=space)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_text(aes(label=sprintf("%.0f%%",pct)), position=position_dodge(0.8),
            vjust=-0.3, size=2.7) +
  geom_hline(yintercept=0.44, linetype="dashed", colour="red") +
  annotate("text", x=5.5, y=3, label="null baseline ~0.4%", colour="red", size=3) +
  scale_fill_manual(values=c("Full space"="#2196F3","PCA space"="#00BCD4")) +
  labs(title="Outlier agreement in both spaces - all values vastly exceed the null baseline",
       x=NULL, y="Considered by all three (%)", fill="Space") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=10))
dev.off()
cat("saved fig_outlier_agree.png\n")

# ---------- FIGURE: null baseline (log scale) ----------
nullb <- data.frame(
  dataset = rep(datasets, 2),
  type = rep(c("Observed agreement","Expected if random"), each=7),
  pct = c(40,75,18.5,16.2,41.7,21.3,27.8,   0.44,0.20,0.01,0.01,0.01,0.01,0.01)
)
nullb$dataset <- factor(nullb$dataset, levels=datasets)

png("fig_nullbaseline.png", width=2500, height=1400, res=300)
ggplot(nullb, aes(dataset, pct, fill=type)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  scale_y_log10() +
  scale_fill_manual(values=c("Observed agreement"="#4CAF50","Expected if random"="#E53935")) +
  labs(title="Observed agreement is 90-4000x above chance - the overlap is real",
       x=NULL, y="Agreement (%, log scale)", fill=NULL) +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=10))
dev.off()
cat("saved fig_nullbaseline.png\n")

# ---------- FIGURE: pairwise agreement (house, full space) ----------
pairw <- data.frame(
  set = rep(c("Set 1","Set 2","Set 3","Set 4","Set 5"), 3),
  pair = rep(c("k-means- & DBSCAN","k-means- & GMM","DBSCAN & GMM"), each=5),
  count = c(149,150,156,61,63,  42,40,99,126,109,  56,52,108,49,104)
)
pairw$pair <- factor(pairw$pair, levels=c("k-means- & DBSCAN","k-means- & GMM","DBSCAN & GMM"))

png("fig_pairwise.png", width=2500, height=1350, res=300)
ggplot(pairw, aes(set, count, fill=pair)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_text(aes(label=count), position=position_dodge(0.8), vjust=-0.3, size=2.7) +
  scale_fill_manual(values=c("k-means- & DBSCAN"="#2196F3","k-means- & GMM"="#4CAF50",
                             "DBSCAN & GMM"="#FF9800")) +
  labs(title="Pairwise outlier agreement (house, full space): different pairs dominate by set",
       x=NULL, y="Points considered by both (of 216)", fill="Method pair") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=10))
dev.off()
cat("saved fig_pairwise.png\n")
cat("\nPart 2 done - 3 figures saved.\n")
