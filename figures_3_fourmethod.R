
library(ggplot2)
library(gridExtra)

methods <- c("k-means","k-means-","DBSCAN","GMM")

# ---------- FIGURE: iris & wine (ARI + silhouette) ----------
iris_df <- data.frame(
  method = rep(methods, 2),
  metric = rep(c("ARI","Silhouette"), each=4),
  value = c(0.620,0.614,0.588,0.904,  0.460,0.473,0.670,0.374)
)
iris_df$method <- factor(iris_df$method, levels=methods)
wine_df <- data.frame(
  method = rep(methods, 2),
  metric = rep(c("ARI","Silhouette"), each=4),
  value = c(0.897,0.949,0.950,0.930,  0.285,0.305,0.610,0.273)
)
wine_df$method <- factor(wine_df$method, levels=methods)

p_iris <- ggplot(iris_df, aes(method, value, fill=metric)) +
  geom_col(position=position_dodge(0.8), width=0.7) +
  geom_text(aes(label=sprintf("%.2f",value)), position=position_dodge(0.8), vjust=-0.3, size=2.6) +
  scale_fill_manual(values=c("ARI"="#2196F3","Silhouette"="#FF9800")) +
  ylim(0,1) +
  labs(title="Iris: GMM best ARI (0.90) but lowest silhouette", x=NULL, y="Value", fill=NULL) +
  theme_minimal(base_size=10) + theme(plot.title=element_text(face="bold",size=9))
p_wine <- ggplot(wine_df, aes(method, value, fill=metric)) +
  geom_col(position=position_dodge(0.8), width=0.7) +
  geom_text(aes(label=sprintf("%.2f",value)), position=position_dodge(0.8), vjust=-0.3, size=2.6) +
  scale_fill_manual(values=c("ARI"="#2196F3","Silhouette"="#FF9800")) +
  ylim(0,1.05) +
  labs(title="Wine: DBSCAN best on both (0.95 ARI, 0.61 sil)", x=NULL, y="Value", fill=NULL) +
  theme_minimal(base_size=10) + theme(plot.title=element_text(face="bold",size=9))

png("fig_4method_iriswine.png", width=2700, height=1200, res=300)
grid.arrange(p_iris, p_wine, ncol=2)
dev.off()
cat("saved fig_4method_iriswine.png\n")

# ---------- FIGURE: house, all five sets (silhouette) ----------
house_df <- data.frame(
  set = rep(c("Set 1\nStructural","Set 2\nStruct+Loc","Set 3\nStruct+Price",
              "Set 4\nLocation","Set 5\nLoc+Price"), 4),
  method = rep(methods, each=5),
  sil = c(0.288,0.174,0.261,0.431,0.356,   # k-means
          0.295,0.183,0.265,0.430,0.362,   # k-means-
          0.413,0.317,0.547,0.708,0.662,   # DBSCAN
          0.152,0.135,0.134,0.374,0.325)   # GMM
)
house_df$method <- factor(house_df$method, levels=methods)

png("fig_4method_house.png", width=2700, height=1350, res=300)
ggplot(house_df, aes(set, sil, fill=method)) +
  geom_col(position=position_dodge(0.85), width=0.8) +
  geom_text(aes(label=sprintf("%.2f",sil)), position=position_dodge(0.85), vjust=-0.3, size=2.3) +
  scale_fill_manual(values=c("k-means"="#2196F3","k-means-"="#4CAF50",
                             "DBSCAN"="#FF9800","GMM"="#9C27B0")) +
  labs(title="Four methods across all five house sets - DBSCAN highest throughout (with caveat)",
       x=NULL, y="Silhouette", fill="Method") +
  theme_minimal(base_size=11) + theme(plot.title=element_text(face="bold",size=10))
dev.off()
cat("saved fig_4method_house.png\n")
cat("\nPart 3 done - 2 figures saved.\n")
