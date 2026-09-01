
library(ggplot2)
library(gridExtra)

# ---------- FIGURE: iris & wine, three metrics ----------
k <- 2:10
iris_sil <- c(0.582,0.460,0.384,0.346,0.323,0.325,0.340,0.354,0.363)
iris_db  <- c(0.683,0.914,0.989,1.053,1.161,1.108,0.994,0.953,0.909)
iris_ch  <- c(251,242,207,203,187,179,175,178,180)
wine_sil <- c(0.259,0.285,0.258,0.232,0.196,0.206,0.152,0.144,0.143)
wine_db  <- c(1.589,1.468,1.866,1.774,1.918,1.728,1.811,1.781,1.779)
wine_ch  <- c(70,71,56,47,42,39,36,34,33)

mk_panel <- function(kk, vals, best_type, colour, title, ylab) {
  best_k <- if (best_type=="max") kk[which.max(vals)] else kk[which.min(vals)]
  df <- data.frame(k=kk, v=vals)
  # add headroom so nothing clips at top/bottom
  rng <- range(vals); pad <- 0.12*(rng[2]-rng[1])
  ggplot(df, aes(k, v)) +
    geom_line(colour=colour, linewidth=0.8) +
    geom_point(colour=colour, size=2) +
    geom_point(data=df[df$k==best_k,], colour="red", size=4) +
    scale_x_continuous(breaks=kk) +
    coord_cartesian(ylim=c(rng[1]-pad, rng[2]+pad), clip="off") +
    labs(title=title, subtitle=paste("optimum at k =", best_k), x="k", y=ylab) +
    theme_minimal(base_size=9) +
    theme(plot.title=element_text(face="bold", size=8.5),
          plot.subtitle=element_text(size=8),
          plot.margin=margin(10,12,8,8))   # extra margin prevents edge clipping
}

# LARGER canvas + wider so 3 columns don't squeeze
png("fig_optk_iriswine.png", width=3300, height=1900, res=300)
grid.arrange(
  mk_panel(k, iris_sil,"max","#2196F3","Iris: Silhouette (higher better)","Silhouette"),
  mk_panel(k, iris_db, "min","#FF9800","Iris: Davies-Bouldin (lower better)","Davies-Bouldin"),
  mk_panel(k, iris_ch, "max","#4CAF50","Iris: Calinski-Harabasz (higher better)","Calinski-Harabasz"),
  mk_panel(k, wine_sil,"max","#2196F3","Wine: Silhouette (higher better)","Silhouette"),
  mk_panel(k, wine_db, "min","#FF9800","Wine: Davies-Bouldin (lower better)","Davies-Bouldin"),
  mk_panel(k, wine_ch, "max","#4CAF50","Wine: Calinski-Harabasz (higher better)","Calinski-Harabasz"),
  ncol=3,
  top="Optimal k: three metrics agree - iris k=2 (all three), wine k=3 (all three)")
dev.off()
cat("saved fig_optk_iriswine.png\n")

# ---------- FIGURE: house drift ----------
sets <- c("Set 1\nStructural","Set 2\nStruct+Loc","Set 3\nStruct+Price",
          "Set 4\nLocation","Set 5\nLoc+Price")
drift <- data.frame(
  set = rep(sets, 3),
  metric = rep(c("Silhouette","Davies-Bouldin","Calinski-Harabasz"), each=5),
  k = c(3,4,2,3,2,  15,4,9,30,175,  2,2,2,275,3)
)
drift$set <- factor(drift$set, levels=sets)
drift$metric <- factor(drift$metric, levels=c("Silhouette","Davies-Bouldin","Calinski-Harabasz"))

png("fig_optk_housedrift.png", width=3000, height=1600, res=300)
ggplot(drift, aes(set, k, fill=metric)) +
  geom_col(position=position_dodge(0.8), width=0.75) +
  geom_text(aes(label=k), position=position_dodge(0.8), vjust=-0.4, size=3) +
  geom_hline(yintercept=10, linetype="dashed", colour="grey50") +
  annotate("text", x=4.5, y=22, label="sensible range", colour="grey40", size=3) +
  scale_fill_manual(values=c("Silhouette"="#2196F3","Davies-Bouldin"="#FF9800",
                             "Calinski-Harabasz"="#4CAF50")) +
  scale_y_continuous(expand=expansion(mult=c(0,0.08))) +
  labs(title="Silhouette stays sensible (2-4); Davies-Bouldin and Calinski-Harabasz drift to absurd k",
       x=NULL, y="Optimal k chosen", fill="Metric") +
  theme_minimal(base_size=11) +
  theme(plot.title=element_text(face="bold", size=9.5),
        legend.position="right",
        plot.margin=margin(12,12,8,8))
dev.off()
cat("saved fig_optk_housedrift.png\n")
cat("\nPart 1 done - 2 figures saved (fixed, no clipping).\n")
