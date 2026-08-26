# Simultaneous Clustering and Outlier Detection

This repository contains the R code for the MSc dissertation *Simultaneous
Clustering and Outlier Detection* (MATH5872M), submitted for the degree of MSc
in Data Science and Analytics at the University of Leeds.

The project compares four clustering methods **k-means**, **k-means−**,
**DBSCAN** and the **Gaussian mixture model**and the three different notions
of an outlier they embody (distance-based, density-based and probability-based),
across two labelled benchmark datasets (iris and wine) and a large, unlabelled
dataset of house sales in King County, USA.

## Overview of the analysis

The code addresses three questions:

1. **How many clusters?** Selecting the number of clusters using three internal
   validation indices (silhouette, Davies–Bouldin, Calinski–Harabasz), and
   examining their reliability on real data.
2. **Which method clusters best?** Comparing the four methods using the adjusted
   Rand index (on labelled data) and the silhouette index.
3. **How do the methods identify outliers?** Comparing the three notions of an
   outlier on a common, equal-count basis, and testing their agreement against a
   null baseline.

## Datasets

| Dataset | Source | Notes |
|---------|--------|-------|
| Iris | UCI / `datasets` | 150 observations, 3 species (two overlap) |
| Wine | [UCI Machine Learning Repository](https://archive.ics.uci.edu/ml/datasets/wine) | 178 observations, 3 cultivars, 13 features |
| King County house sales | [Kaggle](https://www.kaggle.com/datasets/harlfoxem/housesalesprediction) | 21,613 property sales |

The wine data is loaded directly from the UCI URL within the scripts. The house
data (`kc_house_data.csv`) should be downloaded from Kaggle and placed in the
working directory.

## Repository structure

```
.
├── README.md
├── analysis/
│   ├── iris_optk.R              # optimal k — iris
│   ├── wine_optk.R              # optimal k — wine
│   ├── house_optk.R            # optimal k — house (search extended to k=300)
│   ├── iris_outlier.R           # outlier comparison — iris
│   ├── wine_outlier.R           # outlier comparison — wine
│   ├── house_outlier.R          # outlier comparison — house
│   ├── iris_4method.R           # four-method comparison — iris
│   ├── wine_4method.R           # four-method comparison — wine
│   └── house_4method.R          # four-method comparison — house
├── enhancements/
│   ├── enhancement_1_cluster_profiling.R
│   ├── enhancement_2_eda.R
│   ├── enhancement_3_cluster_maps.R
│   └── enhancement_4_outlier_characterisation.R
└── figures/
    ├── figures_1_optimal_k.R
    ├── figures_2_outliers.R
    ├── figures_3_fourmethod.R
    ├── figures_4_enhancements.R
    └── figures_5_pca_scatter.R
```

## Requirements

The code was written in R. The following packages are required:

```r
install.packages(c(
  "cluster", "clusterSim", "fpc", "dbscan", "mclust", "FNN",
  "ggplot2", "gridExtra", "dplyr"
))
```

## Reproducing the results

1. Clone the repository.
2. Download `kc_house_data.csv` from Kaggle and place it in the working
   directory.
3. Run the scripts in `analysis/` to reproduce the main results (optimal number
   of clusters, method comparison, and outlier comparison).
4. Run the scripts in `enhancements/` for the closer analysis of the house data
   (cluster profiling, exploratory data analysis, cluster maps, and outlier
   characterisation).
5. Run the scripts in `figures/` to regenerate the figures used in the
   dissertation.

A fixed random seed (`set.seed(42)`) is used throughout for reproducibility. All
computations on the house data use the full dataset of 21,613 observations,
without sampling.

## Key findings

- On the house data, only the **silhouette index** remained reliable; extending
  the search to k = 300 revealed that the Davies–Bouldin and Calinski–Harabasz
  indices drift to implausible values (up to 175 and 275 clusters).
- The three notions of an outlier agreed on the same observations far more often
  than chance — by factors of roughly **90 to 4,000 times** a null baseline —
  and most strongly where the cluster structure was clearest.
- **No single clustering method** was best across all datasets: the Gaussian
  mixture model recovered the iris species most accurately despite the lowest
  silhouette score, illustrating that a high silhouette does not imply an
  accurate clustering.
- The observations considered outliers in the house data were the **large,
  expensive, high-grade properties**, extreme on several features at once, which
  explains why the different methods agreed on them.

## Author

Submitted as part of the MSc in Data Science and Analytics, University of Leeds.

## Acknowledgements

Supervised by Dr Arief Gusnanto, School of Mathematics, University of Leeds.
