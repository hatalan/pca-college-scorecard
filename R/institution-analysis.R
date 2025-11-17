# Packages used
library(ggplot2)
library(tidyverse)
library(naniar)
library(readxl)
library(ggforce)
library(msos)
library(gridExtra)
library(AMR)
library(writexl)
library(ggfortify)
library(MASS)
library(cluster)
library(ggrepel)
library(ggdendro)

setwd("C:/Users/irgroup/Documents/Projects/pca-college-scorecard/data")

# Reading in institution data
institution <- read.csv("Most-Recent-Cohorts-Institution_05192025.csv")
institution <- institution %>%
  mutate(across(where(is.character), ~na_if(., "PS"))) %>%
  type.convert(as.is = TRUE)

# Reading in data dictionary
data_dic <- read_xlsx("CollegeScorecardDataDictionary.xlsx", sheet = 4)
my_vars <- c("VARIABLE NAME", "API data type")
data_dic <- data_dic %>%
  dplyr::select(all_of(my_vars))
data_dic <- na.omit(data_dic)

# Keep variables with <30% missing
miss_var <- miss_var_summary(institution)
var <- inner_join(miss_var, data_dic, by = c("variable" = "VARIABLE NAME"))
vars_keep <- var %>%
  filter(pct_miss < 30) %>%
  rename(data_type = "API data type") %>%
  filter(data_type == "float") %>%
  pull(variable)
vars_keep <- c("INSTNM", vars_keep)

# Impute means for remaining missing cells
institution_clean <- institution %>%
  dplyr::select(all_of(vars_keep)) %>%
  dplyr::select(INSTNM, where(is.numeric)) %>%
  mutate(across(where(is.numeric), impute_mean)) %>%
  type.convert(as.is = TRUE)

# Calculate covariance matrix of numeric variables
cov = cov(institution_clean[, c(-1, -174:-172)])

# Calculate PCA
pca = prcomp(institution_clean[, c(-1, -174:-172)], center = TRUE, scale = TRUE)

# Scree plot for covariance matrix
index <- 1:15
Rfractions <- pca$sdev^2 / sum(pca$sdev^2)
CRfractions <- cumsum(Rfractions)
pca_plot_1 <- ggplot() +
  geom_line(aes(x = index, y = Rfractions[1:15])) +
  geom_point(aes(x = index, y = Rfractions[1:15]), size = 2) +
  theme_bw() +
  labs(x = "Principal Component", y = "Variance Explained") +
  geom_line(aes(x = index, y = CRfractions[1:15]), color = "gray") +
  geom_point(aes(x = index, y = CRfractions[1:15]), color = "gray", size = 2) +
  annotate("label", x = 6, y = 0.65, label = "Cumulative", color = "gray") +
  annotate("label", x = 6, y = 0.12, label = "Individual", color = "black") +
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 1)) +
  ggtitle("Scree Plot of Covariance Matrix")

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/pca_plot_one.png", 
       plot = pca_plot_1, 
       width = 8,        # Width of the plot in inches
       height = 6,       # Height of the plot in inches
       dpi = 300)         # DPI (dots per inch) for high resolution

#Bi plot
biplot_1 <- ggplot_pca(pca, choices = c(1, 2), ellipse = TRUE, arrows = FALSE) +
  labs(title = "Biplot of PCA for all Institutions")

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/biplot_1.png", 
       plot = biplot_1, 
       width = 8,
       height = 6,
       dpi = 300)

# Calculate K-means
WGSS <- sapply(1:10, function(zz) kmeans(institution_clean[, c(-1, -174:-172)], centers = zz)$tot.withinss)

wgss_1 <- ggplot() +
          geom_point(aes(x = 1:10, y = WGSS)) +
          geom_line(aes(x = 1:10, y = WGSS)) +
          labs(title = "WGSS by Number of Clusters", x = "K", y = "WGSS") +
          theme_bw()

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/wgss.png", 
       plot = wgss_1, 
       width = 8,
       height = 6,
       dpi = 300)

kmeans = kmeans(institution_clean[, c(-1, -174:-172)], centers = 3)
institution_clean$cluster = kmeans$cluster

# Plot clusters with first two principle components
kmeans_1 <- ggplot() +
          geom_point(aes(x = pca$x[,1], y = pca$x[,2], color = factor(kmeans$cluster))) +
          labs(x = "PC1", y = "PC2", title = "K-means Clustering with 1st and 2nd PCs") +
          labs(color = "Cluster") + 
          theme_minimal()

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/kmeans_1.png", 
       plot = kmeans_1, 
       width = 8,
       height = 6,
       dpi = 300)

# Most influential variable on PC1
pc1_loadings <- pca$rotation[, "PC1"]
abs_pc1_loadings <- abs(pc1_loadings)
most_influential_variable <- names(which.max(abs_pc1_loadings))
print(most_influential_variable)

# Most influential variable on PC2
pc2_loadings <- pca$rotation[, "PC2"]
abs_pc2_loadings <- abs(pc2_loadings)
most_influential_variable <- names(which.max(abs_pc2_loadings))
print(most_influential_variable)




### Process repeated on selective 4-year universities




# Filtering for selective 4-year universities
institution_4yr <- institution %>%
  mutate(across(where(is.character), ~na_if(., "PS"))) %>%
  type.convert(as.is = TRUE) %>%
  filter(CCUGPROF %in% c(14, 15)) 

# Reading in data dictionary
data_dic <- read_xlsx("CollegeScorecardDataDictionary.xlsx", sheet = 4)
data_dic <- data_dic %>%
  dplyr::select("VARIABLE NAME", "API data type")
data_dic <- na.omit(data_dic)

#Keeping variables with <1% missing data
miss_var <- miss_var_summary(institution_4yr)
var <- inner_join(miss_var, data_dic, by = c("variable" = "VARIABLE NAME"))
vars_keep <- var %>%
  filter(pct_miss < 1) %>%
  rename(data_type = "API data type") %>%
  filter(data_type == "float") %>%
  pull(variable)
vars_keep <- c("INSTNM", vars_keep)

# Impute means for remaining missing data
institution_4yr_clean <- institution_4yr %>%
  dplyr::select(all_of(vars_keep)) %>%
  dplyr::select(INSTNM, where(is.numeric)) %>%
  mutate(across(where(is.numeric), impute_mean)) %>%
  type.convert(as.is = TRUE)
institution_4yr_clean <-  institution_4yr_clean %>%
  dplyr::select(where(~ length(unique(.)) > 1))

# Calculate covariance matrix
cov = cov(institution_4yr_clean[, c(-1)])

# Calculate PCA
pca_4yr = prcomp(institution_4yr_clean[, c(-1)], center = TRUE, scale = TRUE)

# Scree plot for covariance matrix
index <- 1:15 
Rfractions <- pca_4yr$sdev^2 / sum(pca_4yr$sdev^2)
CRfractions <- cumsum(Rfractions)
pca_plot_2 <- ggplot() +
  geom_line(aes(x = index, y = Rfractions[1:15])) +
  geom_point(aes(x = index, y = Rfractions[1:15]), size = 2) +
  theme_bw() +
  labs(title = "Scree Plot of Covariance Matrix", x = "Principal Component", y = "Variance Explained") +
  geom_line(aes(x = index, y = CRfractions[1:15]), color = "gray") +
  geom_point(aes(x = index, y = CRfractions[1:15]), color = "gray", size = 2) +
  annotate("label", x = 6, y = 0.65, label = "Cumulative", color = "gray") +
  annotate("label", x = 6, y = 0.12, label = "Individual", color = "black") +
  coord_cartesian(xlim = c(1, 15), ylim = c(0, 1))

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/pca_plot_one.png", 
       plot = pca_plot_2, 
       width = 8,
       height = 6,
       dpi = 300)

# Biplot
biplot_2 <- ggplot_pca(pca_4yr, choices = c(1, 2), ellipse = TRUE, arrows = FALSE) + 
      labs(title = "Biplot of PCA for Selective, 4-Year Institutions")

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/biplot_2.png", 
       plot = biplot_2, 
       width = 8,
       height = 6,
       dpi = 300)

# Choosing number of clusters for K-means
WGSS <- sapply(1:10, function(zz) kmeans(institution_4yr_clean[, c(-1, -174:-172)], centers = zz)$tot.withinss)

wgss_2 <- ggplot() +
  geom_point(aes(x = 1:10, y = WGSS)) +
  geom_line(aes(x = 1:10, y = WGSS)) +
  labs(title = "WGSS by Number of Clusters", x = "K", y = "WGSS") +
  theme_bw()

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/wgss_2.png", 
       plot = wgss_2, 
       width = 8,
       height = 6,
       dpi = 300)

# Implementing K-means clustering with 3 clusters
kmeans_result <- kmeans(institution_4yr_clean[, -1], centers = 3, nstart = 25)
institution_4yr_clean$cluster <- kmeans_result$cluster
institution_4yr_clean$cluster <- as.character(institution_4yr_clean$cluster)

# PCA Biplot with K-means
kmeans_2 <- ggplot() +
  geom_point(aes(x = pca_4yr$x[,1], y = pca_4yr$x[,2], color = factor(kmeans_result$cluster))) +
  labs(x = "PC1", y = "PC2", title = "K-means Clustering with 1st and 2nd PCs") +
  labs(color = "Cluster") + 
  theme_minimal()

ggsave(filename = "C:/Users/irgroup/Documents/Projects/pca-college-scorecard/output/kmeans_2.png", 
       plot = kmeans_2, 
       width = 8,
       height = 6,
       dpi = 300)

# Most influential variable on PC1
pc1_loadings <- pca_4yr$rotation[, "PC1"]
abs_pc1_loadings <- abs(pc1_loadings)
most_influential_variable <- names(which.max(abs_pc1_loadings))
print(most_influential_variable)

# Most influential variable on PC
pc2_loadings <- pca_4yr$rotation[, "PC2"]
abs_pc2_loadings <- abs(pc2_loadings)
most_influential_variable <- names(which.max(abs_pc2_loadings))
print(most_influential_variable)
