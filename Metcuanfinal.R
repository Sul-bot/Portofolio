# Install and load necessary packages
install.packages("tidyverse")
install.packages("factoextra")
install.packages("cluster")
install.packages("ggplot2")
install.packages("dplyr")

library(tidyverse)
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)

# Reading the data
data <- read.csv("/Users/sulthanrazin/Downloads/metcuan4.csv")
View(data)

# Prepare the data for clustering
dataku = data[,-1]
row.names(dataku) = data[,1]
View(dataku)

# Summary and structure of the data
summary(dataku)

# Perform PCA
pca <- prcomp(dataku, scale. = TRUE)

# Lihat hasil PCA
summary(pca)


# Biplot to visualize the clustering results
fviz_pca_biplot(pca, 
                geom.ind = "point", # Show points only (not text)
                col.ind = as.factor(final$cluster), # color by clusters
                palette = "jco", 
                addEllipses = TRUE, # Concentration ellipses
                legend.title = "Clusters") +
  labs(title = "Analisis Biplot dengan Klaster",
       x = "PC1", 
       y = "PC2")

# Visualisasi varians yang dijelaskan oleh setiap komponen utama
fviz_eig(pca, addlabels = TRUE, ylim = c(0, 50)) +
  labs(title = "Varians yang Dijelaskan oleh Setiap Komponen Utama",
       x = "Komponen Utama",
       y = "Persentase Varians")

# Visualisasi individu dalam ruang PCA
fviz_pca_ind(pca,
             col.ind = as.factor(final$cluster), # color by clusters
             palette = "jco", 
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Clusters") +
  labs(title = "Visualisasi Individu dengan PCA",
       x = "PC1", 
       y = "PC2")

# Visualisasi variabel dalam ruang PCA
fviz_pca_var(pca,
             col.var = "cos2", # Color by quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE) +    # Avoid text overlapping
  labs(title = "Visualisasi Variabel dengan PCA",
       x = "PC1", 
       y = "PC2")

# Create boxplots for each variable
# Pivot the data to long format for ggplot2
dataku_long <- dataku %>% 
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

# Plot boxplots
ggplot(dataku_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  labs(title = "Boxplot untuk Setiap Variabel Sebelum Klasterisasi", 
       x = "Variabel", 
       y = "Nilai") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Determine the optimal number of clusters using the Elbow method
fviz_nbclust(dataku, kmeans, method = "wss") + 
  labs(title = "Metode Elbow untuk Menentukan Jumlah Cluster Optimal")

# Determine the optimal number of clusters using the Silhouette method
fviz_nbclust(dataku, kmeans, method = "silhouette")

# Perform k-means clustering with 2 clusters (example)
final = kmeans(dataku, 2)
# Create a cluster plot
fviz_cluster(final, data = dataku)

# Add cluster information to the original data
finalakhir = data.frame(dataku, final$cluster)
View(finalakhir)

# Order data by cluster
finalakhir_sorted = finalakhir[order(finalakhir$final.cluster),]
View(finalakhir_sorted)

# Calculate mean of each cluster
dataku %>% mutate(cluster = final$cluster) %>%
  group_by(cluster) %>% summarise_all("min")

# Create boxplots for each variable by cluster
finalakhir %>% 
  pivot_longer(-final.cluster, names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = factor(final.cluster), y = value, fill = factor(final.cluster))) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  labs(title = "Boxplot per Variable per Cluster", x = "Cluster", y = "Value") +
  theme_minimal()

