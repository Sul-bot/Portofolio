# Install and load necessary packages
install.packages("tidyverse")
library(tidyverse)
library(factoextra)
library(cluster)

# Reading the data
data <- read.csv("/Users/sulthanrazin/Downloads/metcuan4.csv")
View(data)

# Prepare the data for clustering
dataku = data[,-1]
row.names(dataku) = data[,1]
View(dataku)

# Summary and structure of the data
summary(dataku)

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
