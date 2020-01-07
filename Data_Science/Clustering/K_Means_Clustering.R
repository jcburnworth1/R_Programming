##### K-Means Clustering Example #####
## Import libraries
library(dplyr)
library(ggplot2)
library(purrr)
library(cluster)

## Create base data frame
lineup <- data.frame(x = c(-1,-2,8,7,-12,-15,-13,15,21,12,-25,26),
                     y = c(1,-3,6,-8,8,0,-10,16,2,-15,1,0))

##### Two centers #####
## Cluster the data with kmeans()
model_km2 <- kmeans(lineup, centers = 2)

## Clusters back to lineup
lineup <- mutate(lineup, cluster_2 = model_km2$cluster)

## Plot the clusters
ggplot(lineup, aes(x = x, y = y, color = factor(cluster_2))) + 
  geom_point()

##### Three centers #####
## Cluster the data with kmeans()
model_km2 <- kmeans(lineup, centers = 3)

## Clusters back to lineup
lineup <- mutate(lineup, cluster_3 = model_km2$cluster)

## Plot the clusters
ggplot(lineup, aes(x = x, y = y, color = factor(cluster_3))) + 
  geom_point()

##### Evaluating different values of K using elbow plots #####
## Looking for the elbow using map.dbl and varying values of k
# Use map_dbl to run many models with varying value of k (centers)
tot_withinss <- map_dbl(1:10,  function(k){
  model <- kmeans(x = lineup, centers = k)
  model$tot.withinss
})

# Generate a data frame containing both k and tot_withinss
elbow_df <- data.frame(
  k = 1:10 ,
  tot_withinss = tot_withinss
)

## Plot the elbow
# Plot the elbow plot
ggplot(elbow_df, aes(x = k, y = tot_withinss)) +
  geom_point() + 
  geom_line() +
  scale_x_continuous(breaks = 1:10)

##### Evaluating different values of K using silhoutte analysis #####
# Generate a k-means model using the pam() function with a k = 2
pam_k2 <- pam(lineup, k = 2)

# Plot the silhouette visual for the pam_k2 model
plot(silhouette(pam_k2))

# Generate a k-means model using the pam() function with a k = 3
pam_k3 <- pam(lineup, k = 3)

# Plot the silhouette visual for the pam_k3 model
plot(silhouette(pam_k3))