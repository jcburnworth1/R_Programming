## Hierarchical Clustering Example
## Import libraries
library(ggplot2)
library(dendextend)
library(dplyr)

## Create base data frame
lineup = data.frame(x = c(-1,-2,8,7,-12,-15,-13,15,21,12,-25,26),
                    y = c(1,-3,6,-8,8,0,-10,16,2,-15,1,0))

## Initial plot of the players
ggplot(lineup, aes(x = x, y = y)) + 
  geom_point() + 
  ggtitle("Soccer Players on a Field")

## Calculate distances
dist_players <- dist(lineup)

## Cluster using "complete" linkage - Changing to difference linkages will change the clustering
## Compelete - Maximum distance between groups
## Single - Minimum distance between groups
## Mean - Average distance between groups
hc_players_complete <- hclust(dist_players, method = "complete")
hc_players_single <- hclust(dist_players, method = "single")
hc_players_average <- hclust(dist_players, method = "average")

## Show dendogram of the clusters - Since this is 2D - We can also use scatter plots
plot(hc_players_complete, main = "Complete Linkage Dendogram")
plot(hc_players_single, main = "Single Linkage Dendogram")
plot(hc_players_average, main = "Average Linkage Dendogram")

## Convert hclust object to dendograms
hc_players_complete_dend <- as.dendrogram(hc_players_complete)
hc_players_single_dend <- as.dendrogram(hc_players_single)
hc_players_average_dend <- as.dendrogram(hc_players_average)

## Plot the dendograms with cuts, will use height = 20 & 40 as example
par(mfrow = c(2,3))
plot(color_branches(hc_players_complete_dend, h = 20), main = "Complete Linkage Dendogram - 20")
plot(color_branches(hc_players_single_dend, h = 20), main = "Single Linkage Dendogram - 20")
plot(color_branches(hc_players_average_dend, h = 20), main = "Average Linkage Dendogram - 20")
plot(color_branches(hc_players_complete_dend, h = 40), main = "Complete Linkage Dendogram - 40")
plot(color_branches(hc_players_single_dend, h = 40), main = "Single Linkage Dendogram - 40")
plot(color_branches(hc_players_average_dend, h = 40), main = "Average Linkage Dendogram - 40")

## Cut the tree down to the desired height (h) or clusters (k)
## cutree takes h (height) or k (number of clusters)
hc_players_complete_cut_20 <- cutree(hc_players_complete, h = 20)
hc_players_complete_cut_40 <- cutree(hc_players_complete, h = 40)

## Clusters back to original data frame
lineup <- lineup %>% 
  mutate(complete_cluster_20 = hc_players_complete_cut_20,
         complete_cluster_40 = hc_players_complete_cut_40)

## Plot results of each cluster
## h = 20 which gives four total clusters (doesn't make sense since soccer is two teams)
ggplot(lineup, aes(x = x, y = y, color = factor(complete_cluster_20))) + 
  geom_point() + 
  ggtitle("Soccer Players Clustered with h = 20")

## h = 40 which gives two total clusters
ggplot(lineup, aes(x = x, y = y, color = factor(complete_cluster_40))) + 
  geom_point() + 
  ggtitle("Soccer Players Clustered with h = 40")
