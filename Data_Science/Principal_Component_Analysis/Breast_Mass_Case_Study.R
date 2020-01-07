##### Human Breast Mass Case Study - PCA, Clustering #####
##### Load Libraries #####
## None needed at this time

##### Load Data & Prep #####
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"
wisc.df <- read.csv(url)

## Convert to matrix
wisc.data <- as.matrix(wisc.df[,3:32])

## Set rown names of matrix to wisc.fd$id
row.names(wisc.data) <- wisc.df$id

## Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

##### EDA on the data #####
## 1.) How many observations are in this dataset?
str(wisc.df) # 569

## 2.) How many variables/features in the data are suffixed with _mean?
length(grep("_mean", names(wisc.df))) # 10

## 3.) How many of the observations have a malignant diagnosis?
table(wisc.df$diagnosis) # 212

##### Performing PCA #####
## Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, MARGIN = 2, sd)

## Execute PCA, scaling if appropriate: wisc.pr
wisc.pr <- prcomp(wisc.data, scale = TRUE)

## Look at summary of results
summary(wisc.pr)

##### Visual the PCA results #####
## Create a biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
        xlab = "PC1", ylab = "PC3")

##### Explaining the variance #####
# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Calculate variability of each component
pr.var <- wisc.pr$sdev ^ 2

# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component", 
        ylab = "Proportion of Variance Explained", 
        ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
        ylab = "Cumulative Proportion of Variance Explained", 
        ylim = c(0, 1), type = "b")

##### Hierarchical Clustering of the data #####
## Scale the data
data.scaled <- scale(wisc.data)

## Calculate the euclidean distance
data.dist <- dist(data.scaled, method = "euclidean")

## Build a hierarchical model
wisc.hclust <- hclust(data.dist, method = "complete")

## Plot results of the hierarchical clustering
plot(wisc.hclust)
abline(h = 20, col = "red")

# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)

## Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)

##### k-means Clustering of the data #####
## Create a k-means model on wisc.data: wisc.km
wisc.km <- kmeans(scale(wisc.data), centers = 2, nstart = 20)

## Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)

##### Hierarchical Clustiner on PCA Results #####
## Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[,1:7]), method = "complete")

## Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

## Compare to actual diagnoses
table(wisc.hclust.clusters, diagnosis)
table(wisc.pr.hclust.clusters, diagnosis)
table(wisc.km$cluster, diagnosis)

#3 Compare to k-means and hierarchical
table(wisc.hclust.clusters, wisc.km$cluster)