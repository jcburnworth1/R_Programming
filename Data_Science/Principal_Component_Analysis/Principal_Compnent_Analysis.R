##### Principal Component Analysis #####
## Read in the data & setup the proper matrix
pokemon <- read.csv('~/Documents/Code/R_Programming/Data_Science/Clustering/Pokemon.csv',
                                stringsAsFactors = FALSE)[,c(2,6:11)]

## To matrix & add row names
pokemon_matrix <- data.matrix(pokemon[,2:7])
row.names(pokemon_matrix) <- pokemon[,1]

## Perform scaled PCA: pr.out
pr.out <- prcomp(pokemon_matrix, scale = TRUE)

## Inspect model output
summary(pr.out)

## Looking at the summary, the minimum number of principal compnents needed to summarize the data is 3
## See below in Cumulative Proportion PC3 column
# Importance of components:
#   PC1    PC2    PC3    PC4     PC5     PC6
# Standard deviation     1.6466 1.0457 0.8825 0.8489 0.65463 0.51681
# Proportion of Variance 0.4519 0.1822 0.1298 0.1201 0.07142 0.04451
# Cumulative Proportion  0.4519 0.6342 *0.7640* 0.8841 0.95549 1.00000

##### Plotting the results of PCA #####
## Biplot - Will show relationships between components and help us see what components are correlated
biplot(pr.out)

## Scree plot
## Process the data to build the scree
## Variability of each principal component: pr.var
pr.var <- pr.out$sdev

## Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

## Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
        ylab = "Proportion of Variance Explained",
        ylim = c(0, 1), type = "b")

## Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
        ylab = "Cumulative Proportion of Variance Explained",
        ylim = c(0, 1), type = "b")

##### Practical Issues with PCA #####
##### Scaling the data #####
## Mean of each variable
colMeans(pokemon_matrix)

## Standard deviation of each variable
apply(pokemon_matrix, 2, sd)

## PCA model with scaling: pr.with.scaling
pr.with.scaling <- prcomp(pokemon_matrix, scale = TRUE)

## PCA model without scaling: pr.without.scaling
pr.without.scaling <- prcomp(pokemon_matrix, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)