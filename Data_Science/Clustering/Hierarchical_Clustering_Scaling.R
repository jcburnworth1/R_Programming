##### Hierarchical Clustering Example with scaling #####
## Create data frame for our data
x <- data.frame(x = c(3.37095845,1.43530183,2.36312841,2.6328626,2.40426832,1.89387548,3.511522,
                      1.90534096,4.01842371,1.9372859,3.30486965,4.28664539,0.6111393,1.72121123,
                      1.86667866,2.6359504,1.71574708,-0.65645542,-0.44046693,3.32011335,
                      1.69336141,0.21869157,1.82808264,3.2146747,3.89519346,-5.43046913,-5.25726938,
                      -6.76316309,-4.53990265,-5.63999488,-4.54454988,-4.29516266,-3.96489648,-5.60892638,
                      -4.49504488,-1.71700868,-0.78445901,-0.85090759,-2.41420765,0.03612261,0.2059986,
                      -0.3610573,0.75816324,-0.72670483,-1.36828104,0.43281803,-0.81139318,1.44410126,
                      -0.4314462,0.65564788),
                y = c(2.3219253,1.2161611,3.5757275,2.6428993,2.0897606,2.2765507,2.6792888,2.0898329,
                      -0.9930901,2.284883,1.6327654,2.1852306,2.5818237,3.3997368,1.2727079,3.3025426,
                      2.3358481,3.0385061,2.9207286,2.7208782,0.9568811,1.9098136,2.6235182,1.0464766,
                      1.4571712,2.5809965,2.7681787,2.4637676,1.1142237,0.9002191,3.512707,2.2579214,
                      2.0884402,1.8791035,0.8056711,-1.3880031,-2.2171398,-2.1827567,-1.0666537,
                      -1.1782269,-0.6078836,-2.4761739,-1.3496514,-0.6088895,-3.1107889,-2.8607926,
                      -3.1317387,-3.459214,-1.9200174,-1.3467957))

## To matrix
x <- data.matrix(x)

##### Build the initial model #####
## Create hierarchical clustering model: hclust.out
hclust.out <- hclust(dist(x), method = "complete")

## Inspect the result
summary(hclust.out)

## Plot the dendogram of the cluster and draw some hyppthetical cut lines
plot(hclust.out, ylim = c(0,10))
abline(h = 3.5, col = "red")
abline(h = 4.5, col = "red")
abline(h = 6.9, col = "red")
abline(h = 9.0, col = "red")

##### Cut the cluster down to size #####
## By Height
h_7 <- cutree(hclust.out, h = 7)

## By Clusters
k_3 <- cutree(hclust.out, k = 3)

##### Changing linkage types #####
## Cluster using complete linkage: hclust.complete - Will produce balanced trees
hclust.complete <- hclust(dist(x), method = "complete")

## Cluster using average linkage: hclust.average - Will produce balanced trees
hclust.average <- hclust(dist(x), method = "average")

## Cluster using single linkage: hclust.single - Will produce unbalanced trees useful for finding outliers
hclust.single <- hclust(dist(x), method = "single")

## Plot dendrogram of hclust.complete
plot(hclust.complete, main = "Complete")

## Plot dendrogram of hclust.average
plot(hclust.average, main = "Average")

## Plot dendrogram of hclust.single
plot(hclust.single, main = "Single")

##### Scaling Data #####
## Will use the pokemon data for this
pokemon <- data.matrix(read.csv('~/Documents/Code/R_Programming/Data_Science/Clustering/Pokemon.csv',
                                stringsAsFactors = FALSE)[,6:11])

## View column means
colMeans(pokemon)

## View column standard deviations
apply(pokemon, MARGIN = 2, sd)

## Scale the data
pokemon.scaled <- scale(pokemon)

## Create hierarchical clustering model: hclust.pokemon
hclust.pokemon <- hclust(dist(pokemon.scaled), method = "complete")

## Plot the result
plot(hclust.pokemon)

## Cut the tree into 3 clusters
cut.pokemon <- cutree(hclust.pokemon, k = 3)