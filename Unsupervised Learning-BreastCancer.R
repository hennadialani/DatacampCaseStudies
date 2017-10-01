#Unsupervised Learning Case Study - Breast Cancer: data has measurements of
#cell nuclei of human breast masses

#Prepping data for EDA
url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

wisc.df<-read.csv(url)

wisc.data<-as.matrix(wisc.df[,3:32])

row.names(wisc.data) <- wisc.df$id

# diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")

#EDA
nrow(wisc.data)  #number of observations
colnames(wisc.data) #which columns have mean suffix
table(diagnosis) #no of trues/falses

#Performing PCA
# Check column means and standard deviations to see if data needs to be scaled
colMeans(wisc.data) 
apply(wisc.data, 2, sd)

# Execute PCA, scaling needed
wisc.pr<-prcomp(x = wisc.data, scale = TRUE)

# summary of results
summary(wisc.pr)

#interpreting PCA
#biplot of wisc.pr
biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

#Principal component 2 explains more variance than principal component 3

#Explaining variance
par(mfrow = c(1, 2))

# variability of each component
pr.var<-wisc.pr$sdev^2

# Variance explained by each principal component
pve<-pr.var/sum(pr.var)

# variance explained for each principal component
plot(pve, xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

# cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component", 
     ylab = "Cumulative Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b")

#communicating PCA
wisc.pr$rotation[,1:3]

#Hierarchical clustering
# Scale the wisc.data data
data.scaled <- scale(wisc.data)

# Calculate the (Euclidean) distances
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model
wisc.hclust <- hclust(data.dist, method = "complete")

#Hierarchical clustering results
plot(wisc.hclust)

#Selecting number of clusters
# Cut tree so that it has 4 clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k = 4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)

#kmeans clustering, comparing results
# k-means model on wisc.data
wisc.km <- kmeans(scale(wisc.data), centers = 2, nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)

#Clustering on PCA results
# Create a hierarchical clustering model
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
table(wisc.pr.hclust.clusters, diagnosis)

# Compare to k-means and hierarchical
table(wisc.hclust.clusters, diagnosis)
table(wisc.km$cluster, diagnosis)