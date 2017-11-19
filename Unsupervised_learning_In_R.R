<H1>k-means clustering</H1>

In this exercise, your task is to create a k-means model of the x data using 3 clusters, then to look at the structure of the resulting model using the summary() function.        

# Create the k-means model: km.out
km.out <- kmeans(x, center = 3, nstart = 20)

# Inspect the result
summary(km.out)

<H1>Results of kmeans()</H1>
# Print the cluster membership component of the model
km.out$cluster

# Print the km.out object
print(km.out)

<H1>Visualizing and interpreting results of kmeans()</H1>
        x and km.out are available in your workspace. Using the plot() function to create a scatter plot of data x:
        
Color the dots on the scatterplot by setting the col argument to the cluster component in km.out.
Title the plot "k-means with 3 clusters" using the main argument to plot().
Ensure there are no axis labels by specifying "" for both the xlab and ylab arguments to plot().

# Scatter plot of x
plot(x, col=km.out$cluster, main="k-means with 3 clusters", xlab = "", ylab ="")


<H1>Handling random algorithms</H1>
In the video, you saw how kmeans() randomly initializes the centers of clusters. This random initialization can result in assigning observations to different cluster labels. Also, the random initialization can result in finding different local minima for the k-means algorithm. This exercise will demonstrate both results.

At the top of each plot, the measure of model quality—total within cluster sum of squares error—will be plotted. Look for the model(s) with the lowest error to find models with the better model results. 
        
# Set up 2 x 3 plotting grid
 par(mfrow = c(2, 3))

# Set seed
set.seed(1)

for(i in 1:6) {
        # Run kmeans() on x with three clusters and one start
        km.out <- kmeans(x, center = 3, nstart = 1)
        
        # Plot clusters
        plot(x, col = km.out$cluster, 
             main = km.out$tot.withinss, 
             xlab = "", ylab = "")
}        
        
        
<H1>Selecting number of clusters</H1>
        # Initialize total within sum of squares error: wss
        wss <- 0

# For 1 to 15 cluster centers
for (i in 1:15) {
        km.out <- kmeans(x, centers = i, nstart = 20)
        # Save total within sum of squares to wss variable
        wss[i] <- km.out$tot.withinss
}

# Plot total within sum of squares vs. number of clusters
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Set k equal to the number of clusters corresponding to the elbow location
k <- 2  # 3 is probably OK, too

<H1>Practical matters: working with real data</H1>

# Initialize total within sum of squares error: wss
wss <- 0

# Look over 1 to 15 possible clusters
for (i in 1:15) {
        # Fit the model: km.out
        km.out <- kmeans(pokemon, centers = i, nstart = 20, iter.max = 50)
        # Save the within cluster sum of squares
        wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:15, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Select number of clusters
k <- 3

# Build model with k clusters: km.out
km.out <- kmeans(pokemon, centers = k, nstart = 20, iter.max = 50)

# View the resulting model
km.out

# Plot of Defense vs. Speed by cluster membership
plot(pokemon[, c("Defense", "Speed")],
     col = km.out$cluster,
     main = paste("k-means clustering of Pokemon with", k, "clusters"),
     xlab = "Defense", ylab = "Speed")

<H1>Hierarchical clustering with results</H1>
        # Create hierarchical clustering model: hclust.out
        hclust.out <- hclust(d=dist(x))

# Inspect the result
summary(hclust.out)        

<H1>Linkage methods</H1>
        # Cluster using complete linkage: hclust.complete
        hclust.complete <- hclust(dist(x), method = "complete")

# Cluster using average linkage: hclust.average
hclust.average <- hclust(dist(x), method = "average")

# Cluster using single linkage: hclust.single
hclust.single <- hclust(dist(x), method = "single")

# Plot dendrogram of hclust.complete
plot(hclust.complete, main = "Complete")

# Plot dendrogram of hclust.average
plot(hclust.average, main = "Average")

# Plot dendrogram of hclust.single
plot(hclust.single, main = "Single")


<H1>Practical matters: scaling</H1>
        # View column means
        colMeans(pokemon)

# View column standard deviations
apply(pokemon, 2, sd)

# Scale the data
pokemon.scaled<-scale(pokemon)

# Create hierarchical clustering model: hclust.pokemon
hclust.pokemon <- hclust(dist(pokemon.scaled), method = "complete")


<H1>PCA using prcomp()</H1>
        # Perform scaled PCA: pr.out
        pr.out <- prcomp(x = pokemon, scale = TRUE)

# Inspect model output
summary (pr.out)        

Importance of components%s:
        PC1    PC2    PC3     PC4
Standard deviation     1.4420 1.0013 0.7941 0.53595
Proportion of Variance 0.5199 0.2507 0.1577 0.07181
Cumulative Proportion  0.5199 0.7705 0.9282 1.00000


<H1>Variance Explained</H1>
        pr.out and the pokemon data are still available in your workspace.

Assign to the variable pr.var the square of the standard deviations of the principal components (i.e. the variance). The standard deviation of the principal components is available in the sdev component of the PCA model object.

Assign to the variable pve the proportion of the variance explained, calculated by dividing pr.var by the total variance explained by all principal components.

# Variability of each principal component: pr.var
pr.var <- pr.out$sdev^2

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

<H1>Practical issues: scaling</H1>
        
        
        # Mean of each variable
        colMeans(pokemon)

# Standard deviation of each variable
apply(pokemon, 2, sd)

# PCA model with scaling: pr.with.scaling
pr.with.scaling<-prcomp(x = pokemon, scale = TRUE)

# PCA model without scaling: pr.without.scaling
pr.without.scaling<-prcomp(x = pokemon, scale = FALSE)

# Create biplots of both for comparison
biplot(pr.with.scaling)
biplot(pr.without.scaling)


<H1>Preparing the data</H1>
        Use read.csv() function to download the CSV (comma-separated values) file containing the data from the URL provided. Assign the result to wisc.df.

Use as.matrix() to convert the features of the data (in columns 3 through 32) to a matrix. Store this in a variable called wisc.data

Assign the row names of wisc.data the values currently contained in the id column of wisc.df. While not strictly required, this will help you keep track of the different observations throughout the modeling process.

Finally, set a vector called diagnosis to be 1 if a diagnosis is malignant ("M") and 0 otherwise. Note that R coerces TRUE to 1 and FALSE to 0.

url <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1903/datasets/WisconsinCancer.csv"

# Download the data: wisc.df
wisc.df<-read.csv(url)

# Convert the features of the data: wisc.data
wisc.data <- as.matrix(wisc.df[,3:32])

# Set the row names of wisc.data
row.names(wisc.data) <- wisc.df$id

# Create diagnosis vector
diagnosis <- as.numeric(wisc.df$diagnosis == "M")


<H1>Performing PCA</H1>
        
        The variables you created before, wisc.data and diagnosis, are still available in your workspace.

Check the mean and standard deviation of the features of the data to determine if the data should be scaled. Use the colMeans() and apply() functions like youve done before.

Execute PCA on the wisc.data, scaling if appropriate, and assign the model to wisc.pr.

Inspect a summary of the results with the summary() function.        
# Check column means and standard deviations
colMeans(wisc.data)
apply(wisc.data, 2, sd)


# Execute PCA, scaling if appropriate: wisc.pr
wisc.pr <- prcomp(x = wisc.data, scale = TRUE)


# Look at summary of results
summary(wisc.pr)


<H1>Interpreting PCA results</H1>
        The variables you created before, wisc.data, diagnosis, and wisc.pr, are still available.

Create a biplot of the wisc.pr data. What stands out to you about this plot? Is it easy or difficult to understand? Why?
        
        Execute the code to scatter plot each observation by principal components 1 and 2, coloring the points by the diagnosis.

Repeat the same for principal components 1 and 3. What do you notice about these plots?
        
        # Create a biplot of wisc.pr
        biplot(wisc.pr)

# Scatter plot observations by components 1 and 2
plot(wisc.pr$x[, c(1, 2)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC2")

# Repeat for components 1 and 3
plot(wisc.pr$x[, c(1, 3)], col = (diagnosis + 1), 
     xlab = "PC1", ylab = "PC3")

# Do additional data exploration of your choosing below (optional)

# Variability of each principal component: pr.var
pr.var <- wisc.pr$sdev^2
# Variance explained by each principal component: pve
pve <- pr.var / sum(pr.var)

# Set up 1 x 2 plotting grid
par(mfrow = c(1, 2))

# Plot variance explained for each principal component
plot(pve, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")

# Plot cumulative proportion of variance explained
plot(cumsum(pve), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     ylim = c(0, 1), type = "b")


<H1>Hierarchical clustering of case data</H1>
        # Scale the wisc.data data: data.scaled
        data.scaled <- scale(wisc.data)

# Calculate the (Euclidean) distances: data.dist
data.dist <- dist(data.scaled)

# Create a hierarchical clustering model: wisc.hclust
wisc.hclust <- hclust(data.dist, method = "complete")


Selecting number of clusters
# Cut tree so that it has 4 clusters: wisc.hclust.clusters
wisc.hclust.clusters <- cutree(wisc.hclust, k=4)

# Compare cluster membership to actual diagnoses
table(wisc.hclust.clusters, diagnosis)

k-means clustering and comparing results
# Create a k-means model on wisc.data: wisc.km
wisc.km <- kmeans(scale(wisc.data), center = 2, nstart = 20)

# Compare k-means to actual diagnoses
table(wisc.km$cluster, diagnosis)

# Compare k-means to hierarchical clustering
table(wisc.km$cluster, wisc.hclust.clusters)


# Create a hierarchical clustering model: wisc.pr.hclust
wisc.pr.hclust <- hclust(dist(wisc.pr$x[, 1:7]), method = "complete")

# Cut model into 4 clusters: wisc.pr.hclust.clusters
wisc.pr.hclust.clusters <- cutree(wisc.pr.hclust, k = 4)

# Compare to actual diagnoses
table(diagnosis, wisc.pr.hclust.clusters)

# Compare to k-means and hierarchical
table(diagnosis, wisc.hclust.clusters)
table(diagnosis, wisc.km$cluster)
