### k-means: how well did you do earlier?
# seeds and seeds_type are pre-loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(100)

# Do k-means clustering with three clusters, repeat 20 times: seeds_km
seeds_km <- kmeans(seeds, nstart = 20, centers = 3)

# Print out seeds_km
print(seeds_km)

# Compare clusters with actual seed types. Set k-means clusters as rows
table(seeds_km$cluster, seeds_type) 


# Plot the length as function of width. Color by cluster
plot(seeds$width, seeds$length, col = seeds_km$cluster)

### The influence of starting centroids
# seeds is pre-loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(100)

# Apply kmeans to seeds twice: seeds_km_1 and seeds_km_2
seeds_km_1 <- kmeans(seeds, nstart = 1, centers = 5)
seeds_km_2 <- kmeans(seeds, nstart = 1, centers = 5)

# Return the ratio of the within cluster sum of squares
seeds_km_1$tot.withinss/seeds_km_2$tot.withinss

# Compare the resulting clusters
table(seeds_km_1$cluster, seeds_km_2$cluster)

#Well done! As you can see, some clusters remained the same, others have changed. For example, cluster 5 from seeds_km_1 completely contains cluster 1 from seeds_km_2 (33 objects). Cluster 4 from seeds_km_1 is split, 18 objects were put in seeds_km_2's fourth cluster and 41 in its fifth cluster. For consistent and decent results, you should set nstart > 1 or determine a prior estimation of your centroids.

### Making a scree plot!
# Source: https://cran.r-project.org/web/packages/cluster.datasets/cluster.datasets.pdf

# The dataset school_result is pre-loaded

# Set random seed. Don't remove this line.
set.seed(100)

# Explore the structure of your data
str(school_result)

# Initialise ratio_ss 
ratio_ss <- rep(0, 7)

# Finish the for-loop. 
for (k in 1:7) {
        
        # Apply k-means to school_result: school_km
        school_km <- kmeans(school_result, nstart = 20, centers = k)
        
        # Save the ratio between of WSS to TSS in kth element of ratio_ss
        ratio_ss[k] <- school_km$tot.withinss / school_km$totss
}

# Make a scree plot with type "b" and xlab "k"
plot(ratio_ss, type = "b", xlab = "k")

### Standardized vs non-standardized clustering (1)
# The dataset run_record has been loaded in your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Explore your data with str() and summary()
str(run_record)
summary(run_record)

# Cluster run_record using k-means: run_km. 5 clusters, repeat 20 times
run_km <- kmeans(run_record, nstart = 20, centers = 5)

# Plot the 100m as function of the marathon. Color using clusters
plot(run_record$marathon, run_record$X100m,  col=run_km$cluster)

# Calculate Dunn's index: dunn_km. Print it.
dunn_km <- dunn(clusters = run_km$cluster, Data = run_record)
print(dunn_km)

### Standardized vs non-standardized clustering (2)
# The dataset run_record as well as run_km are available

# Set random seed. Don't remove this line.
set.seed(1)

# Standardize run_record, transform to a dataframe: run_record_sc
run_record_sc <- as.data.frame(scale(run_record))

# Cluster run_record_sc using k-means: run_km_sc. 5 groups, let R start over 20 times
run_km_sc <- kmeans(run_record_sc, 5, nstart = 20)

# Plot records on 100m as function of the marathon. Color using the clusters in run_km_sc
plot(run_record$marathon, run_record$X100m, col = run_km_sc$cluster,
     xlab = "marathon", ylab ="100m", main = "Run Records")

# Compare the resulting clusters in a nice table
table(run_km$cluster, run_km_sc$cluster)

# Calculate Dunn's index: dunn_km_sc. Print it.
dunn_km_sc <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)
dunn_km_sc

### Single Hierarchical Clustering
# The dataset run_record_sc has been loaded in your workspace

# Apply dist() to run_record_sc: run_dist
run_dist <- dist(run_record_sc)

# Apply hclust() to run_dist: run_single
run_single <- hclust(run_dist, "single")

# Apply cutree() to run_single: memb_single
memb_single <- cutree(run_single, k = 5)

# Apply plot() on run_single to draw the dendrogram
plot(run_single)

# Apply rect.hclust() on run_single to draw the boxes
rect.hclust(run_single, k = 5, border = 2:6)
# Good job! However, it appears the two islands Samoa and Cook's Islands, who are not known for their sports performances, have both been placed in their own groups. Maybe, we're dealing with some chaining issues? Let's try a different linkage method in the next exercise!

### Complete Hierarchical Clustering

# run_record_sc is pre-loaded

# Code for single-linkage
run_dist <- dist(run_record_sc, method = "euclidean")
run_single <- hclust(run_dist, method = "single")
memb_single <- cutree(run_single, 5)
plot(run_single)
rect.hclust(run_single, k = 5, border = 2:6)

# Apply hclust() to run_dist: run_complete
run_complete <- hclust(run_dist, "complete")

# Apply cutree() to run_complete: memb_complete
memb_complete <- cutree(run_complete, k = 5)

# Apply plot() on run_complete to draw the dendrogram
plot(run_complete)

# Apply rect.hclust() on run_complete to draw the boxes
rect.hclust(run_complete, k = 5, border = 2:6)

# table() the clusters memb_single and memb_complete. Put memb_single in the rows
table(memb_single, memb_complete)

# Good job! Compare the two plots. The five clusters differ significantly from the single-linkage clusters. That one big cluster you had before, is now split up into 4 medium sized clusters. Have a look at the table you generated as well!

### Hierarchical vs k-means
# So you've clustered the countries based on their Olympic run performances using three different methods: k-means clustering, hierarchical clustering with single linkage and hierarchical clustering with complete linkage. You can ask yourself: which method returns the best separated and the most compact clusters?

# Let's use Dunn's index. Remember, it returns the ratio between the minimum intercluster distance to the maximum intracluster diameter. The dunn() function in R, requires the argument clusters, indicating the cluster partitioning, the Data and a method to determine the distance. In this case, that's "euclidean", which is the default.

# Your job is to calculate Dunn's index for all three clusterings and compare the clusters to each other. The R objects you calculated in the previous exercises are already available in your workspace.

#### Hierarchical vs k-means
# run_record_sc, run_km_sc, memb_single and memb_complete are pre-calculated

# Set random seed. Don't remove this line.
set.seed(100)

# Dunn's index for k-means: dunn_km
dunn_km <- dunn(clusters = run_km_sc$cluster, Data = run_record_sc)

# Dunn's index for single-linkage: dunn_single
dunn_single <- dunn(clusters = memb_single, Data = run_record_sc)

# Dunn's index for complete-linkage: dunn_complete
dunn_complete <- dunn(clusters = memb_complete, Data = run_record_sc)

# Compare k-means with single-linkage
table(run_km_sc$cluster, memb_single)

# Compare k-means with complete-linkage
table(run_km_sc$cluster, memb_complete)


### Clustering US states based on criminal activity
# You've seen that different clustering methods can return entirely different clusters, each with their own interpretation and uses. It's time to put your skills, both the programming and the interpretation, to the test!
        
# Your client has provided you with a dataset, crime_data, containing info on the crimes committed in each of the 50 US states and the percentage of urban population (Source: Edureka http://www.edureka.co/blog/implementing-kmeans-clustering-on-the-crime-dataset/). He'd like you to group the states in 4 clusters. He didn't specify which similarity to use, but the euclidean distance seems acceptable, don't you agree?

# You decide to try out two techniques: k-means and single-linkage hierarchical clustering. You then want to compare the results by calculating the Dunn's indices to make a conclusion. Which clustering will you deliver to your client?

# Set random seed. Don't remove this line.
set.seed(1)

# Scale the dataset: crime_data_sc
crime_data_sc <- scale(crime_data)


# Perform k-means clustering: crime_km
crime_km <- kmeans(crime_data_sc, centers = 4, nstart = 20)

# Perform single-linkage hierarchical clustering
## Calculate the distance matrix: dist_matrix
dist_matrix <- dist(crime_data_sc)

## Calculate the clusters using hclust(): crime_single
crime_single <- hclust(dist_matrix, "single")

## Cut the clusters using cutree: memb_single
memb_single <- cutree(crime_single, k = 4)

# Calculate the Dunn's index for both clusterings: dunn_km, dunn_single
dunn_km <- dunn(clusters = crime_km$cluster, Data = crime_data_sc)
dunn_single <- dunn(clusters = memb_single, Data = crime_data_sc)


# Print out the results
print(dunn_km)
print(dunn_single)
