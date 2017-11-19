<H1>The Confusion Matrix</H1>

# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Have a look at the structure of titanic
str(titanic)

# A decision tree classification model is built on the data
tree <- rpart(Survived ~ ., data = titanic, method = "class")

# Use the predict() method to make predictions, assign to pred
pred<-predict(tree, titanic, type = "class")

# Use the table() method to make the confusion matrix
table(titanic$Survived, pred)

<H1>Deriving ratios from the Confusion Matrix</H1>
# The confusion matrix is available in your workspace as conf

# Assign TP, FN, FP and TN using conf
TP <- conf[1, 1] # this will be 212
FN <- conf[1, 2] # this will be 78
FP <- conf[2, 1]  # fill in
TN <- conf[2, 2] # fill in

# Calculate and print the accuracy: acc
acc<-(TP + TN)/(TP + FN +  FP + TN)
acc

# Calculate and print out the precision: prec
prec <- TP / (TP + FP)
prec

# Calculate and print out the recall: rec
rec <- TP / (TP + FN)
rec


<H1>The quality of a regression RMSE</H1>
# if truth$colwas a column with true values of a variable and pred is the prediction of that variable, the formula could be calculated in R as follows:
sqrt((1/nrow(truth)) * sum( (truth$col - pred) ^ 2))

# The air dataset is already loaded into your workspace

# Take a look at the structure of air
str(air)

# Inspect your colleague's code to build the model
fit <- lm(dec ~ freq + angle + ch_length, data = air)

# Use the model to predict for all values: pred
pred <- predict(fit)

# Use air$dec and pred to calculate the RMSE 
rmse <- sqrt((1/nrow(air)) * sum( (air$dec - pred) ^ 2))

# Print out rmse
rmse

<H1>Adding complexity to increase quality</H1>
# The air dataset is already loaded into your workspace

# Previous model
fit <- lm(dec ~ freq + angle + ch_length, data = air)
pred <- predict(fit)
rmse <- sqrt(sum( (air$dec - pred) ^ 2) / nrow(air))
rmse

# Your colleague's more complex model
fit2 <- lm(dec ~ freq + angle + ch_length + velocity + thickness, data = air)

# Use the model to predict for all values: pred2
pred2 <- predict(fit2)

# Calculate rmse2
rmse2 <- sqrt(sum( (air$dec - pred2) ^ 2) / nrow(air))

# Print out rmse2
rmse2

<H1>Lets do some clustering!</H1>
# The seeds dataset is already loaded into your workspace

# Set random seed. Don't remove this line
set.seed(1)

# Explore the structure of the dataset
str(seeds)

# Group the seeds in three clusters
km_seeds <- kmeans(seeds, 3)

# Color the points in the plot based on the clusters
plot(length ~ compactness, data = seeds, col = km_seeds$cluster)

# Print out the ratio of the WSS to the BSS
km_seeds$tot.withinss / km_seeds$betweenss


<H1>Split the sets</H1>
# For example, you could use the following commands to shuffle a data frame df and divide it into training and test sets with a 60/40 split between the two.
n <- nrow(df)
shuffled_df <- df[sample(n), ]
train_indices <- 1:round(0.6 * n)
train <- shuffled_df[train_indices, ]
test_indices <- (round(0.6 * n) + 1):n
test <- shuffled_df[test_indices, ]


# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset, call the result shuffled
n <- nrow(titanic)
shuffled <- titanic[sample(n),]

# Split the data in train and test
train_indices <- 1:round(0.7 * n)
train <- shuffled[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled[test_indices, ]

# Print the structure of train and test
str(train)
str(test)

<H1>First you train, then you test</H1>
# The titanic dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Shuffle the dataset; build train and test
n <- nrow(titanic)
shuffled <- titanic[sample(n),]
train <- shuffled[1:round(0.7 * n),]
test <- shuffled[(round(0.7 * n) + 1):n,]

# Fill in the model that has been learned.
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the outcome on the test set with tree: pred
pred <- predict(tree, test, type = "class")

# Calculate the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print this confusion matrix
conf

<H1>Using Cross Validation</H1>
# The shuffled dataset is already loaded into your workspace

# Set random seed. Don't remove this line.
set.seed(1)

# Initialize the accs vector
accs <- rep(0,6)

for (i in 1:6) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/6)*nrow(shuffled))) + 1):((i*round((1/6) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  tree <- rpart(Survived ~ ., train, method = "class")
  
  # Make a prediction on the test set using tree
  pred <- predict(tree, test, type = "class")
  
  # Assign the confusion matrix to conf
  conf <- table(test$Survived, pred)
  
  # Assign the accuracy of this model to the ith index in accs
  accs[i] <- sum(diag(conf))/sum(conf)
}

# Print out the mean of accs
mean(accs)

<H1>Overfitting the spam!</H1>
# The spam filter that has been 'learned' for you
spam_classifier <- function(x){
  prediction <- rep(NA, length(x)) # initialize prediction vector
  prediction[x > 4] <- 1 
  prediction[x >= 3 & x <= 4] <- 0
  prediction[x >= 2.2 & x < 3] <- 1
  prediction[x >= 1.4 & x < 2.2] <- 0
  prediction[x > 1.25 & x < 1.4] <- 1
  prediction[x <= 1.25] <- 0
  return(factor(prediction, levels = c("1", "0"))) # prediction is either 0 or 1
}

# Apply spam_classifier to emails_full: pred_full
pred_full <- spam_classifier(emails_full$avg_capital_seq)

# Build confusion matrix for emails_full: conf_full
conf_full <- table(emails_full$spam, pred_full)

# Calculate the accuracy with conf_full: acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print acc_full
acc_full

<H1>Increasing the bias</H1>
# The all-knowing classifier that has been learned for you
# You should change the code of the classifier, simplifying it
spam_classifier <- function(x){
  prediction <- rep(NA, length(x))
  prediction[x > 4] <- 1
  prediction[x <= 4] <- 0
  return(factor(prediction, levels = c("1", "0")))
}

# conf_small and acc_small have been calculated for you
conf_small <- table(emails_small$spam, spam_classifier(emails_small$avg_capital_seq))
acc_small <- sum(diag(conf_small)) / sum(conf_small)
acc_small

# Apply spam_classifier to emails_full and calculate the confusion matrix: conf_full
pred_full <- spam_classifier(emails_full$avg_capital_seq)
conf_full <- table(emails_full$spam, pred_full)

# Calculate acc_full
acc_full <- sum(diag(conf_full)) / sum(conf_full)

# Print acc_full
acc_full



<H1>Classify with the decision tree</H1>
# The train and test set are loaded into your workspace.

# Code from previous exercise
# The train and test set are loaded into your workspace.

# Code from previous exercise
set.seed(1)
library(rpart)
tree <- rpart(Survived ~ ., train, method = "class")

# Predict the values of the test set: pred
pred <- predict(tree, test, type = "class")

# Construct the confusion matrix: conf
conf <- table(test$Survived, pred)

# Print out the accuracy
print(sum(diag(conf)) / sum(conf))


<H1>Pruning the tree</H1>
# All packages are pre-loaded, as is the data

# Calculation of a complex tree
set.seed(1)
tree <- rpart(Survived ~ ., train, method = "class", control = rpart.control(cp=0.00001))

# Draw the complex tree
fancyRpartPlot(tree)

# Prune the tree: pruned
pruned <- prune(tree, 0.01)

# Draw pruned
fancyRpartPlot(pruned)


<H1>Splitting criterion</H1>
In this exercise, you'll build two decision trees based on different splitting criteria. In the video you've learned about information gain: the higher the gain when you split, the better. However, the standard splitting criterion of rpart() is the Gini impurity.

# All packages, emails, train, and test have been pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Train and test tree with gini criterion
tree_g <- rpart(spam ~ ., train, method = "class")
pred_g <- predict(tree_g, test, type = "class")
conf_g <- table(test$spam, pred_g)
acc_g <- sum(diag(conf_g)) / sum(conf_g)

# Change the first line of code to use information gain as splitting criterion
tree_i <- rpart(spam ~ ., train, method = "class", parms = list(split = "information"))
pred_i <- predict(tree_i, test, type = "class")
conf_i <- table(test$spam, pred_i)
acc_i <- sum(diag(conf_i)) / sum(conf_i)

# Draw a fancy plot of both tree_g and tree_i
fancyRpartPlot(tree_g)
fancyRpartPlot(tree_i)

# Print out acc_g and acc_i
print(acc_g)
print(acc_i)

<H1>Preprocess the data - Rescale</H1>
For example, to normalize a vector x, you could do the following:
x-min(x) / max(x) - min(x)

# train and test are pre-loaded

# Store the Survived column of train and test in train_labels and test_labels
train_labels <- train$Survived
test_labels <- test$Survived

# Copy train and test to knn_train and knn_test
knn_train <- train 
knn_test <- test

# Drop Survived column for knn_train and knn_test
knn_train$Survived <- NULL
knn_test$Survived <- NULL


# Normalize Pclass
min_class <- min(knn_train$Pclass)
max_class <- max(knn_train$Pclass)
knn_train$Pclass <- (knn_train$Pclass - min_class) / (max_class - min_class)
knn_test$Pclass <- (knn_test$Pclass - min_class) / (max_class - min_class)

# Normalize Age
min_age <- min(knn_train$Age)
max_age <- max(knn_train$Age)
knn_train$Age <- (knn_train$Age - min_age) / (max_age - min_age)
knn_test$Age <- (knn_test$Age - min_age) / (max_age - min_age)

<H1>The knn() function</H1>
        # knn_train, knn_test, train_labels and test_labels are pre-loaded
        
        # Set random seed. Don't remove this line.
        set.seed(1)

# Load the class package
library(class)

# Fill in the ___, make predictions using knn: pred
pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = 5)

# Construct the confusion matrix: conf
conf <- table (test_labels, pred)

# Print out the confusion matrix
print(conf)

<H1>K's choice</H1>
The range, a vector of K values to try, and an accs vector to store the accuracies for these different values, have already been created. You don't have to write extra code for this step.

# knn_train, knn_test, train_labels and test_labels are pre-loaded

# Set random seed. Don't remove this line.
set.seed(1)

# Load the class package, define range and accs
library(class)
range <- 1:round(0.2 * nrow(knn_train))
accs <- rep(0, length(range))

for (k in range) {
        
        # Fill in the ___, make predictions using knn: pred
        pred <- knn(train = knn_train, test = knn_test, cl = train_labels, k = k)
        
        # Fill in the ___, construct the confusion matrix: conf
        conf <- table(test_labels, pred)
        
        # Fill in the ___, calculate the accuracy and store it in accs[k]
        accs[k] <- sum(diag(conf)) / sum(conf)
}

# Plot the accuracies. Title of x-axis is "k".
plot(range, accs, xlab = "k")

# Calculate the best k
which.max(accs)


<H1>Creating the ROC curve (1)</H1>
        # train and test are pre-loaded
        
        # Set random seed. Don't remove this line
        set.seed(1)

# Build a tree on the training set: tree
tree <- rpart(income ~ ., train, method = "class")

# Predict probability values using the model: all_probs
all_probs <- predict(tree, test, type = "prob")

# Print out all_probs
print(all_probs)

# Select second column of all_probs: probs
probs <- all_probs[,2]

<H1>Creating the ROC curve (2)</H1>
        # train and test are pre-loaded
        
        # Code of previous exercise
        set.seed(1)
tree <- rpart(income ~ ., train, method = "class")
probs <- predict(tree, test, type = "prob")[,2]

# Load the ROCR library
library(ROCR)

# Make a prediction object: pred
pred <- prediction( probs, test$income)

# Make a performance object: perf
perf <- performance(pred, "tpr", "fpr")

# Plot this curve
plot(perf)

<H1>The area under the curve</H1>
# test and train are loaded into your workspace
        
# Build tree and predict probability values for the test set
set.seed(1)
tree <- rpart(income ~ ., train, method = "class")
probs <- predict(tree, test, type = "prob")[,2]

# Load the ROCR library
library(ROCR)

# Make a prediction object: pred
pred <- prediction( probs, test$income)

# Make a performance object: perf
perf <- performance(pred, "auc")

# Print out the AUC
print(perf@y.values[[1]])

<H1>Comparing the methods</H1>
        # Load the ROCR library
        library(ROCR)

# Make the prediction objects for both models: pred_t, pred_k
pred_t <- prediction( probs_t, test$spam)
pred_k <- prediction( probs_k, test$spam)

# Make the performance objects for both models: perf_t, perf_k
perf_t <- performance(pred_t, "tpr", "fpr")
perf_k <- performance(pred_k, "tpr", "fpr")


# Draw the ROC lines using draw_roc_lines()
draw_roc_lines(perf_t, perf_k)



