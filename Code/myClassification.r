####### myClassification.r file for Ben Rodgers (42006945) ########


### 3.1 Load the data and divide the dataset into training and test

#Read file
classification_cancer <- readRDS(file="./data/bcw_processed.Rda")
set.seed(6945)
m = nrow(classification_cancer)
training_percentage = 0.7
test_percentage = 0.3
ind <- sample(2, m, replace = TRUE, prob = c(training_percentage, test_percentage))

# Divide the dataset 
training_data = classification_cancer[ind == 1, ]
test_data = classification_cancer[ind == 2, ]

### 3.2 Learn a classification tree from the training data using the default parameters of the ctree function from the "party" library
#-> Analysis in report

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Install library
install.packages("party")
library(party)

# Make formula
myFormula <- Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape +
  Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses

# Make classification tree
cancer_ctree <- ctree(myFormula, data = training_data)

# Plot tree
png('./Plot/3.2 Classifciation Tree - Default parameters', width = 900, height = 700)
plot(cancer_ctree)
dev.off()

# Plot tree

png('./Plot/3.2 Classifciation Tree - Default parameters - Simple plot', width = 900, height = 700)
plot(cancer_ctree, type="simple")
dev.off()

# Get the predicted class labels for the test data
cancer_ctree_pred <-predict(cancer_ctree, newdata = test_features)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = cancer_ctree_pred))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get the result
results <- data.frame(precision, recall, f1)
accuracy 
results


### 3.3 Adjust the settings to the default

# Set seed
set.seed(6945)

# Make sample
m = nrow(classification_cancer)
training_percentage = 0.7
test_percentage = 0.3
ind <- sample(2, m, replace = TRUE, prob = c(training_percentage, test_percentage))

# Get sample data
training_data = classification_cancer[ind == 1, ]
test_data = classification_cancer[ind == 2, ]

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Use a formula
myFormula <- Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape +
  Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses

# Make the classification tree
cancer_ctree2 <- ctree(myFormula, data = training_data, control = ctree_control(mincriterion = 0.5))

png('./Plot/3.3 Classifciation Tree - ctree_control adjusted', width = 1100, height = 700)
plot(cancer_ctree2)
dev.off()

png('./Plot/3.3 Classifciation Tree - ctree_control adjusted - Simple plot', width = 1100, height = 700)
plot(cancer_ctree2, type="simple")
dev.off()

# Calculate the accuracy, precision and recall
cancer_ctree_pred2 <-predict(cancer_ctree2, newdata = test_features)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = cancer_ctree_pred2))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get results
results <- data.frame(precision, recall)
accuracy 
results


### 3.4 K-NN classification

### K=1
# Install class package
install.packages("class")
library(class)

#Set seed 
set.seed(6945)

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Make the prediction based on the k-nearest neighbour
knn_pred1 <-knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 1)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = knn_pred1))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

# Calculate accuracy, precision and recall
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

#Get results
results <- data.frame(precision, recall)
accuracy 
results

###K=2 

#Set seed 
set.seed(6945)

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Make the prediction based on the k-nearest neighbour
knn_pred2 <-knn(train = training_features,
                test = test_features,
                cl = training_labels,
                k = 2)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = knn_pred2))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

# Calculate accuracy, precision and recall
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get the result
results <- data.frame(precision, recall)
accuracy 
results

### K = 3

#set seed
set.seed(6945)

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Make the prediction based on the k-nearest neighbour
knn_pred3 <-knn(train = training_features,
               test = test_features,
               cl = training_labels,
               k = 3)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = knn_pred3))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

# Calculate accuracy, precision and recall
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get the result
results <- data.frame(precision, recall)
accuracy 
results

### K = 4

#set seed
set.seed(6945)

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Make the prediction based on the k-nearest neighbour
knn_pred4 <-knn(train = training_features,
               test = test_features,
               cl = training_labels,
               k = 4)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = knn_pred4))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

# Calculate accuracy, precision and recall
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get the result
results <- data.frame(precision, recall)
accuracy 
results

### K = 5

#set seed
set.seed(6945)

# Partition class label for training and testing
training_features <-training_data[,1:9]
training_labels <-training_data[,10]
test_features <- test_data[,1:9]
test_labels <- test_data[,10]

# Make the prediction based on the k-nearest neighbour
knn_pred5 <-knn(train = training_features,
               test = test_features,
               cl = training_labels,
               k = 5)

# Create the confusion matrix
cm = as.matrix(table(Actual = test_labels, Predicted = knn_pred5))
n = sum(cm)
nc = nrow(cm)
diag = diag(cm)
rowsums = apply(cm, 1, sum)
colsums = apply(cm, 2, sum)

# Calculate accuracy, precision and recall
accuracy = sum(diag) /n
precision = diag / colsums
recall = diag/rowsums

# Get the result
results <- data.frame(precision, recall)
accuracy 
results

