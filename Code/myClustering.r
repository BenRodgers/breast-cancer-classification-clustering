####### myClustering.r file for Ben Rodgers (42006945) ########


### 2.1 Load  preprocessed data 
clustering_cancer <- readRDS(file="./data/bcw_processed.Rda")
#Remove the class label
clustering_cancer2 <- clustering_cancer
clustering_cancer2$Class <- NULL

### 2.2 Cluster the data into 2 clusters using K-Means clustering and plot
set.seed(6945)
# Specify number of clusters
nclust = 2
# Perform k-means cluster
(kmeans.result <- kmeans(clustering_cancer2, nclust))
png('./Plot/2.2 K = 2 Means Cluster - Default parameters', width = 800, height = 600)
# Make plot
plot(clustering_cancer2[, c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = kmeans.result$cluster)
title(paste("k = 2"))
dev.off()


### 2.3. Plot according to the Class column
png('./Plot/2.3 K = 2 Means Cluster - Class Column', width = 800, height = 600)
# Make plot
plot(clustering_cancer[, c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = clustering_cancer$Class)
title(paste("k = 2 - Using Class column"))
dev.off()

### 2.4. Compare the 2 plots obtained in the previous two tasks
# -> Answers in report

### 2.5. Cluster the data into more than 2 clusters (i.e., k = 3, 4, 5) 

### 3 clusters
set.seed(6945)
nclust = 3
(kmeans3.result <- kmeans(clustering_cancer2, nclust))
png('./Plot/2.5 K = 3 Means Cluster - Default parameters', width = 800, height = 600)
plot(clustering_cancer2[, c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = kmeans3.result$cluster)
title(paste("k = 3"))
dev.off()

### 4 clusters
set.seed(6945)
nclust = 4
(kmeans4.result <- kmeans(clustering_cancer2, nclust))
png('./Plot/2.5 K = 4 Means Cluster - Default parameters', width = 800, height = 600)
plot(clustering_cancer2[, c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = kmeans4.result$cluster)
title(paste("k = 4"))
dev.off()

### 5 clusters
set.seed(6945)
nclust = 5
(kmeans5.result <- kmeans(clustering_cancer2, nclust))
png('./Plot/2.5 K = 5 Means Cluster - Default parameters', width = 800, height = 600)
plot(clustering_cancer2[, c("Clump.Thickness", "Uniformity.of.Cell.Size")], col = kmeans5.result$cluster)
title(paste("k = 5"))
dev.off()

### 2.6. Compare the plots and SSEs obtained in the previous task, and provide your comments on the quality of clustering. 
# -> Analysis in report

### 2.7. Apply hierarchical clustering to the data  plot. 
### Particularly, cluster the dendrogram into 2, 3, 4, and 5 clusters and plot all of them. 

### Default clustering
### 2 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer2)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample))
png('./Plot/2.7 K = 2 Means Cluster Dendogram - Default parameters', width = 800, height = 600)
plot(hc, hang = -1)
nclust =2
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 3 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer2)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample))
png('./Plot/2.7 K = 3 Means Cluster Dendogram - Default parameters', width = 800, height = 600)
plot(hc, hang = -1)
nclust =3
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 4 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample))
png('./Plot/2.7 K = 4 Means Cluster Dendogram - Default parameters', width = 800, height = 600)
plot(hc, hang = -1)
nclust =4
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 5 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample))
png('./Plot/2.7 K = 5 Means Cluster Dendogram - Default parameters', width = 800, height = 600)
plot(hc, hang = -1)
nclust =5
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 2.8 Compare the plots 
#-> Analysis in the report

### 2.9 Single and Complete Dendograms
#-> Analysis in the report

### Single
### 2 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="single")
png('./Plot/2.9 K = 2 Means Cluster Dendogram - Single', width = 800, height = 600)
plot(hc, hang = -1)
nclust =2
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 3 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="single")
png('./Plot/2.9 K = 3 Means Cluster Dendogram - Single', width = 800, height = 600)
plot(hc, hang = -1)
nclust =3
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 4 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="single")
png('./Plot/2.9 K = 4 Means Cluster Dendogram - Single', width = 800, height = 600)
plot(hc, hang = -1)
nclust =4
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 5 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="single")
png('./Plot/2.9 K = 5 Means Cluster Dendogram - Single', width = 800, height = 600)
plot(hc, hang = -1)
nclust =5
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### Complete
### 2 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="complete")
png('./Plot/2.9 K = 2 Means Cluster Dendogram - Complete', width = 800, height = 600)
plot(hc, hang = -1)
nclust =2
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 3 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="complete")
png('./Plot/2.9 K = 3 Means Cluster Dendogram - Complete', width = 800, height = 600)
plot(hc, hang = -1)
nclust =3
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 4 cluster denodgram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="complete")
png('./Plot/2.9 K = 4 Means Cluster Dendogram - Complete', width = 800, height = 600)
plot(hc, hang = -1)
nclust =4
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 5 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="complete")
png('./Plot/2.9 K = 5 Means Cluster Dendogram - Complete', width = 800, height = 600)
plot(hc, hang = -1)
nclust =5
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

###Average
### 2 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer2)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="ave")
png('./Plot/2.9 K = 2 Means Cluster Dendogram - Average', width = 800, height = 600)
plot(hc, hang = -1)
nclust =2
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 3 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer2)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="ave")
png('./Plot/2.9 K = 3 Means Cluster Dendogram - Average', width = 800, height = 600)
plot(hc, hang = -1)
nclust =3
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 4 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="ave")
png('./Plot/2.9 K = 4 Means Cluster Dendogram - Average', width = 800, height = 600)
plot(hc, hang = -1)
nclust =4
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()

### 5 cluster dendogram
set.seed(6945)
n = nrow(clustering_cancer)
idx <- sample(1:n)
cancerSample <-clustering_cancer2[idx,]
hc <- hclust(dist(cancerSample), method="ave")
png('./Plot/2.9 K = 5 Means Cluster Dendogram - Average', width = 800, height = 600)
plot(hc, hang = -1)
nclust =5
rect.hclust(hc, k=nclust)
groups <-cutree(hc, k=nclust)
dev.off()
