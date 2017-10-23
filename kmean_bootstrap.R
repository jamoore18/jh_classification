# Jay Moore 9/13/17
# Wing Classifier with k-means bootstrap clusterting

wing_input <- read.csv(file="/Users/Jay/files/wingClassifier/DGE.170815/Jhae.wingPC1_10.dataset.161019.csv", header=TRUE, sep=",")
wing_input
wing_pca <- wing_input[c("PC1","PC2")]
wing_pca
plot(wing_pca$PC1,wing_pca$PC2)

cluster_results <- NULL
print(nrow(wing_pca))
for (i in (1:180)){
    wing_cluster <- kmeans(wing_pca, centers=i, iter.max = 100)
    cluster_results[i] <- wing_cluster$tot.withinss
}

wing_cluster

plot(cluster_results, main="Bootstrap K-means total within SSD", 
     xlab="Clusters", ylab="SSD")



optimum_k <- kmeans(wing_pca, centers=3, iter.max = 100)


plot(wing_pca$PC1,wing_pca$PC2)

# tmp <- cbind(wing_input, optimum_k$cluster)
# tmp <- cbind(tmp, optimum_k$centers[optimum_k$cluster])
# tmp
# write.csv(tmp,"/Users/Jay/files/wingClassifier/DGE.170815/Jhae.wingPC1_10.modified.csv")

data_with_PC_var <- read.csv(file="/Users/Jay/files/wingClassifier/DGE.170815/Jhae.wingPC1_10.modified.csv", stringsAsFactors=TRUE,header=TRUE, sep=",")

attach(data_with_PC_var)

n <- dim(data_with_PC_var)[1]

levels(as.factor(cluster))

cluster_identity <- NULL

for (i in levels(as.factor(cluster))){
    for (j in levels(as.factor(morph))){
        output <- subset(data_with_PC_var, subset = (morph == j & cluster==i))
        cluster_identity <- rbind(cluster_identity, c(j, i, dim(output)[1], dim(output)[1]/n))
        
    }
}
dim(cluster_identity)
cluster_identity

#square variance
hist(PC1_var)

within_1_sd <- data_with_PC_var[(abs(PC1_var - mean(PC1_var)) < sd(PC1_var)),]
hist(within_1_sd$PC1)

plot(within_1_sd$PC1, within_1_sd$PC2) # samples within tighter distrib (1 std dev)

length(within_1_sd$specimenID)
