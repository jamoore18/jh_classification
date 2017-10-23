# Jay Moore 9/13/17
# Wing Classifier with k-means bootstrap clusterting

wing_input <- read.csv(file="/Users/Jay/files/wingClassifier/DGE.170815/Jhae.wingPC1_10.dataset.161019.csv", header=TRUE, sep=",")
str(wing_pca)
wing_pca <- wing_input[c("PC1")]

cov(wing_pca)

cluster_results <- NULL
n <-(nrow(wing_pca))
for (i in (1:10)){
    wing_cluster <- kmeans(wing_pca, centers=i, iter.max = 100)
    cluster_results[i] <- wing_cluster$tot.withinss
}
wing_cluster
# plot(diag(cov(wing_pca)))
# plot(log(diag(cov(wing_pca))))
cluster_results

# You can use the F statistic when deciding to support or reject the null hypothesis
# The F statistic must be used in combination with the p value when you are deciding if your overall results are significant.
# It has a minimum value of zero; there is no maximum value.
# If your observed value of F is larger than the value in the F table, then you can reject the null hypothesis 
# with 95 percent confidence that the variance between your two populations isn’t due to random chance.
rss <- cluster_results
p <- length(rss)
# F <- (rss[1:(p-1)] - rss[2:p]) / (rss[2:p]/(n-c(2:p)))
F <- (rss[1:(p-1)] - rss[2:p])/(2) / (rss[2:p]/(n-2*c(2:p)))

F
plot(F, cex=1)

pf(F, df1=1, df2= n-p, lower.tail = FALSE)

