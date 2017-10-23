# Jay Moore 9/13/17
# Wing Classifier with hierarchical clustering

wing_input <- read.csv(file="/Users/Jay/files/wingClassifier/Jhae.wingPC1_10.dataset.161019.csv", header=TRUE, sep=",")
str(wing_pca)
wing_pca <- wing_input[c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")]

hc = hclust(dist(wing_pca), method = 'ward.D2')
str(hc)
par(cex=0.3)
plot(hc, axes=F,xlab='', ylab='',sub ='', main='Comp 1/2')


rect.hclust(hc, k=3, border='red')
