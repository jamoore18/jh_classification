# JM 9/22/17
# main_script.R - wrapper script for modelling PCA and adbundance data
# Soapberry bug classification

# ISSUE: this script is not deterministic

source("/Users/Jay/files/wingClassifier/scripts/utility_scripts.R")
source("/Users/Jay/files/wingClassifier/scripts/analysis_scripts.R")

wing_data.path <- "/Users/Jay/files/wingClassifier/DGE.170815/Jhae.wingPC1_10.dataset.161019.csv"

wing_data.df <- safe_read(wing_data.path)

wing_pca.df <- wing_data.df[,c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10")]

cluster_scores <- k_means.btstrp(wing_pca.df, verbose = T)
cluster_scores

rss <- cluster_scores;
p <- 10;
n <- length(rss)

f <- (rss[1] - rss[2:p])/(c(2:p)-1) / (rss[2:p]/(n-c(2:p)));
plot(f)
if(verbose == TRUE){
    plot(f, cex=1, main="F scores");
}
# find optimum number of clusters
for(idx in 1:length(f)){
    if(f[idx] > f[idx+1]){
        k <- idx;
    }
    else if (f[idx] < f[idx+1]){
        break
    }
}
print(f)
print(k)
return(k);


optimum_k <- F_test_clusters(cluster_scores, verbose = T)

# ANOVA F-test wiki
# parsimony - statistics
# for the p-value scores get better after 2
# best model that fits the data least
# F distribution - need to find the 95 quantile
val <- qf(.95, 1, 185)

value <- 1 - pf(val, 1, 185)
value

# How many componenents do we want to include?
# How many means are there?

vars <- apply(wing_pca.df, 2, var)
plot(log(vars))

# follow up with another F test
# cross validation of which one produced the best
# fit model on 80% data - on rest add up ss of error and find which mean it is closest to, find the distance of it to the mean, square it
# do that for the 20% of observations
# not sure if F test is an appropriate test
# cross validation is a better approach
# F test our observation are normally, this is not guarenteed here
# Each cluster is an normally 