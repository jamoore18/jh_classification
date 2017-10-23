# JM 9/22/17
# Analysis scripts
# require(caret)

k_means.btstrp <- function(dataframe, verbose, n = (nrow(dataframe)-1)){
    
    cluster_results <- NULL
    
    for (i in (1:n)){
        i.cluster <- kmeans(dataframe, centers=i, iter.max = 10000)
        cluster_results[i] <- i.cluster$tot.withinss
    }
    if (verbose == TRUE){
    plot(cluster_results, main="Bootstrap K-means total within SSD", xlab="Clusters", ylab="SSD")
    }
    return(cluster_results);
}


F_test_clusters <- function(cluster_results, verbose){
    rss <- cluster_results;
    p <- length(rss);
    f <- (rss[1:(p-1)] - rss[2:p])/(2) / (rss[2:p]/(n-2*c(2:p)));
    
    
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
    
}
