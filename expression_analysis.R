# Parse and analysis kallisto abundance data

source("/Users/Jay/files/wingClassifier/scripts/utility_scripts.R")
source("/Users/Jay/files/wingClassifier/scripts/analysis_scripts.R")

wing_data.path <- "/Users/Jay/files/wingClassifier/DGE.170815/dt.vs.GU.CDS.kallisto.gene.abundance.csv"

wing_data.df <- safe_read(wing_data.path)

# log transform 

# wing_data.df <- log(wing_data.df[, 1:12])


# wing_pca.df <- prcomp(wing_data.df, center = TRUE, scale. = TRUE) 

library(devtools)
library(ggfortify)


wing_data.df <- as.data.frame(t(wing_data.df))
wing_data.df <- cbind(morph_id=NA, wing_data.df)
wing_data.df <- cbind(sex_id=NA, wing_data.df)

morph_pattern <- c("[L]","[S]")
sex_pattern <- c("[f]","[m]")

set_id_vals <- function(data, header, pattern){
    for (idx in 1:length(pattern)){
        i <- grep(pattern[idx], row.names(data))
        data[i,header] <- idx
        print(data[i,header])
    }
    return(data)
}

wing_data.df <- set_id_vals(wing_data.df,"morph_id", morph_pattern)
wing_data.df <- set_id_vals(wing_data.df,"sex_id", sex_pattern)

wing_data.df[,3:100]

a <- prcomp(wing_data.df[,3:100], center = TRUE)
b <- a$rotation
c <- as.data.frame(b)



ggplot2::autoplot()

plot(c$PC1,c$PC2)

ggplot(c, aes(x=c$PC1,y=c$PC2))


#############################################

cluster_scores <- k_means.btstrp(wing_pca.df, verbose = T, n = 20)
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
