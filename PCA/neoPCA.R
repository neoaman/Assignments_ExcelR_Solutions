setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\PCA")

df <- read.csv("wine.csv")
dim(df)
str(df)
head(df)
table(df$Type)

write.csv(summary(df),"summary.csv")
#____________________________________MY FUNCTIONS____________________________________
normalize_dummy <- function(x){
  col <- ncol(x)
  row <- nrow(x)
  y <- 1:nrow(x)
  for (i in 1:col){
    if(class(x[,i])=="numeric" | class(x[,i])=="integer")
    {
      minx <- min(x[,i])
      maxx <- max(x[,i])
      for(j in 1:row)
      {
        x[j,i] <- ifelse((x[j,i] - minx) != 0,yes =((x[j,i] - minx) / (maxx - minx)),no = 0)
      }
    }
    
  }
  f <- c()
  for(i in 1:ncol(x)){
    if(class(x[,i])=="factor"){
      dummies <- data.frame(dummies::dummy(x[,i]))
      y <- data.frame(y,dummies)
      f <- c(f,i)
    }
    else{
      next
    }
  }
  if(is.null(f)){
    output <- x
  }
  else{output <- data.frame(x[,-f],y[,-1])}
  return(output)
}
all_hclust <- function(dist,k=3,method="all"){
  method=c("single", "complete", "average", "mcquitty", "ward.D", "ward.D2", "centroid","median")
  row <- 0
  clust_df <<- as.data.frame(method,matrix(0,nrow = length(method),ncol = k))
  all_clus <- list()
  for(i in method){
    row <- row+1
    hcl <- hclust(d = dist,method = i)
    clusters <- cutree(tree = hcl,k = k)
    all_clus[[i]] <- clusters
    clust_df[row,2:(k+1)] <- as.integer(tabulate(clusters))
  }
  
  all_clus[["table"]] <- clust_df
  return(all_clus)
}

df_norm <- data.frame(Type=df$Type,normalize_dummy(df[,-1]))
head(df_norm)

#_______________________________________PCA_________________________________
# Performing PCA for this data
df_pca<-princomp(x = df_norm[,-1], cor = TRUE, scores = TRUE, covmat = NULL)
df_pca_2 <- df_pca$scores[,1:3]

#______________________________HIERARCHICAL CLUSTERING________________________________________________
# Perform Cluster analysis in original data
dist_O <-dist(df_norm,method = "euclidian")
clustlist_O <- all_hclust(dist = dist_O,k = 3)
clustlist_O$table # From table we can see that ward.D2,ward.D,single,mcquitty,average is giving best result among all
# Findout accuracies we can see
mean(clustlist_O$single==df_norm$Type);mean(clustlist_O$average==df_norm$Type);mean(clustlist_O$mcquitty==df_norm$Type);mean(clustlist_O$ward.D==df_norm$Type)
mean(clustlist_O$ward.D2==df_norm$Type)

# Performing Clustering on the PCA Data with 3 principal component (column)__________________
dist_2 <-dist(df_pca_2,method = "euclidian")
clustlist_2 <- all_hclust(dist = dist_2,k = 3)
clustlist_2$table # From table we can see that ward.D2,ward.D is giving best result among all
# Findout accuracies we can see
mean(clustlist_2$ward.D2==df_norm$Type) # accuracy is 0.9494382
table(clustlist_2$ward.D,df_norm$Type)
mean(clustlist_2$ward.D==df_norm$Type) #Accuracy is 0.9550562
table(clustlist_2$ward.D,df_norm$Type)

df[which(clustlist_2$ward.D!=df_norm$Type),]
df[which(clustlist_2$ward.D2!=df_norm$Type),]


#______________________________KMEANS CLUSTERING________________________________________________________
# Perform Kmeans in Original Data
Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(33) # seed = 33  ; I set this seed for constant plot for every iteration so please dont remove it
  km_a <- kmeans(x = df_norm[,-1],i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue") # I may consider 3 as my optimum k value

set.seed(33);km_o <- kmeans(x = df_norm[,-1],3)
table(km_o$cluster)
table(df_norm$Type)

table(KmeansClusterGroup = km_o$cluster,HierarchicalGroup=df_norm$Type)

length(union(which(km_o$cluster==2),which(df_norm$Type==1)))-length(intersect(which(km_o$cluster==2),which(df_norm$Type==1)))
# Except 6 observations from Kmeans group 2 all same as the cluster group 1 in Hierarchical clustering We may consider Kmeans cluster of group 2 as our group 1 cluster in hierarchical clustering.
length(union(which(km_o$cluster==1),which(df_norm$Type==3)))-length(intersect(which(km_o$cluster==1),which(df_norm$Type==3)))
# Except 3 observations from Kmeans group 1 all same as the cluster group 3 in Hierarchical clustering We may consider Kmeans cluster of group 1 as our group 3 cluster in hierarchical clustering.
length(union(which(km_o$cluster==3),which(df_norm$Type==2)))-length(intersect(which(km_o$cluster==3),which(df_norm$Type==2)))
# Except 9 observations from Kmeans group 3 all same as the cluster group 2 in Hierarchical clustering We may consider Kmeans cluster of group 3 as our group 2 cluster in hierarchical clustering.

# Question may aries : why I am always considering the original cluster data in this whole script as my Hirearchical clustering result, Cause Its really is the same type. with 100% of same result using ward.D, ward.D2 linkage.

# It seems like Clustering is good but allocation of number is differing only
clusto1 <- ifelse(km_o$cluster == 1,3,ifelse(km_o$cluster==2,1,2))

mean(clusto1==df_norm$Type) # 0.9494382
table(Kmeans_Groups = clusto1,Hierarchical_groups=df_norm$Type)
# Perform Kmeans in PCA Data________________________________
Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(33) # seed = 33  ; I set this seed for constant plot for every iteration so please dont remove it
  km_a <- kmeans(x = df_pca_2,i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue") # I may consider 3 as my optimum k value

set.seed(33);km_PCA <- kmeans(x = df_pca_2,3)
table(km_PCA$cluster)
table(df_norm$Type)
table(KmeansPCA=km_PCA$cluster,HierarchicalOriginal=df_norm$Type)
table(KmeansPCA=km_PCA$cluster,KMeansOriginal=df_norm$Type)

# It seems like the clustering is good but the assignment of group may differ

clust2 <- ifelse(km_PCA$cluster == 1,3,ifelse(km_PCA$cluster==2,1,2))

mean(clust2==df_norm$Type)

mean(clust2==clustlist_2$ward.D) # Efficiency is 0.9606742
table(KmeansPCA=clust2,HierarchicalPCA=clustlist_2$ward.D)

mean(clust2==clusto1) # Efficiency is 0.9775281
table(KmeansPCA=clust2,KmeansOriginal=clusto1)

# Now comparing between the two model (PCA and Original)
mean(clust2==clusto1) # Exact prediction 

which(clust2!=clusto1)

list("These particuar rows are wrongly predicted"= df[which(clust!=clusto),])

