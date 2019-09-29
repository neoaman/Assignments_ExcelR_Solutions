setwd("D:\\STUDY PROCESS\\Excelr\\Assignments\\Pending\\Clustering")
# Libraries ----

# Question no 1 :----
"Perform clustering (Both hierarchical and K means clustering) for the airlines data to obtain optimum number of clusters. 
Draw the inferences from the clusters obtained."

airline <- xlsx::read.xlsx("EastWestAirlines.xlsx",sheetIndex = 2)
airline <- data.frame(airline[,-c(4,5,6)],"cc1_miles" = as.factor(airline$cc1_miles),
                     "cc2_miles"=as.factor(airline$cc2_miles),"cc3_miles"=as.factor(airline$cc3_miles))
#write.csv(summary(airline[,-1]),"summ_air.csv")
dim(airline) # Data contains 3999 rows and 12 columns
head(airline)
df_air <- airline[,-1]
1721/nrow(airline)
# Create Required functions
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

# Normalize_dummycreation and hierarchical clustering
df_air <- normalize_dummy(df_air)
dist_A <-dist(df_air,method = "euclidian")
clustlist <- all_hclust(dist = dist_A,k = 3)
clustlist$table # From table we can see that ward.D2 is giving best result among all
#write.csv(clustlist$table,"hclust_1.csv")
df_AA <-data.frame(airline,cluster = clustlist$ward.D2) 
mode <- function(x){
  which.max(tabulate(x))
}
Exp_A_1 <- data.frame(aggregate(x = airline[,-c(10:12)],by = list(df_AA$cluster),FUN =mean),aggregate(x = airline[,c(10:12)],by = list(df_AA$cluster),FUN =mode)[,-1])
#write.csv(Exp_A_1,"ExpectedA1.csv")

# Check Using the sub Sample
df_s_airline <-airline[sample(1:nrow(airline),size = round(nrow(airline)*0.7),replace = F),]
df_s_air <- normalize_dummy(df_s_airline)
dist_s_A <-dist(df_s_air,method = "euclidian")
clustlist_s_a <- all_hclust(dist_s_A,k = 3)
clustlist_s_a$table
df_SA <- data.frame(df_s_airline,cluster = clustlist_s_a$ward.D2) 
Exp_SA_1 <- data.frame(aggregate(x = df_s_airline[,-c(10:12)],by = list(df_SA$cluster),FUN =mean),
                      aggregate(x = airline[,c(10:12)],by = list(df_AA$cluster),FUN =mode)[,-1])

list(Exp_A_1,Exp_SA_1)
#write.csv(Exp_SA_1,"ExpectedAS1.csv")
# Dendogrm
cl <- hclust(d= dist_A,method = "ward.D2")
plot(cl,hang = -1,labels=airline$id,cex = 0.25)
rect.hclust(cl,k=3)

# KMeans Clustering
df_norm_A <- normalize_dummy(airline[,-1])
km_A <- kmeans(x = df_norm_A,5)
table(km_A$cluster)

Twss<-c();k <- 1:7
for(i in 1:7){
  set.seed(33) # seed = 33  ; I set this seed for constant plot for every iteration so please dont remove it
  km_a <- kmeans(x = df_norm_A,i)
  Twss[i]<-km_a$tot.withinss
}
Twss
plot(y=Twss,x = k,"b",
     col=c(rep(1,2),3,rep(1,4)),cex = c(rep(1,2),2,rep(1,4)),lwd=3,main = 'Scree Plot',
     col.main="blue",col.axis="blue") # I may consider 3 as my optimum k value

text(x = 3,y = 1800,labels = "Elbow point")
# From the plot I get my elbow point for k = 3.
set.seed(33) # I set the same seed for same answer for every iteration
km_A <- kmeans(x = df_norm_A,3)
table(km_A$cluster)

aggregate(x = airline[,-c(1,3,10:12)],by = list(km_A$cluster),mean)
#write.csv(aggregate(x = df_finalA[,-c(1,3,10:12)],by = list(km_A$cluster),mean),"aggreAirKmea.csv")
head(airline)



# Question no 2 ----

crime <- read.csv("crime_data.csv")
head(crime)
df_crime <- crime[,-1]
summary(crime)
#write.csv(summary(crime),"Summary_Crime.csv")
df_s_c <- normalize_dummy(df_crime)
boxplot(df_s_c)$out
dist_C <- dist(df_s_c,method = 'euclidian')
clus_C <- all_hclust(dist = dist_C,k = 3)
clus_C$table
#write.csv(clus_C$table,"Crime_hclust.csc")
df_C <- data.frame(crime,cluster=clus_C$ward.D2)
Exp_C_1 <- data.frame(aggregate(x = df_C[,-c(1,6)],by = list(df_C$cluster),FUN =mean))
#write.csv(Exp_C_1,"Crime_Hclust2.csv")
hcl_c <- hclust(dist_C,method = "ward.D2")
plot(hcl_c,hang = -1,labels = crime$X)
rect.hclust(hcl_c,k = 3,border =c("red",'yellow','green'), )

# Test with a sample
set.seed(12) 
df_s_crime <-crime[sample(1:nrow(crime),size = round(nrow(crime)*0.8),replace = F),]
df_s_c <- normalize_dummy(df_s_crime[,-1])
dist_s_c <-dist(df_s_c,method = "euclidian")
clustlist_s_c <- all_hclust(dist_s_c,k = 3)
clustlist_s_c$table
df_SC <- data.frame(df_s_crime,cluster = clustlist_s_c$ward.D2) 
Exp_SC_1 <- data.frame(aggregate(x = df_SC[,-1],by = list(df_SC$cluster),FUN =mean))

list(Crimedata = Exp_C_1,SampledCrimeData=Exp_SC_1)
#write.csv(Exp_SC_1,"crime_sample_cluster.csv")
# KMeans Clustering
df_norm_C <- normalize_dummy(crime[,-1])
km_c <- kmeans(x = df_norm_C,5)
table(km_c$cluster)

wss<-c()
for(i in 1:10){
  set.seed(11) # seed is 11
  km_c <- kmeans(x = df_norm_C,i)
  wss[i]<-km_c$tot.withinss
}
wss
plot(y=wss,x = 1:10,"b",col=c(rep(1,2),3,rep(1,7)),cex=c(rep(1,2),2,rep(1,7))) # I may consider 5 as my optimum cluster size
text(3,4,labels = "Elbow Point")
set.seed(11) # seed is 11
km_c <- kmeans(x = df_norm_C,3)
table(km_c$cluster)
km_c$cluster
aggregate(crime[,-1],by = list(km_c$cluster),FUN = mean)
#write.csv(aggregate(crime[,-1],by = list(km_c$cluster),FUN = mean),file = "crime_kclusters.csv")
plot(x = crime$Rape,y = crime$Murder,col=ifelse(km_c$cluster == 1,"green",ifelse(km_c$cluster == 2,"red","orange")))
attach(crime)
scatterplot3d::scatterplot3d(x =Murder ,y =Assault ,z =Rape ,
      color = ifelse(km_c$cluster == 1,"green",ifelse(km_c$cluster == 2,"red","orange")),
      pch = 20,type = "h",box = F,main = "Crime Cluster")
write.csv(data.frame(crime,km_c$cluster),"CrimeData")
Criminal <- data.frame(crime,km_c$cluster)
set.seed(5)
windows()
wordcloud::wordcloud(words = Criminal$X,
                     max.words = 50,min.freq =  1,freq = crime$UrbanPop,random.order = T,
                     ordered.colors =T ,
                     colors = ifelse(km_c$cluster == 1,"green",ifelse(km_c$cluster == 2,"red","orange")),rot.per = 0,fixed.asp = T,random.color = F)

