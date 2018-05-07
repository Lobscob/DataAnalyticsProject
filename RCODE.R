kmeansdata$X1<- NULL


part1<- as.integer(dim(kmeansdata)[1]/3*2) #make 66% partition
part2<-as.integer(dim(kmeansdata)[1]-part1) # test data
responseY <- kmeansdata[1:part1,2:21]

pca <- princomp(responseY, cor=T) # principal components analysis using correlation matrix
pc.comp <- pca$scores
pc.comp1 <- -1*pc.comp[,1] # principal component 1 scores (negated for convenience)
pc.comp2 <- -1*pc.comp[,2] # principal component 2 scores (negated for convenience)

X <- cbind(pc.comp1, pc.comp2)
cl <- kmeans(X,3)
cl$cluster
plot(pc.comp1, pc.comp2,col=cl$cluster)
points(cl$centers, pch=16)

mss <- (nrow(x)-1)*sum(apply(x,2,var))
for (i in 2:15) mss[i] <- sum(kmeans(x,centers=i)$withinss)
plot(1:15, mss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

#so 3 is the best k
cl<-kmeans(x,3)

df <- data.frame(kmeansdata$movieId,cl$cluster)

cluster1 <- df[df$cl.cluster==1,1:2]
cluster2 <- df[df$cl.cluster==2,1:2]
cluster3 <- df[df$cl.cluster==3,1:2]

uniqueUsers <- unique(ratings$userId)

#All mean ratings for cluster 1 of all users
meanRatings<-NULL
meanRatings <- matrix(ncol=3, nrow=length(uniqueUsers))
instances <- 10000
for(i in 1:instances){

      am<- ratings[ratings$userId==uniqueUsers[i],1:3] #allMoviesFromUser
      amc <- am[am$movieId %in% cluster1$kmeansdata.movieId,1:3] #allMoviesFromUserInCluster
      x<- round(mean(amc$rating),2)
      
      meanRatings[i,]<- c( uniqueUsers[i],cluster=1,rating=x)
    
}

output1 <- data.frame(userId =meanRatings[,1], cluster =meanRatings[,2],rating = meanRatings[,3])
output1 <- output1[output1$rating>4,1:3]
output1 <- na.omit(output1)

#All mean ratings for cluster 2 of all users
meanRatings<-NULL
meanRatings <- matrix(ncol=3, nrow=length(uniqueUsers))
for(i in 1:instances){
  
  am<- ratings[ratings$userId==uniqueUsers[i],1:3] #allMoviesFromUser
  amc <- am[am$movieId %in% cluster2$kmeansdata.movieId,1:3] #allMoviesFromUserInCluster
  x<- round(mean(amc$rating),2)
  
  meanRatings[i,]<- c( uniqueUsers[i],cluster=1,rating=x)
  
}

output2 <- data.frame(userId =meanRatings[,1], cluster =meanRatings[,2],rating = meanRatings[,3])
output2 <- output2[output2$rating>4,1:3]
output2 <- na.omit(output2)

#All mean ratings for cluster 3 of all users
meanRatings<-NULL
meanRatings <- matrix(ncol=3, nrow=length(uniqueUsers))
for(i in 1:instances){
  
  am<- ratings[ratings$userId==uniqueUsers[i],1:3] #allMoviesFromUser
  amc <- am[am$movieId %in% cluster3$kmeansdata.movieId,1:3] #allMoviesFromUserInCluster
  x<- round(mean(amc$rating),2)
  
  meanRatings[i,]<- c( uniqueUsers[i],cluster=1,rating=x)
  
}

output3 <- data.frame(userId =meanRatings[,1], cluster =meanRatings[,2],rating = meanRatings[,3])
output3 <- output3[output3$rating>4,1:3]
output3 <- na.omit(output3)




