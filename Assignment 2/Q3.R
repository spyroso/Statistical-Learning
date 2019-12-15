#ASSIGNMENT 2#

#QUESTION 3#


##########################             PART A             ##########################

load("D:/WINTER 2018/STAT497/Assignment 2/PJM-LMP-20180130.RData")
LMPmatrix <- data.matrix(dfELEC[,-1])   #Remove the first column which contained the names of the regions



##########################             PART B             ##########################

Kmeansclustering <-function(Datapoints, ncluster, theseed, maxiter) {
  
  #STEP 0; set the seed
  set.seed(theseed)
  
  #STEP 1: Randomly place the k centroids
  centroid <- matrix(nrow=ncluster,ncol=ncol(Datapoints))
  for (l in 1:ncluster) {
    for (m in 1:ncol(Datapoints)) {
      centroid[l,m] <- runif(1,min(Datapoints[,m]),max(Datapoints[,m]))
    }
  }
  
  #STEP 2: Assign an initial cluster to each zone 
  #Choose the centroid with the smallest euclidean distance for the price of each zone
  cluster1 <- rep(0,nrow(Datapoints))
  cluster2 <- rep(0,nrow(Datapoints))
  
  for (i in 1:nrow(Datapoints)){
    dist = rep(0,ncluster)
    
    for (j in 1:ncluster){
      for (k in 1:ncol(Datapoints)) {
        dist[j] = dist[j] + (Datapoints[i,k]-centroid[j,k])^2         #Calculate the Euclidean distance between each zone and all the clusters
      }
    }
    cluster1[i] <- which.min(dist)                 #Assign the cluster which has the shortest Euclidean distance with the zone
  }
  
  #Check if all clusters have at least one observation, if not, we change the centroid to an observation we have
  for (l in 1:ncluster) {
    temp <- which(cluster1==l)                  #Find which zones are in the lth cluster
    
    #If the cluster has no points in it, assign a random observation to it before recalculating the centroids(this ensures all clusters have at least 1 observation from now on)
    if (length(temp==0)) {                      
      x <- floor(runif(1,1,nrow(Datapoints)+1))
      cluster1[x] <- l
      centroid[l,] <- Datapoints[x,]
    }
  }  
    
  
  #STEP 3: Iterations to create the clusters 
  count <-  0                                     #Start the counter to know how many iterations were done
  
  while (count < maxiter) {
    count <- count+1
    
    #Recalculate the centroids 
    for (l in 1:ncluster) {
      for (m in 1:ncol(Datapoints)) {
          centroid[l,m] <- mean(Datapoints[which(cluster1==l),m])   #calculate the mean of each feature of the new dataset. 
      }
    }
    
    #Reassign a cluster to each zone
    for (i in 1:nrow(Datapoints)){
      dist = rep(0,ncluster)
      
      for (j in 1:ncluster){
        for (k in 1:ncol(Datapoints)) {
          dist[j] = dist[j] + (Datapoints[i,k]-centroid[j,k])^2
        }
        
      }
      cluster2[i] <- which.min(dist)
    }
    
    #If the cluster assignments are the same as they were, we stop the iterative process. 
    if (identical(cluster1,cluster2)) {
      break
    } else {
      cluster1 <- cluster2
    }
    
  }   #end the while loop
  
  #Step 4: Return KmeansOut which contains the final centroids and cluster assignments for each observation
  KmeansOut <- list(centroid,cluster1)
  return(KmeansOut)
  
}   #end the function

Kmeansclustering(LMPmatrix,4,34,20)


##########################             PART C             ##########################

#Iterative process which uses the Kmeansclustering function for k=2,3,4 until all clusters have at least one observations in it
#Returns the final K means outputs


for (i in 2:4) {
  count = 0   #Start a counter to keep track of the number of iterations done
  cluster <- rep(1,length=nrow(LMPmatrix))          #Create a dummy cluster vector which is a series of 1 (this only serves to get into the first if statement)
  rr <- c(1:i)
  while((!all(rr%in%cluster))&&(count<90))  {       #Check if all the clusters have at least one observation and the counter is still less than 30 (ensures the while loop stops)
    count = count+1
    seed = 9*i+count                               #Change the seed at each iteration
    current<- Kmeansclustering(LMPmatrix,i,seed,20)  #Use the Kmeansclustering function to determine the current cluster and centroids with this seed
    cluster <- current[[2]]
  }     #Iteration stops when all clusters have at least one observation
  
  print(current)           #Print cluster 
  print(count)
}

