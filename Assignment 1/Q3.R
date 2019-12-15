load("D:/WINTER 2018/STAT497/Assignment 1/IV.RData")
install.packages("plot3D")
library(plot3D)

y <- ImplicitVol
X <- cbind(Maturity,Strike)


##########################             PART A             ##########################                          # 

#Creating a function which returns the pdf values of the multivariate gaussian distribution
#xmat is a nxp matrix, muvec is px1 and Sigmamat is diagonal pxp
dmvn <- function(xmat,muvec,Sigmamat) {
  
  #Multivariate Gaussian Distribution
  pdfvec <- rep(0,nrow(xmat))
  for (i in 1:nrow(xmat)){
    pdfvec[i] <- exp((-0.5)*t(xmat[i,]-muvec)%*%solve(Sigmamat)%*%(xmat[i,]-muvec))/sqrt(((2*pi)^(length(muvec)))*det(Sigmamat)) 
  }
  return(pdfvec)
}

##########################             PART B             ##########################

#2x1 column matrix containing the means of Maturity and Strike
mu <- t(t(c(mean(Maturity),mean(Strike))))  

#gridX contains all (Maturity,Strike) possibilities we want to predict
gridMaturity = seq(from=0, to=1, by=0.05)
gridStrike = seq(from=850,to=1150,by=20)
  
#Create a list sigma which contains the three variance-covariance matrices
sigma <- list(sigma1= diag(c(0.3,100)),sigma2=diag(c(0.1,25)),sigma3=diag(c(0.9,200)))

#Adding a column of 1 to beta matrix and X matrix
beta0 <- rep(1,nrow(X))
Xmat0 <- cbind(beta0,Maturity,Strike)

b_0 <- rep(1,length(gridMaturity)*length(gridStrike))
gM <- rep(gridMaturity, each=length(gridStrike))
gS <- rep(gridStrike, length(gridMaturity))
gridX <- cbind(b_0,gM,gS)
gridX

#Create weight and betahat arrays to hold weights and estimated betas
weight<- list(1,2,3)
betahat <- list(1,2,3)
yhat <- list(1,2,3)

#Loops which calculate the fhatmatlist for each covariance matrix, uses dmvn function from part(a)
for ( k in 1:3) {
  betahat = matrix(rep(0,336*3),nrow = 3, ncol = 336)
  for ( i in 1:336) {
    weight <- dmvn(X,as.matrix(as.numeric(gridX[i,c(2,3)])),sigma[[k]])
    W <- diag(weight)
    betahat[,i] <- solve(t(Xmat0)%*%W%*%Xmat0) %*% (t(Xmat0)%*%W%*%y)
  }
  yhat[[k]] <- diag(gridX %*% betahat)
}
fhatmatlist <- list(matrix(yhat[[1]],nrow=21,ncol=16, byrow =  TRUE),
                    matrix(yhat[[2]],nrow=21,ncol=16, byrow = TRUE),
                    matrix(yhat[[3]],nrow=21,ncol=16, byrow = TRUE))


##########################             PART C             ##########################

grid <- mesh(gridMaturity,gridStrike)

#Create the 3 local regression graphs and the scatter plot
par(mfrow=c(2,2),oma=c(1,0,0,1)+0.1,mar=c(2,0,1,1)+0.3)
surf3D(grid$x,grid$y,fhatmatlist[[1]],bty="b2",ylab="Maturity",xlab="Strike",zlab= "Implicit Volatility",
       theta=80,main = "Sigma 1")
surf3D(grid$x,grid$y,fhatmatlist[[2]],bty="b2",ylab="Maturity",xlab="Strike",zlab= "Implicit Volatility",
       theta=80,main = "Sigma 2")
surf3D(grid$x,grid$y,fhatmatlist[[3]],bty="b2",ylab="Maturity",xlab="Strike",zlab= "Implicit Volatility",
       theta=80,contour=T,main = "Sigma 3")
scatter3D(X[,2],X[,1],y,ylab="Maturity",xlab="Strike", zlab= "Implicit Volatility", main="Scatter plot")


##########################             PART D             ##########################

#Predict Nearest Neighbors function 
PNN <- function (yval,xval,xpred,NNnumber) {
  
  dist <- matrix(data=rep(0,nrow(xval)*nrow(xpred)),nrow=nrow(xpred),ncol=nrow(xval))  
  NNpositions <- list()
  NNpreds<- rep(0,nrow(xpred))

  for (k in 1:nrow(xpred)) {
    for (r in 1:nrow(xval)) {
      # calculate the euclidean distance between each xval observation and the predictions we want to make
      s=0
      while (s < ncol(xpred)){
        s=s+1
        dist[k,r] <- dist[k,r] + (xval[r,s]-xpred[k,s])^2  
      }
      dist[k,r]= sqrt(dist[k,r])
    }
    sortedvec <- sort(dist[k,])
    smallest <- sortedvec[1:NNnumber]
    NNpositions[[k]] <- which(dist[k,] %in% smallest)       # finds which xval observations have the smallest distance with the xpred
    
    #Calculate the mean of the y values closest
    i=0
    while (i < NNnumber) {
      i=i+1
      NNpreds[k] = NNpreds[k] + yval[NNpositions[[k]][i]] 
    } 
    NNpreds[k] = NNpreds[k]/NNnumber
  }
  return(NNpreds) #returns a vector of predicted y values
}

##########################             PART E             ########################## 
Xmat <- matrix(nrow=ncol(X),ncol=nrow(X))
Xmat[2,]<- X[,2]/400                      #Create a new X matrix with Strike -> Strike/400
Xmat[1,]<- X[,1]

#Create a mu array where strike -> strike/400
mu <- array(dim=c(21,16,2))
for (i in 1:21) {
  for (j in 1:16) {
    mu[i,j,1]= gridMaturity[i]
    mu[i,j,2]= gridStrike[j]/400
  }
}

#NN contains the NNnumbers we want to use
NN <- c(5,15,30)

#create fhatmatlistKNN to store the predicted y values 
fhatmatlistKNN <- list(matrix(nrow=21,ncol=16),matrix(nrow=21,ncol=16),matrix(nrow=21,ncol=16))

#Loops to find the predicted y values, uses PNN function from part (d)
for (z in 1:3) {
  for (t in 1:21) {
    fhatmatlistKNN[[z]][t,] <- PNN(y,t(Xmat),mu[t,,],NN[z])
  }
  fhatmatlistKNN[[z]]
}


##########################             PART F             ########################## 
#Create the 3 K-Nearest Neighbors graphs and the scatter plot
par(mfrow=c(2,2),oma=c(1,0,0,1)+0.1,mar=c(2,0,1,1)+0.3)
surf3D(mu[,,2],mu[,,1],fhatmatlistKNN[[1]],bty="b2",xlab="Strike",ylab="Maturity",zlab= "Implicit Volatility",
       theta=40,contour=T,main = "K=5",box=T)
surf3D(mu[,,2],mu[,,1],fhatmatlistKNN[[2]],bty="b2",xlab="Strike",ylab="Maturity",zlab= "Implicit Volatility",
       theta=40,contour=T,main = "K=15")
surf3D(mu[,,2],mu[,,1],fhatmatlistKNN[[3]],bty="b2",xlab="Strike",ylab="Maturity",zlab= "Implicit Volatility",
       theta=40,contour=T, main= "K=30")
scatter3D(Xmat[2,],Xmat[1,],y,ylab="Maturity", xlab="Strike", zlab= "Implicit Volatility", main= "Scatter plot")
