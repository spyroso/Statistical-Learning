#Nam Phuong Nguyen (40011040)
#Tony Yuan (40029336)
#Spyros Orfanos (40032280)


###############################      Question 1      ############################### 

load("C:/Users/npngu/Dropbox/Stat 497/Assignment 3/YieldCurvedata.RData")

## PART A  ------------------------------------------------------------------------
YCmatrix <-data.matrix(YCdf)                #Create the YCmatrix from YieldCurvedata.RData 
YCmatrix <- YCmatrix[,-1]                   #Remove the first column containing dates
 
YCmatrixdm <- scale(YCmatrix,scale=F)       #Store centered data set
Allmeans <- apply(YCmatrix, MARGIN=2,mean)  #Calculate and store the mean of each column 
#Allmeans

## PART B  ------------------------------------------------------------------------
covmat <- cov(YCmatrixdm)         #Create the Variance-Covariance matrix 
eval<- eigen(covmat)$values       #Extract the eigenvalues from the variance covariance matrix
evec <- eigen(covmat)$vectors     #Extract the eigenvectors from the variance covariance matrix
lamda <- diag(eval)               #Form a diagonal matrix with the eigenvalues
rotations <- evec * sqrt(eval)
loadings <- abs(evec)

#Cummulative percentage of variance explained by the first 6 components 
PVE <- eval/sum(eval)             #Calculate the density of the percentage of variance from each component
CPVE<- cumsum(PVE)                #Calculate the cumulative distribution of the percentage of variance
#PVE[1:6]
CPVE[1:6]                         #Show the first 6 cumulative percentage of variance

#par(mfrow=c(2,1))
#plot(PVE[1:6],ylab="PVE",xlab="Principle component",main="Percentage of Variance")
#plot(CPVE[1:6],xlab="Principle component",main="Cumulative Percentage of variance")


## PART C  ------------------------------------------------------------------------
par(pch=20)
plot(Maturitiesvec,evec[,1],type="p",main="Effect of Three First Loading Vectors",
     ylab="Loading Factor (%)", xlab ="Maturities",                             #Plot the first loading vector
     ylim=c(-0.4,0.2),col="light blue")
points(Maturitiesvec,evec[,2],col="light green")                                              #Plot the second loading vector
points(Maturitiesvec,evec[,3],col="light pink")                                               #Plot the third loading vector
abline(0,0)        #Plot the X axis for visual aid
legend(23,-0.23,legend=c("a1","a2","a3"),col=c("light blue","light green","light pink"),    #Create a legend
       pch=20,title="Loading Vectors")



## PART D  ------------------------------------------------------------------------

#Validation using all components
allcompdata = (evec %*% t(evec)%*% t(YCmatrixdm)) + Allmeans
validation <- t(allcompdata)

#Here we test whether the diference between the validation matrix and YCmatrix is 0 
test <- validation - YCmatrix           #in this case, it is close enough to 0 to assume that the yield curve 
                                        #from all PCA is equivalent to our initial yield curve

#Recover the rows on which the dates we want are 
dates <- c(which(YCdf$Date == "1991-01-02"), which(YCdf$Date == "2000-05-31"),
           which(YCdf$Date == "2003-03-26"), which(YCdf$Date == "2006-06-27"))

#Use all the PC (the original yield curve should be the same as the PCA yield curve)
par(mfrow=c(2,2))
for (i in 1:4) {
  plot(Maturitiesvec,YCmatrix[dates[i],], type = "p",col="black",        #Plot the original yield curve
       main = YCdf$Date[dates[i]],ylab = "Yield Curve",
       xlab = "Time-to-Maturity")
  lines(x=Maturitiesvec,y=validation[dates[i],], col= "blue")            #Plot the PCA yield curve
}   #We could check for all dates but I think its enough to visually validate using only 4 of them

#Use only the 3 first PC
approxPCA3 <- t((evec[,1:3] %*% t(evec[,1:3]) %*% t(YCmatrixdm)) + Allmeans)  #Calculate the yield curve using the 3 first PC

for (i in 1:4) {
  plot(Maturitiesvec,YCmatrix[dates[i],], type = "l",col="black",        #Plot the original yield curve
       main = YCdf$Date[dates[i]],ylab = "Yield Curve",
       xlab = "Time-to-Maturity")
  lines(x=Maturitiesvec,y=approxPCA3[dates[i],], col= "blue")            #Plot the PCA yield curve
}


#Use the 5 first PC
approxPCA5 <- t((evec[,1:5] %*% t(evec[,1:5]) %*% t(YCmatrixdm)) + Allmeans)  #Calculate the yield curve using the 5 first PC

for (i in 1:4) {
  plot(Maturitiesvec,YCmatrix[dates[i],],type = "l", col="black",         #Plot the original yield curve
       main =YCdf$Date[dates[i]],
       ylab = "Yield Curve", xlab = "Time to Maturity")     
  lines(Maturitiesvec,approxPCA5[dates[i],], col= "blue")                 #Plot the PCA yield curve
}



