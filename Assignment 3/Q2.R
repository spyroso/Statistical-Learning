library(nnet)
library(plotly)
library(dplyr)
#A Genearted the appropriate matrix and 
set.seed(1980)
n = 60 #number of  observations
xfrom =-2 #range of x values
xto = 2 #range of x values
x1 = (xto-xfrom)*runif(n) + xfrom
Gaussvec = rnorm(n) #noise term

y = x1^3 + Gaussvec# responses

x = matrix(c(rep(1,60),x1), nrow = 2, ncol = length(x1), byrow = TRUE)



#B activation functions and derivative of activation fucntion
activlogistic = function(avec) {
  z = 1 / (1 + exp(-avec))
  return(z)
}  

deractivlogistic = function(avec) {
  z = ( exp( - avec) / (1 + exp( - avec))^2)
  return(z)
}


#C Will properly evaluate the neural net values at different points
nneteval = function(xinvec, wmat1, wmat2, activfunc) {

  
  f1 <- match.fun(activfunc) 
  
  a1 = wmat1 %*% xinvec
  z = f1(a1)
  zcol = ncol(z)
  zrow = nrow(z)
  vec1 = rep(1,zcol)
  z= matrix(c(vec1,as.vector(t(z))),nrow = (zrow+1), byrow = TRUE)
  a2 = wmat2 %*% z
  return(a2)
  
}

#Uses nnet to calcualte
nnetSSE = function(xinvec, wmat1, wmat2, activfunc, yinvec) {
  a2 = nneteval(xinvec, wmat1, wmat2, activfunc)
  SSE = sum((yinvec - as.vector(a2))^2)
  return(SSE)
}


#D
nnetGradiet = function( yinvec, xinvec, wmat1, wmat2, activfunc, deractivfunc) {
  

  #If multiple points are given, will find the gradient of all the points by summing. 
  #Will take a matrix for x with the proper form
  numPoints = ncol(xinvec) ; numPoints
  M = nrow(wmat1); M
  p = nrow(xinvec); p
  #Attaches the function that are given as strings
  f1 <- match.fun(activfunc) ; f1
  f2 <- match.fun(deractivfunc) ; f2
  #The full gradient that will be summed up if multiple data points are given
  fullGrad1 = matrix( rep(0, nrow(wmat1)*ncol(wmat1)), nrow = nrow(wmat1)    ) ; fullGrad1
  fullGrad2 = matrix( rep(0, nrow(wmat2)*ncol(wmat2)), nrow = nrow(wmat2)    )  ; fullGrad2
  for (j in 1:numPoints) { 

    
    
    #Saves the jth data point in temporarymatrix
    tempx = matrix(xinvec[,j],p) 
    #Saves the jth response
    tempy = yinvec[j] 
    #Calculates the residual using the current weights multiplied by negative 2
    c = -2*(yinvec[j] - nneteval(tempx,wmat1 = wmat1,wmat2 = wmat2,activfunc = "activlogistic")[1,1])
    #Finds a1 activations
    a1 = wmat1 %*% tempx
    #calculates all the derivative matrices based on algorithms
    awmat1 = matrix(c(rep(1,5),rep(matrix(tempx[,1], nrow = 2)[2,1],5)), ncol = 2)
    awmat2 = matrix(c(1,f1(a1)), nrow = 1) 
    aamat1 = matrix(f2(a1)*wmat2[-1],nrow = 5,ncol = 2) 
    aamat2 = matrix(rep(1,M+1), nrow = 1) 
    #The gradient is found
    grad1 = c*awmat1*aamat1; grad1
    grad2 = c*awmat2 *aamat2; grad2
    #Gradient of specific point is added to the whole gradient in cases where more than 1 data point.
    fullGrad1 = fullGrad1 +grad1
    fullGrad2 = fullGrad2 + grad2
  }
  
  return(list(fullGrad1,fullGrad2))
  
}

#E
set.seed(100)
M = 5
K = 1
w1 = matrix(rnorm(M*2), nrow = M, ncol = K+1)
w2 = matrix(rnorm((M+1) *K), nrow = K, ncol = (M+1))
n = 0.01
#Learning rate at 0.01
l = ncol(x)
#200 iterations to optimize weights.
for ( k in 0:200) {
  t = (k%%l) + 1
  print(t)
  xtemp = matrix(x[,t],2)
  ytemp = y[t]
  # print(xtemp)
  # print(ytemp)
  aList = nnetGradiet( ytemp, xtemp, wmat1 = w1, wmat2 = w2, "activlogistic", "deractivlogistic")
  # bList = nnetGradiet1p(ytemp, xtemp, wmat1 = w1, wmat2 = w2, "activlogistic", "deractivlogistic")
  print(w1)
  print(w2)
  print(aList)
  # print(bList)
  w1 = w1 - n*aList[[1]]
  w2 = w2 - n*aList[[2]]
}
w1
w2



#e
ypredict = nneteval(x,wmat1 = w1,wmat2 = w2, activfunc = "activlogistic")
df = data.frame(cbind(x1,y,as.vector(ypredict)))
colnames(df) = c("X", "Y", "YPredict")


  p1 = plot_ly(data = df) %>%
    add_markers(x=~X , y=~Y,type = "scatter",name = "Observed", mode = "markers",color = I('black'), marker = list(size = 5, opacity = 0.5))%>%
    add_markers(x=~X , y=~YPredict,type = "scatter", name = "Neural Net Prediction",mode = "markers", color = I('blue'), marker = list(size = 5, opacity = 0.5))%>%
    
    layout(title = "Neural Net Predictions",
           yaxis = list(zeroline = FALSE,
                        title = "Y"),
           xaxis = list(zeroline = FALSE,
                        title = "X"))
  

p1

for ( k in 0:5000) {
  t = (k%%l) + 1
  print(t)
  xtemp = matrix(x[,t],2)
  ytemp = y[t]
  # print(xtemp)
  # print(ytemp)
  aList = nnetGradiet( ytemp, xtemp, wmat1 = w1, wmat2 = w2, "activlogistic", "deractivlogistic")
  # bList = nnetGradiet1p(ytemp, xtemp, wmat1 = w1, wmat2 = w2, "activlogistic", "deractivlogistic")
  print(w1)
  print(w2)
  print(aList)
  # print(bList)
  w1 = w1 - n*aList[[1]]
  w2 = w2 - n*aList[[2]]
}
ypredict = nneteval(x,wmat1 = w1,wmat2 = w2, activfunc = "activlogistic")
# plot(x1,nneteval(x,wmat1 = w1,wmat2 = w2, activfunc = "activlogistic"))
# plot(x1,y)
df = data.frame(cbind(x1,y,as.vector(ypredict)))
colnames(df) = c("X", "Y", "YPredict")



p2 = plot_ly(data = df) %>%
  add_markers(x=~X , y=~Y,type = "scatter",name = "Observed", mode = "markers",color = I('black'), marker = list(size = 5, opacity = 0.5))%>%
  add_markers(x=~X , y=~YPredict,type = "scatter", name = "Neural Net Prediction",mode = "markers", color = I('blue'), marker = list(size = 5, opacity = 0.5))%>%
  
  layout(title = "Neural Net Predictions",
         yaxis = list(zeroline = FALSE,
                      title = "Y"),
         xaxis = list(zeroline = FALSE,
                      title = "X"))


p2













