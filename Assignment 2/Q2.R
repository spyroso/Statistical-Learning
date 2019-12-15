#ASSIGNMENT 2#

#QUESTION 2#

library(plotly)
library(dplyr)
library(plot3D)



##########################             PART A             ##########################
f1 = function(x) {
  return(4 + 3*sqrt(abs(x)+2))
}

f2 = function(x) {
  return(sin(x/12) + sqrt(x+12))
}
#These 2 functions will return the vector with the vectors after having this function applied on it

set.seed(1980)
x1 = runif(10000,-7,7)
x2 = runif(10000,-12,12)
epsilon = rnorm(10000)
y = f1(x1) + f2(x2) + 0.5*epsilon
sim.data = cbind(x1,x2,y)



scatter3D(x1,x2,y,theta=40,labels = c("x1", "x1", "y"))
par(mfrow=c(2,1))
plot(x1,f1(x1))
plot(x2,f2(x2))



##########################             PART B             ##########################

nodesx1 = seq(-5,5,2)
nodesx2 = seq(-10,10,4)


BuildSplines1DDesign = function(xvec,nodes) {
  design = as.matrix(cbind(rep(1,length(xvec)), xvec, xvec^2, xvec^3))
  k = length(nodes)
  for( i in 1:k) {
    a = (xvec - nodes[i])^3
    
    for (j in 1: length(a)) {
      if(a[j] < 0) {
        a[j] = 0
      }
    }
    #This for loop sets all negative values to 0
    design = as.matrix(cbind(design,a))
  }
  colnames(design) <- NULL
  return(design)
}

Designx1 = BuildSplines1DDesign(x1,nodesx1)
Designx2 = BuildSplines1DDesign(x2,nodesx2)


##########################             PART C             ##########################

betas1 = as.matrix(rep(0,10))
betas2 = as.matrix(rep(0,10))

for ( i in 1:100) {
  U1 = y - Designx2%*%betas2
  betas1 = as.matrix(solve(t(Designx1) %*% Designx1) %*% t(Designx1) %*% U1)
  
  U2 = y - Designx1%*%betas1
  betas2 = as.matrix(solve(t(Designx2) %*% Designx2) %*% t(Designx2) %*% U2)
  
}
f1hat = function(x) {
  BuildSplines1DDesign(xvec = x, nodes = nodesx1) %*% betas1
}
f2hat = function(x) {
  BuildSplines1DDesign(xvec = x, nodes = nodesx2) %*% betas2
}


##########################             PART D             ##########################

x1seq = seq(-7,7,0.1)
x2seq = seq(-12,12,0.1)
f1(x1seq)
f1hat(x1seq)
f2(x2seq)
f2hat(x2seq)

x1data = data.frame(x1seq, f1(x1seq), f1hat(x1seq))
x2data = data.frame(x2seq, f2(x2seq), f2hat(x2seq))

plot_ly(data = x1data) %>%
  add_lines(x = x1seq, y =~ f1.x1seq., name = "f2",color = I('black') )%>%
  add_lines(x = x1seq, y =~ f1hat.x1seq., name = "f2.hat", color = I('blue'))%>%
  
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    title = "f1(x) and Cubic Spline")

plot_ly(data = x2data) %>%
  add_lines(x = x2seq, y =~ f2.x2seq., name = "f2",color = I('black') )%>%
  add_lines(x = x2seq, y =~ f2hat.x2seq., name = "f2.hat", color = I('blue'))%>%
  
  layout(
    xaxis = list(title = "X"),
    yaxis = list(title = "f1(x)"),
    title = "f2(x) and Cubic Spline")



c1 = mean(f1(x1seq) - f1hat(x1seq))
#The c1 is calculated here to provide the constant adjustment for our cubic spline.
f1(x1seq)
f1hat(x1seq) + c1
f2(x2seq)
f2hat(x2seq) - c1

x1data$f1hat.x1seq.constant = x1data$f1hat.x1seq. + c1
x2data$f2hat.x2seq.constant = x2data$f2hat.x2seq. - c1

plot_ly(data = x1data) %>%
  add_lines(x = x1seq, y =~ f1.x1seq., name = "f2",color = I('black') )%>%
  add_lines(x = x1seq, y =~ f1hat.x1seq.constant, name = "f2.hat + constant", color = I('blue'))%>%
  
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    title = "f1(x) and Cubic Spline + c1")

plot_ly(data = x2data) %>%
  add_lines(x = x2seq, y =~ f2.x2seq., name = "f2",color = I('black') )%>%
  add_lines(x = x2seq, y =~ f2hat.x2seq.constant, name = "f2.hat + constant", color = I('blue'))%>%
  
  layout(
    xaxis = list(title = ""),
    yaxis = list(title = ""),
    title = "f2(x) and Cubic Spline + c1")

