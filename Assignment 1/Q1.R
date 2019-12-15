#######################################################################################
############################    QUESTION 1          ###################################
#######################################################################################

install.packages("plotly")
install.packages("dplyr")
library(plotly)
library(dplyr)

#######################################################################################
####################      Part A: Loading Data        #################################
#######################################################################################

load("C:/Users/Tony/Downloads/Loaddata (1).RData")

#######################################################################################
####################      Part B: Design matrix      ##################################
#######################################################################################

a = (as.numeric(Weekseries - Weekseries[1]))/7
designVector = rep(1,length(Weekseries))
n = 5
for ( i in 1:n) {
  designVector = append(designVector, sin(3*pi/2 + 2*pi*i*a/(365.25/7)))
  designVector = append(designVector, cos(3*pi/2 + 2*pi*i*a/(365.25/7)))
}

Designmatrix = matrix( 
  designVector, # the data elements 
  length(a),              # number of rows 
  2*n +1,              # number of columns 
  byrow = FALSE)        # fill matrix by rows 

#######################################################################################
####################      Part C: OLSSE Function     ##################################
#######################################################################################


OLSSSE <- function(yTrain, XdesignTrain, XdesignValid, yValid) {
  hatMatrix = XdesignTrain %*% solve(t(XdesignTrain) %*% XdesignTrain) %*% t(XdesignTrain)
  betaMatrix = solve(t(XdesignTrain) %*% XdesignTrain) %*% t(XdesignTrain) %*% yTrain
  yPredict = XdesignValid %*% betaMatrix
  return(sum((yPredict - yValid)^2))
}

#######################################################################################
####################      Part D: 6-fold cross validation         #####################
#######################################################################################

n = data.frame(as.numeric((Weekseries - Weekseries[1])/7) , as.numeric(format(Weekseries, format="%Y")))
colnames(n) <-  c("Week", "Year")

startYear = 2006
SSEmat = matrix(nrow = 5, ncol = 6)
for (i in 1:6){for (j in 1:5){
  
  outSampleRows = n[which(n$Year == startYear + i),][,"Week"] + 1
  inSampleRows = n[which(n$Year != startYear + i),][,"Week"] + 1
  
  inSampleDesignmatrix = Designmatrix[inSampleRows,]
  outSampleDesignmatrix = Designmatrix[outSampleRows,]
  inSampleResponse = Loadseries[inSampleRows]
  outSampleResponse = Loadseries[outSampleRows]
  
  
  SSEmat[j,i] = OLSSSE(yTrain = inSampleResponse, XdesignTrain = inSampleDesignmatrix[,c(1:(2*j+1))], XdesignValid = outSampleDesignmatrix[,c(1:(2*j+1))], yValid =  outSampleResponse)
  }
}

SSEtot = rowSums(SSEmat)
which.min(SSEtot)
# P=4 optimal SSE

#######################################################################################
####################      Part E: Optimal Predictions        ##########################
#######################################################################################

new_Designmatrix = Designmatrix[,c(1:(2*4+1))]
weekseries.betas = solve(t(new_Designmatrix) %*% new_Designmatrix) %*% t(new_Designmatrix) %*% Loadseries

OptPreds = new_Designmatrix %*%weekseries.betas
#new.weekseries.df =as.data.frame(cbind(new_Designmatrix, Loadseries))

GraphingData = data.frame(a,Loadseries, OptPreds)
plot_ly(data = GraphingData) %>%
  add_lines(x = a, y =~ Loadseries, name = "Observed",color = I('black') )%>%
  add_lines(x = a, y =~ OptPreds, name = "Model", color = I('blue'))%>%

  layout(
         xaxis = list(title = a),
         yaxis = list(title = "LoadSeries"),
         title = "LoadSeries vs Week")
plot_ly(data = GraphingData) %>%
  add_lines(x = a, y =~ Loadseries - OptPreds, name = "Model", color = I('blue'))%>%
  layout(
    xaxis = list(title = a),
    yaxis = list(title = "LoadSeries"),
    title = "LoadSeries vs Week",
    yaxis2 = list(title = "Exposure",
                  side="right",
                  overlaying = "y"))

#######################################################################################
####################      Part F: August 13, 2012 Prediction      #####################
#######################################################################################
predictionWeek = as.Date("2012-08-13")
t = as.numeric((predictionWeek -Weekseries[1])/7)
predictVector = 1
n = 4
for ( i in 1:4) {
  predictVector = append(predictVector, sin(3*pi/2 + 2*pi*i*t/(365.25/7)))
  predictVector = append(predictVector, cos(3*pi/2 + 2*pi*i*t/(365.25/7)))
}

pred.2012.08.13 = predictVector%*%weekseries.betas
