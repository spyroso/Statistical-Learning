#____________________________________________________________________________________________________________________________________________________________
#
# 
#                                                             DATA PRE PROCESSING
# 
# 
#____________________________________________________________________________________________________________________________________________________________


library(caret)
library(gbm)
library(randomForest)
library(doParallel)
library(nnet)
library(MASS)
library(olsrr)
library(plyr)
library(pROC)
library(plotly)
library(dplyr)
library(corrplot)
library(cvTools)
library(coefplot)
library(rpart)
library(xlsx)
library(ggplot2)
library(glmnet)

#Loading the training data found in Kaggle
train = read.csv("C:/Users/Tony/Dropbox/Stat 497/Official Project/train.csv")
#train = read.csv("C:/Users/npngu/Dropbox/Stat 497/Official Project/train.csv")
#train = read.csv("C:/Users/Spyros/Dropbox/Stat 497/Official Project/train.csv")



#Initial Viewing of my data to analyze the different variables
summary(train)

#Seperating mydata into categorical and numerical variables to see what's going on.
#______________________________________STEP 1) Initial Cleaning. (NA's, Useless Variables)_________________________________________________

removedVars <-  c("Id")
#Variables of id not needed for regression analysis since no signal
train = train[ , !(names(train) %in% removedVars)]


#_______________________________CODE THAT GETS LIST OF NUMERIC AND CATEGORICAL_____________________________________
getCatVar = function() {
cat_var <- names(train)[which(sapply(train, is.factor))]
return(cat_var)
}
getNumVar = function() {
num_var <- names(train)[which(sapply(train, is.numeric))]
return(num_var)
}
cat_var  = getCatVar()
num_var = getNumVar()
#_____________________________________1) FIRST DATA PREP FOR GRAPHING_____________________________________________________________

#_____________________________________1A) CATEGORICAL_____________________________________________________________
#Let us see which categorical variables show that they have na and which ones have low dispersion
summary(train[,cat_var])

#We want to find which variables have most of their exposure in one level using graphs.
removedVars = c("Street", "Utilities","Condition2", "PoolQC", "RoofMatl",removedVars) 
#Will drop these from my list of categorical vairables and my training data.
cat_var = cat_var[! cat_var %in% removedVars]
train = train[ , !(names(train) %in% removedVars)]

#Will add NA as group and replace with none as group level to avoid errors

cat_var_na = cat_var[(colSums(sapply(train[cat_var],is.na)) !=0)]
num_var_na = num_var[(colSums(sapply(train[num_var],is.na)) !=0)]
summary(train)
for (i in 1: length(cat_var_na)) {
  levels = c()
  levels <- levels(train[,cat_var_na[i]])
  if (sum((levels!= "None")*1) == length(levels)) {
    levels[length(levels) + 1] <- "None"
  }
  
  # refactor Species to include "None" as a factor level
  # and replace NA with "None"
  train[,cat_var_na[i]] <- factor(train[,cat_var_na[i]], levels = levels)
  train[,cat_var_na[i]][is.na(train[,cat_var_na[i]])] <- "None"
}

summary(train[,cat_var_na])


#END OF CATEGORICAL Variables INITIAL

#1B) NUMERICAL VARIABLES

#Must deal with numerical variables here
num_var <- names(train)[which(sapply(train, is.numeric))]

summary(train[,num_var])
summary(train[,num_var_na])

#Fixing Garage Year to 1900 as a astarting point
for ( i in 1:nrow(train)) {
  if(is.na(train[i,"GarageYrBlt"])) {
    train[i,"GarageYrBlt"] = 1900
  }
}

#We will make NA's 0 so there are now no NA's
for ( j in 1:length(num_var_na) ) {
  for ( i in 1:nrow(train)) {
    if(is.na(train[i,num_var_na[j]])) {
      train[i,num_var_na[j]] = 0
    }
  }
}

summary(train[,num_var_na])

summary(train[,num_var])


#_____________________________________DATA VISUALISATION_____________________________________________________________

#This code will perform all the plots. If need to save plots refer to code 2 which does visualisation and saves. These graphs are analyzed
cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]

plotGrouped = function(x,data = train, logged = FALSE) {
  groupdata = data
  if(logged == TRUE){
  groupdata$SalePrice = log(groupdata$SalePrice)
  }
  groupdata$exposure = 1
  if (!is.character(x)) {
    stop("Please Provide a String")
  }
  
  plot.data = plyr::ddply(groupdata,x,summarise,meanSale=mean(SalePrice),exposure = sum(exposure))
  p = plot_ly(data = plot.data) %>%
    add_bars(x= plot.data[,x], y=~exposure, yaxis="y2", opacity = 0.5, name = "Exposure")%>%
    add_lines(x = plot.data[,x], y =~ meanSale, name = "SalePrice")%>%
    layout(barmode="stack",
           xaxis = list(title = x),
           yaxis = list(title = "Mean Sale Price"),
           title = paste(x,"Sale Price", sep=" "),
           yaxis2 = list(title = "Exposure",
                         side="right",
                         overlaying = "y"))  
  return(p)
}
plotScatter = function(x,data = train,logged = FALSE) {

  scatterdata = data
  if(logged == TRUE){
    
  scatterdata$SalePrice = log(scatterdata$SalePrice)
  }
  p = plot_ly(data = scatterdata,x= scatterdata[,x], y=~SalePrice,type = "scatter",mode = "markers",  marker = list(size = 5, opacity = 0.5))%>%
    layout(title = paste("Sale Price vs", x),
           yaxis = list(zeroline = FALSE,
                        title = "Sale Price"),
           xaxis = list(zeroline = FALSE,
                        title = x))
  
  return(p)
}
plotBoxPlot = function(x, data = train,logged = FALSE) {
  boxplotdata = data
  if(logged == TRUE){
  boxplotdata$SalePrice = log(boxplotdata$SalePrice)
  }
  p = plot_ly(data = boxplotdata, y = ~SalePrice, color = boxplotdata[,x], type = "box")%>%
    layout(title = paste("Sale Price Box Plot of", x),
           yaxis = list(title = "Sale Price"),
           xaxis = list(title = x))
  
  return(p)
}



savecatgraphs <- vector("list",length = length(cat_var))
savenumgraphs <- vector("list",length = length(num_var))
saveboxplots <-vector("list",length = length(cat_var))
savecatgraphsLog <- vector("list",length = length(cat_var))
savenumgraphsLog <- vector("list",length = length(num_var))
saveboxplotsLog <-vector("list",length = length(cat_var))

for ( i in 1:length(cat_var)) {
  savecatgraphs[[i]] = plotGrouped(cat_var[i])
  savecatgraphs[[i]] = plotGrouped(cat_var[i], logged = TRUE)
  
}

for ( i in 1:length(num_var)) {
  savenumgraphs[[i]] = plotScatter(num_var[i])
  savenumgraphs[[i]] = plotScatter(num_var[i], logged = TRUE)
  
  }
for ( i in 1:length(cat_var)) {
  saveboxplots[[i]] = plotBoxPlot(cat_var[i])
  saveboxplots[[i]] = plotBoxPlot(cat_var[i], logged = TRUE)
  
  
}



plotScatter("GarageYrBlt")

#_______________________________ 2)VARIABLE MODIFICATION (REGROUPING, STANDARDIZING, FEATURE MODIFICATION, COLINNEARITY)____________________
#Numerical have covariance so must check covariance to remove certain variables.


#Yay
#____________________________________2A) REGROUPING of Categorical___________________________________________________________
#Looking for data now for any other variables that need regrouping.
summary(train[,cat_var])
#From looking throuh many variables have groupe variables with very low frequency

#AUTOMATIC REGROUPING OF VARIABLES WITH THE SMALL GROUPS INTO LARGEST GROUPS 
smallGroupVar = c()
n = nrow(train)
cat_var = names(train)[which(sapply(train, is.factor))]

#Find gorups that has levels with less than 1 percent of the data and then moves that data into the largest group.

#This for loop will get the list of variables that have a group that accounts for less than 1 percent of the data
for( i in 1: length(cat_var)) {
  tempSmallGroup= plyr::count(train[,cat_var[i]])
  aFreq = tempSmallGroup$freq/n
  c = which(aFreq < 0.01)
  if(length(c) > 0) {
    smallGroupVar = c(smallGroupVar,cat_var[i])
  }
  
}

#This for loop will perform automatic regrouping into the largest group
for( i in 1: length(smallGroupVar)) {
  smallGroupData = train[,smallGroupVar[i]]
  tempSmallGroup= plyr::count(smallGroupData)
  aFreq = tempSmallGroup$freq/n
  aNames = levels(tempSmallGroup$x)
  c = which(aFreq < 0.01)
  groupMax = which.max(aFreq)
  lowExpGroups = aNames[c]
  highestExpGroup = aNames[groupMax]
  datalist = c()
  #Finds all the data points which has small group
  for( j in 1:length(lowExpGroups)) {
    datalist = c(  datalist,   which(smallGroupData == lowExpGroups[j])   )
  }
  #Replaces the small group, with highest group
  smallGroupData[datalist] = highestExpGroup
  #Replaces the column in data with new modified data
  train[,smallGroupVar[i]] = smallGroupData
  #Removes the empty groups
  train[,smallGroupVar[i]] <- factor(train[,smallGroupVar[i]])
  
}




#2B) _______________________________________________CORRELATIONS FOR LINEAR REGRESSION___________________________________________


corVar = num_var

corVar = corVar[! corVar %in% removedVars]


correlation = cor(train[,corVar], method = "pearson", use = "complete.obs")
corplot = corrplot(correlation)




#__________________________________________________________2C) Feature Manipulations______________________________________
#Refer to analysis section of our code. It shall perform the reasoning behind steps, however a gentle introduction in comments will exists

cat_var <- names(train)[which(sapply(train, is.factor))]
num_var <- names(train)[which(sapply(train, is.numeric))]
#VARIABLES THAT WERE DETERMINED TO BE NOT NEEDED
removedVars <-  c("BsmtHalfBath" ,"X3SsnPorch","LowQualFinSF","MiscVal","PoolArea","BsmtFinSF2","BsmtUnfSF","MoSold", removedVars)
train = train[ , !(names(train) %in% removedVars)]


#New List of num_var
num_var <- names(train)[which(sapply(train, is.numeric))]

#Finding which numerical variables do not have many unique values
num_var_nominal = c()
for( i in 1:length(num_var)) {
  if( length(unique(train[,num_var[i]])) <20 ) { 
    num_var_nominal = c(num_var[i], num_var_nominal)
  }
}
summary(train[,num_var_nominal])

#Changing MSScubClass to a group
train$MSSubClass = as.factor(train$MSSubClass)
plotGrouped("MSSubClass")
plotBoxPlot("MSSubClass")


train$MSSubClass

##This code is run again to Regroup MSSUBClass such that no small groups exist
#ReGrouping
smallGroupData = train$MSSubClass
tempSmallGroup= plyr::count(smallGroupData)
aFreq = tempSmallGroup$freq/n
aNames = levels(tempSmallGroup$x)
c = which(aFreq < 0.01)
groupMax = which.max(aFreq)
lowExpGroups = aNames[c]
highestExpGroup = aNames[groupMax]
datalist = c()
#Finds all the data points which has small group
for( j in 1:length(lowExpGroups)) {
  datalist = c(  datalist,   which(smallGroupData == lowExpGroups[j])   )
}
#Replaces the small group, with highest group
smallGroupData[datalist] = highestExpGroup
#Replaces the column in data with new modified data
train$MSSubClass = smallGroupData
#Removes the empty groups
train$MSSubClass <- factor(train$MSSubClass)


#_______________________________________________2D)SEPERATE DATA FRAME WITH STANDARDIZED______________________________________


num_var <- names(train)[which(sapply(train, is.numeric))]

trainSTD <- train
for ( i in 1: length(num_var)) {
  avg = mean(trainSTD[,num_var[i]])
  std = sd(trainSTD[,num_var[i]])
  trainSTD[,num_var[i]] = (trainSTD[,num_var[i]] - avg)/std
}

summary(trainSTD)
#__________________________SplitData______________________

smp_size <- floor(0.7 * nrow(trainSTD))

## set the seed to make your partition reproductible
set.seed(123)
train_ind_STD <- sample(seq_len(nrow(trainSTD)), size = smp_size)


testSTD <- trainSTD[-train_ind_STD, ] 
trainSTD1 <- trainSTD[train_ind_STD, ]
trainSTD = trainSTD1

for(i in 1:length(cat_var)) {
  print(levels(train[,cat_var[i]]) == levels(test[,cat_var[i]]))
}

#____________________________________________________________

#__________________________SPLITTING DATA__________________________________
#
## 70% of the sample size
# smp_size <- floor(0.7 * nrow(train))

## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(train)), size = smp_size)


test <- train[-train_ind, ] 
train1 <- train[train_ind, ]
train = train1

for(i in 1:length(cat_var)) {
  print(levels(train[,cat_var[i]]) == levels(test[,cat_var[i]]))
}
#
#___________________________DONE SPLITTING_______________________________

#Quick functions that allow simple models to have their mse calculated

mseTest = function(x , newdata = test) {
  return(ModelMetrics::mse(as.vector(newdata$SalePrice),as.vector(predict(x,newdata = newdata))))
}
mseTestLog = function(x , newdata = test) {
  return(ModelMetrics::mse(as.vector(newdata$SalePrice),exp(as.vector(predict(x,newdata = newdata)))))
}
