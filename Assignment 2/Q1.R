####################
#### QUESTION 1 ####
####################

install.packages("plotly")
install.packages("dplyr")
library(plotly)
library(dplyr)

####################
#####  PART A  #####
####################
load("/Users/Spyros/Downloads/CreditDefault.RData")
X = model.matrix(InDefault~., dfdefault)
Y = dfdefault$InDefault

#minimizing the negative of the log-likelihood function using nlm
l <- function(beta) {
  sum( -Y*(X %*% beta) + log(1 + exp(X %*% beta)))
}
MLE = nlm(l,c(0,0,0,0))
BETA = c(MLE$estimate)
BETA   #logistic regression coefficients

Y_0 = 1 / (1 + exp(X %*% BETA))  #Probability of No Default 
Y_1 = exp(X %*% BETA) / (1 + exp(X %*% BETA))  #Probability of Default 
hist(Y_1, xlim=c(0,1), xlab = "Probability of Default", ylab = "Frequency", main = "Histogram for Probability of Default")

####################
#####  PART B  #####
####################
G_Hat = NULL 
Error = NULL
for (j in 1:3000){
  G_Hat[j] = ifelse (Y_1[j] < 0.5, 0,1)
  Error[j] = ifelse (Y[j] == G_Hat[j], ifelse(G_Hat[j] == 1, "TP", "TN"), ifelse(G_Hat[j] == 1, "FP", "FN"))
}
Error_Rate = (length(which(Error == "FN")) + length(which(Error == "FP")))/length(Y)
Error_Rate

####################
#####  PART C #####
####################
# A = 30 
TP = 0 
TN = 0 
FN = 30
FP = 1
Error_30 = NULL
G_Hat_30 = NULL
for (j in 1:3000){
  G_Hat_30[j] = ifelse ( (TP*Y_1[j] + FP*(1-Y_1[j])) < (FN*(Y_1[j]) + TN*(1-Y_1[j])), 1, 0)
  Error_30[j] = ifelse (Y[j] == G_Hat_30[j], ifelse(G_Hat_30[j] == 1, "TP", "TN"), ifelse(G_Hat_30[j] == 1, "FP", "FN"))
}    

FN_30 = length(which(Error_30 == "FN"))
TN_30 = length(which(Error_30 == "TN"))
FP_30 = length(which(Error_30 == "FP"))
TP_30 = length(which(Error_30 == "TP"))

#Creating the confusion matrix 
CM_30 = matrix(c(TN_30,FP_30, FN_30,TP_30), 2, 2)
CM_30
Error_Rate_30 = (FN_30 + FP_30)/ length(Y)
Error_Rate_30

# A = 60 
TP = 0
TN = 0 
FN = 60
FP = 1

G_Hat_60 = NULL
Error_60 = NULL
for (j in 1:3000){
  G_Hat_60[j] = ifelse ( (TP*Y_1[j] + FP*(1-Y_1[j])) < (FN*(Y_1[j]) + TN*(1-Y_1[j])), 1, 0)
  Error_60[j] = ifelse (Y[j] == G_Hat_60[j], ifelse(G_Hat_60[j] == 1, "TP", "TN"), ifelse(G_Hat_60[j] == 1, "FP", "FN"))
}    

FN_60 = length(which(Error_60 == "FN"))
TN_60 = length(which(Error_60 == "TN"))
FP_60 = length(which(Error_60 == "FP"))
TP_60 = length(which(Error_60 == "TP"))

#Confusion matrix for A = 60 
CM_60 = matrix(c(TN_60,FP_60, FN_60,TP_60), 2, 2)
CM_60
Error_Rate_60 = (FN_60 + FP_60)/ length(Y)
Error_Rate_60

####################
#####  PART D #####
####################
# A = 0 to inf 
A = seq(from = 0, to = 10000, by = 0.5)
TP = 0
TN = 0 
FN = A
FP = 1

Xx = NULL 
Yy = NULL 

#using 5000 values for A
for (i in 1:5000){
  G_Hat = NULL
  Error = NULL 
  FPR = NULL 
  TPR = NULL
  
  for (j in 1:3000){
    if ( (TP*Y_1[j]+ FP*(1-Y_1[j])) < (FN[i]*(Y_1[j]) +TN*(1-Y_1[j])) ){ 
      G_Hat[j] = 1
    } else { 
      G_Hat[j] = 0
    }
    
    Error[j] = ifelse (Y[j] == G_Hat[j], ifelse(G_Hat[j] == 1, "TP", "TN"), ifelse(G_Hat[j] == 1, "FP", "FN"))
    FPR = length(which(Error == "FP")) / (length(Y) - sum(Y)) 
    TPR = length(which(Error == "TP")) / sum(Y)
    
  }
  Xx[i] = FPR
  Yy[i] = TPR
}

data = data.frame(Xx,Yy)
plot_ly(data = data) %>%
  add_lines(x = Xx, y = Yy, name = "ROC Curve",color = I('blue') )%>%
  
  layout(
    xaxis = list(title = "Rate of FP"),
    yaxis = list(title = "Rate of TP"),
    title = "ROC Curve")

#Calculating the area under the ROC curve using trapezoid method 
AUROC = NULL 
for (i in 1:4999){
  AUROC[i] = .5*(Yy[i] + Yy[i+1])*(Xx[i+1]- Xx[i])
}
AUC = sum(AUROC)
AUC   # 0.7632363





