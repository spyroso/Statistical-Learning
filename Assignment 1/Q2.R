#######################################################################################
############################    QUESTION 2          ###################################
#######################################################################################
#loading & checking the data
library(ISLR)
Credit = Credit

#######################################################################################
####################      Part A: 5-fold cross validation         #####################
#######################################################################################
SSE = rep(0,5)
for(i in 1:5){
  outSample = Credit[((1+80*i-80):(80*i)),]     #the five validation sets will be: (1) rows 1:80, (2) 81:160, etc. 
  inSample = Credit[-((1+80*i-80):(80*i)),]     #training sets will be the entire dataset excluding the validation set
  lm.temp = lm(Balance~., data = inSample)
  SSE[i] = sum((predict.lm(lm.temp, newdata = outSample) - outSample$Balance)^2)
}
MSE_Tot = sum(SSE) / nrow(Credit)
RMSE = sqrt(MSE_Tot)
RMSE


#######################################################################################
##################      PART B: Significance of Predictor Variables     ###############
#######################################################################################
x = model.matrix(Balance~., Credit)
Balance = Credit$Balance
lm.all = lm(Balance~x)
sig_test= summary(lm.all)$coeff[-1,4] < 0.05     #tests whether the variable is significant at the 5% level
sig_vars = which(sig_test,TRUE)       #returns the variables that are significant at 5% level
names(sig_vars)

x_sig = x[,sig_vars+1]  #creates a matrix with the significant predictor variables. Note: the first column of x is the intercept, so we shift sig_vars by one.
Credit_sig = cbind(x_sig,Balance)
Credit_sig = as.data.frame((Credit_sig)) #This is the new data frame made up of the significant variables and the response (Balance)

SSE_sig = 0
for(i in 1:5){
  inSample = Credit_sig[-((1+80*i-80):(80*i)),]
  outSample = Credit_sig[((1+80*i-80):(80*i)),]
  lm.temp = lm(Balance~., data = inSample)
  SSE_sig[i] = sum((predict.lm(lm.temp, newdata = outSample) - outSample$Balance)^2)
}
MSE_sig_Tot = sum(SSE_sig) / nrow(Credit)
sqrt(MSE_sig_Tot)
#The RMSE is calculated to be 99.78


#######################################################################################
###########   PART C: Minimizing BIC (forward and exhaustive approaches)    ###########
#######################################################################################
library(leaps)

#Forward approach
regfit.fwd = regsubsets(Balance~., data = Credit, nvmax = 12, method = "forward")
reg.summary.fwd = summary(regfit.fwd)
which.min(reg.summary.fwd$bic) 
coef(regfit.fwd,which.min(reg.summary.fwd$bic))
names(coef(regfit.fwd,which.min(reg.summary.fwd$bic))[-1])
min(reg.summary.fwd$bic)
#Model that minimizes BIC has 5 parameters: Income, Limit, Rating, Cards, StudentYes

#Exhaustive Approach
regfit.exh = regsubsets(Balance~., data = Credit, nvmax = 12, method = "exhaustive")
reg.summary.exh = summary(regfit.exh)
which.min(reg.summary.exh$bic)
coef(regfit.exh, which.min(reg.summary.exh$bic))
names(coef(regfit.exh, which.min(reg.summary.exh$bic))[-1])
min(reg.summary.exh$bic)
#Model that minimizes BIC has 4 parameters: Income, Limit, Cards, and StudentsYes

if (min(reg.summary.fwd$bic) < min(reg.summary.exh$bic)){
  print("forward approach yields smaller BIC")
} else {
  print("exhaustive approach yields smaller BIC")}
#Here, the exhasutive approach yields a smaller BIC 


#######################################################################################
############################ Part D: Ridge Regression       ###########################
#######################################################################################
library(glmnet)
lambdagrid =10^seq(10,-2,length=100)
x = model.matrix(Balance~., Credit)
y = Credit$Balance
SSE_ridge = matrix(nrow = 5, ncol = 100)
#calculates the SSE for each sample and each value of lamda
for(j in 1:100){
  for (i in 1:5){
    
    inSample_x = x[-((1+80*i-80):(80*i)),]
    outSample_x = x[(1+80*i-80):(80*i),]
    
    inSample_y = y[-((1+80*i-80):(80*i))]
    outSample_y = y[((1+80*i-80):(80*i))]
    
    glm.temp = glmnet(inSample_x,inSample_y, alpha = 0, lambda = lambdagrid, standardize = FALSE)
    
    SSE_ridge[i,j] = (sum((predict(glm.temp,s = 10^(10 -(12/99)*(j-1)), newx = outSample_x))-outSample_y)^2)
  }
}
SSE_ridge = colSums(SSE_ridge)
MSE_ridge = SSE_ridge/nrow(x)
RMSE_ridge = sqrt(MSE_ridge)
lambda_index_optimal_ridge = which.min(RMSE_ridge)
lambda_optimal_ridge = 10^(10 -(12/99)*(lambda_index_optimal_ridge-1))
lambda_optimal_ridge
min(RMSE_ridge)
#The optimal value of lambda is 0.04037 and the associated RMSE is 102.6047 


#######################################################################################
########################   Part E: Lasso Regression         ###########################
#######################################################################################
library(glmnet)
lambdagrid =10^seq(10,-2,length=100) #notice that this is an arithmetic sequence, so we can write each term explicitly. A_1 + d(99) = A_99 => d = -12/99. 
x = model.matrix(Balance~., Credit)
y = Credit$Balance
SSE_lasso = matrix(nrow = 5, ncol = 100)
#calculates the SSE for each sample and each value of lamda
for(j in 1:100){
  for (i in 1:5){
    
    inSample_x = x[-((1+80*i-80):(80*i)),]
    outSample_x = x[(1+80*i-80):(80*i),]
    
    inSample_y = y[-((1+80*i-80):(80*i))]
    outSample_y = y[((1+80*i-80):(80*i))]
    
    glm.temp = glmnet(inSample_x,inSample_y, alpha = 1, lambda = lambdagrid, standardize = FALSE)
    
    SSE_lasso[i,j] = (sum((predict(glm.temp,s = 10^(10 -(12/99)*(j-1)), newx = outSample_x))-outSample_y)^2)
  }
}
SSE_lasso = colSums(SSE_lasso)
MSE_lasso = SSE_lasso/nrow(x)
RMSE_lasso = sqrt(MSE_lasso)
lambda_index_optimal_lasso = which.min(RMSE_lasso)
lambda_optimal_lasso = 10^(10 -(12/99)*(lambda_index_optimal_lasso-1))
lambda_optimal_lasso
min(RMSE_lasso)
#The optimal value of lambda is 0.01 and the associated RMSE is 102.6022


#######################################################################################
#################     PART F: Conclusion for Model Selection    #######################
#######################################################################################
#Model from part B
SSE = rep(0,5)
for(i in 1:5){
  outSample = Credit[((1+80*i-80):(80*i)),]     
  inSample = Credit[-((1+80*i-80):(80*i)),]     
  lm.temp = lm(Balance~Income+ Limit+ Rating+ Cards+ Student, data = inSample)
  SSE[i] = sum((predict.lm(lm.temp, newdata = outSample) - outSample$Balance)^2)
}
MSE_Tot = sum(SSE) / nrow(Credit)
RMSE = sqrt(MSE_Tot)
RMSE

selected.model = lm(Balance~Income+ Limit+ Rating+ Cards+ Student, data = Credit)
summary(selected.model)
coef(selected.model)


