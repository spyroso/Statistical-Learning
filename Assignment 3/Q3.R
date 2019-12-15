#PART B
load("/Users/Spyros/Downloads/Returnsdata.RData")
library(MASS)

#Note: Unbiased parameter estimates for a multivariate normal distribution are: 
#(1) mu_hat = X_bar
#(2) sigma_hat = 1/(n-1) * sum(i=1:n) (X_i - X_bar)(X_i - X_bar)^T

#estimating the mean log-returns
mu_hat_stock_l = mean(Returndf[,2])     #estimate of mean for stock fund log-returns 
mu_hat_bond_l = mean(Returndf[,3])      #estimate of mean for bond fund log-returns 
mu_hat_mth_l = colMeans(Returndf[,-1])  #vector of mean estimates for monthly log-returns
mu_hat_ann_l = 12 * mu_hat_mth_l        #vector of mean estimates for annual log-returns
mu_hat_ann_l

#estimating the variance-covariance matrix of log-returns
X1_C = Returndf[,2] - mean(Returndf[,2])      #centering observations
X2_C = Returndf[,3] - mean(Returndf[,3])      #centering observations
X_C = rbind(X1_C, X2_C) 
sigma_hat_mth_l = (X_C %*% t(X_C)) /(nrow(Returndf)-1)   #covariance matrix of monthly log-returns
sigma_hat_ann_l = 12*sigma_hat_mth_l                     #covariance matrix of annual log-returns
sigma_hat_ann_l

#standard deviation of annual log-returns of assets
STD_1 = sqrt(sigma_hat_ann_l[1,1])           #standard deviation of annual log-return of asset #1 (the stock)
STD_2 = sqrt(sigma_hat_ann_l[2,2])           #standard deviation of annual log-return of asset #2 (the bond)
STD_1 
STD_2 
#estimating the correlation between the log-returns of the two assets
CORR = sigma_hat_ann_l[1,2]/(STD_1*STD_2)    #correlation of log-retruns betweent the two assets 


#PART C 
set.seed(99999)
r = mvrnorm(n = 10000, mu_hat_ann_l, sigma_hat_ann_l, empirical = TRUE) #simulating 10000 annual log-returns 
R = exp(r) - 1   #converting annual log-returns into annual returns
w_star = matrix(0, 23, 2)

#Using Monte Carlo Simulation to  optimal portfolio allocation w* for n = 3,4,...,25
for (n in 1:23){
  W = NULL
    E <- function(w){
      mean( (1 + R * w)^(-1-n) )
      }
    W = c(nlm(E,c(0,0))$estimate)
  w_star[n,1] = exp(W[1]) /  (exp(W[1]) + exp(W[2]))
  w_star[n,2] = exp(W[2]) /  (exp(W[1]) + exp(W[2]))
}
w_star  
w_star_1 = w_star[,1]
w_star_2 = w_star[,2]


plot(c(3:25), w_star_1, col = "blue", type = "l", xlab = "eta", ylab= "Weights", main = "Optimal Portfolio Allocations for Varying Eta", ylim = c(0.49,0.51))
  lines(c(3:25), w_star_2, col = "black", type = "l")
  legend(x = "top", legend=c("w*_1","w*_2"), col=c("blue","black"), pch=c(16,16), bg='lightgrey')
  