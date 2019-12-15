#### 1.1 Linear Regression and hybrid stepwise approach ### 

#Plot the histograms for original sale price and log transformed
yval <- log(train[,ncol(train)])
hist(train[,ncol(train)],main= "Histogram of Sale Prices",xlab="Sale Prices ($)")
hist(yval,main="Histogram of Log of Sale Prices",xlab="Log-Transformed Sale Prices ($)",breaks=20)


#Perform stepwise hybrid approach with AIC
n = nrow(train)
null = lm(log(SalePrice) ~ 1, data = train)
full = lm(log(SalePrice) ~ ., data = train)
AICBoth = step(null, scope = list(lower=null,upper=full),
                                           direction="both")


#Perform stepwise hybrid approach with BIC. Set penalty k = log(n)
n = nrow(train)
BICBoth = step(null, scope = list(lower=null,upper=full),
               direction="both",k=log(n))

#Errors in our AIC Model there are singularities. Will Remove
#This model was created using data analysis and considering non included categorical variables.
AICMod = lm(formula = log(SalePrice) ~ OverallQual + Neighborhood + GrLivArea + 
     BsmtFinType1 + GarageCars + OverallCond + MSSubClass + YearBuilt + 
      CentralAir + SaleCondition + Fireplaces + 
     Condition1 + FullBath + BsmtFullBath + HalfBath + KitchenQual + 
     Foundation + WoodDeckSF + ScreenPorch + Exterior1st + LandContour + 
     OpenPorchSF + HeatingQC + Heating + LotFrontage + 
      BsmtFinSF1 + YrSold + LotArea + MiscFeature + 
     TotRmsAbvGrd, data = train)
summary(AICMod)
summary(BICBoth)



#Calculate the out of sample MSE asa performance meausre 
mseTestLog(AICBoth)    
mseTestLog(BICBoth)
mseTestLog(AICMod)
#AICMOD perofrms best here. 

#Residual vs Fitted Graphs
scatterdata =data.frame(predict(AICBoth, newdata = test),log(test$SalePrice)-predict(AICBoth, newdata = test))
colnames(scatterdata) = c("Actual" , "Residuals")
p = plot_ly(data = scatterdata,x=~ Actual, y=~Residuals,type = "scatter",mode = "markers",  marker = list(size = 5, opacity = 0.5))%>%
  layout(title = "Residuals",
         yaxis = list(zeroline = FALSE,
                      title = "Sale Price"),
         xaxis = list(zeroline = FALSE,
                      title = ""))
p


scatterdata =data.frame(exp(predict(BICBoth, newdata = test)),test$SalePrice-exp(predict(BICBoth, newdata = test)))
colnames(scatterdata) = c("Actual" , "Residuals")
p = plot_ly(data = scatterdata,x=~ Actual, y=~Residuals,type = "scatter",mode = "markers",  marker = list(size = 5, opacity = 0.5))%>%
  layout(title = "Residuals",
         yaxis = list(zeroline = FALSE,
                      title = "Sale Price"),
         xaxis = list(zeroline = FALSE,
                      title = ""))
p




#Comparing MSE There'sa huge difference.

#' predict(AICBoth, newdata = remove_missing_levels(AICBoth, test))
#Perform stepwise hybrid for categorical's to see strongest categorical variables
nullCat = lm(log(SalePrice) ~ 1, data = train[,c(getCatVar(),"SalePrice")])
fullCat = lm(log(SalePrice) ~ ., data = train[,c(getCatVar(),"SalePrice")])
BICBothCat = step(nullCat, scope = list(lower=nullCat,upper=fullCat),
                  direction="both",k=log(n))

mseTest(BICBothCat)
