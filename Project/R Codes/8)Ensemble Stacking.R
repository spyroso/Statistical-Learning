library(coefplot)
#Doing A GLMNET as our second layer model.
#Cross Validation to find optimal lambdas
level2train = train
set.seed(1980)
flds <- createFolds(level2train$SalePrice, k = 5, list = TRUE, returnTrain = FALSE)
n = nrow(train)


#Perofming Kth fold to find optimal Lambdafor our glmnet in our second order model
lambdas = c(0:5)* 0.02
MSElambda = rep(0,length(lambdas))

cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)
for( j in 1:length(lambdas)) {
  
  glmnetGrid.temp <- expand.grid( 
    alpha = 0,
    lambda = lambdas[j]
  )
  SSEMat = rep(0,5)
  for ( i in 1:5) {
    isData = level2train[-flds[[i]],]
    osData = level2train[flds[[i]],]
    tempGBM = gbm(log(SalePrice) ~ ., data = isData, n.trees = 3000, interaction.depth = 4, shrinkage = 0.01, n.minobsinnode = 15)
    
    tempGam = gam(formula = log(SalePrice) ~ OverallQual + Neighborhood + s(GrLivArea,5) + 
                   BsmtFinType1    + OverallCond + MSSubClass + s(YearBuilt,3) + 
                   CentralAir + SaleCondition + Fireplaces +
                   Condition1 + FullBath + BsmtFullBath +  KitchenQual  +YearRemodAdd+
                   Foundation + WoodDeckSF + ScreenPorch + Exterior1st + LandContour  + 
                   OpenPorchSF   + Heating 
                 + RoofStyle + s(LotArea,3) +  
                   TotRmsAbvGrd+ 
                   s( MasVnrArea,3)  + 
                   BsmtFinSF1  +
                   s(X2ndFlrSF,12) +  
                   GarageArea , data = isData)

    isData$v1 = predict(tempGBM, n.trees = 3000)
    isData$v2 = predict(tempGam)
    osData$v1 = predict(tempGBM,n.trees = 3000, newdata = osData)
    osData$v2 = predict(tempGam, newdata = osData)
    
    glmnet.fit.temp = caret::train(log(SalePrice) ~ v1*v2, data = isData, 
                                   method = 'glmnet', 
                                   tuneGrid = glmnetGrid.temp)
    SSEMat[i] = sum((osData$SalePrice - exp(predict(glmnet.fit.temp, newdata = osData)))^2)
  }
  
  MSElambda[j] = sum(SSEMat) / n
  print(MSElambda[j])
}
data.frame(lambdas, MSElambda)

stopCluster(cl)
registerDoSEQ()


##Kth fold was run multiple times and we decided to use 0.3 after it was selected
level2trainLasso = train
level2trainLasso$v1 = predict(gbm.model, n.trees = 3000)
level2trainLasso$v2 = predict(gam.model.full)

myvars = c("v1", "v2", "SalePrice")
level2trainLasso = level2trainLasso[myvars]

glmnetGrid.lasso.level2 <- expand.grid( 
  alpha = 0,
  lambda = 0.03
)
lasso.level2.model = caret::train(log(SalePrice) ~ v1*v2 , data = level2trainLasso, 
                                  method = 'glmnet', 
                                  tuneGrid = glmnetGrid.lasso.level2)

#TestData



level2testLasso = test 
level2testLasso$v1 = predict(gbm.model, n.trees = 3000, newdata = level2testLasso)
level2testLasso$v2 = predict(gam.model.full, newdata = level2testLasso)
level2testLasso$Ensemble = exp(predict(lasso.level2.model, newdata = level2testLasso))


#Graident Boosting Machine
ModelMetrics::mse(exp(level2testLasso$v1),test$SalePrice)
#Generalized Additive Model 
ModelMetrics::mse(exp(level2testLasso$v2),test$SalePrice)
#Level 2 Ridge Stacked
ModelMetrics::mse(level2testLasso$Ensemble,test$SalePrice)

a = lasso.level2.model$finalModel
a = a$beta
betas = a[,100]
