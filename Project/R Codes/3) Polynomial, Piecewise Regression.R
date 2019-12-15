install.packages("coefplot")
num_var <- names(train)[which(sapply(train, is.numeric))]

n = nrow(train)

#Dropping the variable that I know will not perform well.
num_var_nominal = c()
for( i in 1:length(num_var)) {
  if( length(unique(train[,num_var[i]])) <20 ) { 
    num_var_nominal = c(num_var[i], num_var_nominal)
  }
}
dropVar = c(num_var_nominal, "SalePrice")
poly_num_var = num_var[! num_var %in% dropVar]
# _____________________Getting only the variables that have many levels so these will be the continous ones________________________
#WILL PERFORM A CROSS VALIDATIONS TO SEE WHICH LINEAR MODELS ARE MORE OF A POLYNOMIAL INDIVIDUALLY

n = nrow(train)
varsPoly = rep(0,length(poly_num_var))
flds <- createFolds(train$SalePrice, k = 10, list = TRUE, returnTrain = FALSE)

for(m in 1:length(poly_num_var)) {
  MSRESpoly = rep(0,5)
  for ( j in 1:5) {
    formula = as.formula(paste0("log(SalePrice) ~", "poly(",poly_num_var[m],",", j, ")" ))
    tempSSRES = rep(0,10)

    for( i in 1:10) {
      isData = train[-flds[[i]],]
      osData = train[flds[[i]],]
      templm = lm(formula = formula , data = isData)
      tempSSRES[i] = sum((osData$SalePrice - predict(templm, newdata = osData))^2)
    }
    MSRESpoly[j] = sum(tempSSRES)/n
  }
  varsPoly[m] = which.min(MSRESpoly)
  print(which.min(MSRESpoly))
}

data.frame(varsPoly, poly_num_var)
summary(train[poly_num_var])
##Trying again with logged variables
# Getting only the variables that have many levels so these will be the continous ones)
#




#____________________________________There are certain variables that are 0 TRUNCATED______________________________
#TESTING WHETEHR SUPERIOR WITH OR WITHOUT having an extra term to accomidated
truncatedvar = c("MasVnrArea", "BsmtFinSF1" , "TotalBsmtSF", "X2ndFlrSF", "GarageArea", "WoodDeckSF", "OpenPorchSF" , "BsmtFinSF1", "LotFrontage")
flds <- createFolds(train$SalePrice, k = 10, list = TRUE, returnTrain = FALSE)

#Testing Whether it would be better to have variable not exist at a 0 point.
#Note creates rank deficient model but is okay.
truncLin = rep(0,length(truncatedvar))
for(m in 1:length(truncatedvar)) {
  MSRESpoly = rep(0,2)
  for ( j in 1:2) {
    if( j == 1){
      j = 1
    formula = as.formula(paste0("log(SalePrice) ~", truncatedvar[m], ": I(" ,truncatedvar[m], " != 0)"))
  } else {
    formula = as.formula(paste0("log(SalePrice) ~", truncatedvar[m]))
  }
  tempSSRES = rep(0,10)
  for( i in 1:10) {
    isData = train[-flds[[i]],]
    osData = train[flds[[i]],]
    templm = lm(formula = formula , data = isData)
    tempSSRES[i] = sum((osData$SalePrice - predict(templm, newdata = osData))^2)
  }
  MSRESpoly[j] = sum(tempSSRES)/n
}
truncLin[m] = which.min(MSRESpoly)
}
data.frame(truncLin, truncatedvar)





#Testing variables 
truncPoly = rep(0,length(truncatedvar))
for(m in 1:length(truncatedvar)) {
  MSRESpoly = rep(0,5)
  for ( j in 1:5) {
    formula = as.formula(paste0("log(SalePrice) ~", "poly(",truncatedvar[m],",", j, ")", ": I(" ,truncatedvar[m], " != 0)"))
    tempSSRES = rep(0,10)
    
    for( i in 1:10) {
      isData = train[-flds[[i]],]
      osData = train[flds[[i]],]
      templm = lm(formula = formula , data = isData)
      tempSSRES[i] = sum((osData$SalePrice - predict(templm, newdata = osData))^2)
    }
    MSRESpoly[j] = sum(tempSSRES)/n
  }
  truncPoly[m] = which.min(MSRESpoly)
  print(which.min(MSRESpoly))
}

data.frame(truncPoly, truncatedvar)
summary(train[truncatedvar])


#______________________________________________DEALING WITH CORRELATION FROM PREVIOUS MODELS_________________________________________________
#Looking and analysing the correltion of varaibles selected
#No longer using AICBOTH
num_var <- names(train)[which(sapply(train, is.numeric))]

numeric_var_AICMod = names(coefficients(AICMod))
numeric_var_AICMod = intersect(num_var, numeric_var_AICMod)
numeric_var_AICMod = c(numeric_var_AICMod, "SalePrice")

numeric_var_BICBOTH = names(coefficients(BICBoth))
numeric_var_BICBOTH = intersect(num_var, numeric_var_BICBOTH)
numeric_var_BICBOTH = c(numeric_var_BICBOTH, "SalePrice")





#Redevelopping our linear regression model with interactions and polynomials for commplexity. This was performed through analysis.

lm.model.full = lm(formula = log(SalePrice) ~ OverallQual + Neighborhood + poly(GrLivArea,3) + 
              BsmtFinType1    + OverallCond + MSSubClass + poly(YearBuilt,3) + 
              CentralAir + SaleCondition + Fireplaces +
              Condition1 + FullBath + BsmtFullBath +  KitchenQual  +YearRemodAdd+
              Foundation + WoodDeckSF + ScreenPorch + Exterior1st + LandContour  + 
              OpenPorchSF + I(OpenPorchSF== 0)  + Heating 
              + RoofStyle + poly(LotArea,3) +  
              TotRmsAbvGrd+ 
              MasVnrArea + I(MasVnrArea == 0) + 
                BsmtFinSF1 + 
                X2ndFlrSF + I(X2ndFlrSF == 0)+ 
                GarageArea+ I(GarageArea == 0 )  
              +LotFrontage + I(LotFrontage == 0)   
              , data = train)
lm.model.full.noindicators = lm(formula = log(SalePrice) ~ OverallQual + Neighborhood + poly(GrLivArea,3) + 
                     BsmtFinType1    + OverallCond + MSSubClass + poly(YearBuilt,3) + 
                     CentralAir + SaleCondition + Fireplaces +
                     Condition1 + FullBath + BsmtFullBath +  KitchenQual  +YearRemodAdd+
                     Foundation + WoodDeckSF + ScreenPorch + Exterior1st + LandContour  + 
                     OpenPorchSF  + Heating 
                   + RoofStyle + poly(LotArea,3) +  
                     TotRmsAbvGrd+ 
                     MasVnrArea + 
                     BsmtFinSF1 + 
                     X2ndFlrSF +  
                     GarageArea  
                   +LotFrontage    
                   , data = train)

#Large Improvement
mseTestLog(lm.model.full)
mseTestLog(lm.model.full.noindicators)
mseTestLog(AICMod)
