install.packages("gam")
library(gam)



SecondFloorDegreesFreedom = rep(0,20)
for ( i in 1:20) { 
  gam.model.test = gam(formula = log(SalePrice) ~ OverallQual + Neighborhood + s(GrLivArea,5) + 
                         BsmtFinType1    + OverallCond + MSSubClass + s(YearBuilt,3) + 
                         CentralAir + SaleCondition + Fireplaces +
                         Condition1 + FullBath + BsmtFullBath +  KitchenQual  +YearRemodAdd+
                         Foundation + WoodDeckSF + ScreenPorch + Exterior1st + LandContour  + 
                         OpenPorchSF   + Heating 
                       + RoofStyle + s(LotArea,3) +  
                         TotRmsAbvGrd+ 
                         s( MasVnrArea,3)  + 
                         BsmtFinSF1  +
                         s(X2ndFlrSF,i) +  
                         GarageArea , data = train) 
  SecondFloorDegreesFreedom[i] =   mseTestLog(gam.model.test)
}
data.frame(c(1:20),(SecondFloorDegreesFreedom))
#A gam was attempted and was fit using a spline instead of apolynomial. The Degrees of freedom were adjusted manually
gam.model.full = gam(formula = log(SalePrice) ~ OverallQual + Neighborhood + s(GrLivArea,5) + 
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
  GarageArea , data = train) 
mseTestLog(gam.model.full)

