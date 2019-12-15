#Conclusion Results

MSEdf = data.frame(c("AIC Stepwise", "BIC Stepwise", "AIC Stepwise w/ manual variable selection", 
             "Polynomial + Piecewise", "GAM & Splines",
             "Simple Tree", "Random Forest", "Gradient Boosting",
             "Ensemble Stacked"),c(mseTestLog(AICBoth),
mseTestLog(BICBoth),
mseTestLog(AICMod),
mseTestLog(lm.model.full),
mseTestLog(gam.model.full),
mseTest(rpart.fit),
mseTest(rf.model),
ModelMetrics::mse(exp(gbm.predictions),test$SalePrice),
ModelMetrics::mse(level2testLasso$Ensemble,test$SalePrice)))
colnames(MSEdf) = c("Model Name", "Out of Sample MSE")
MSEdf

#Summary of our most important models
summary(lm.model.full)
summary(gam.model.full)
summary(gbm.model)
