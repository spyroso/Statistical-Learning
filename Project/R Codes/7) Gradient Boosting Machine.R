#Perfomring 5 fold cross validation to find optimal parameters
gbmControl = trainControl(method = "repeatedcv",
                         number = 5,
                         repeats = 1)
summary(train)
#Selecting grid for which I will search through for optimal parameters
gbmGrid <-  expand.grid( 
  n.trees = 500*(1:6),
  interaction.depth = (1:4),
  shrinkage = 0.001*(1:10) ,
  n.minobsinnode = 20
)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

#Since the gradient approach reqcuires a normality assumption we will log our data like we had previously
gbm.fit = caret::train(log(SalePrice) ~ ., data = train, 
                      method = 'gbm', 
                      trControl = gbmControl,
                      tuneGrid = gbmGrid)
stopCluster(cl)
registerDoSEQ()
#This caret object has bestvalues

#Takes long so saved the file and loading here
load("C:/Users/Tony/Dropbox/Stat 497/Official Project/Model grids/legit/gbm_fit.rda")
gbm.fit


#I found the best parameters were 3000 , 4 , 0.01 and 15 through many different kth fold cross validations
#The original model was trained using 20
set.seed(1980)
gbm.model = gbm(log(SalePrice) ~ ., data = train, n.trees = 3000, interaction.depth = 4, shrinkage = 0.01, n.minobsinnode = 15)
gbm.predictions = predict(gbm.model, n.trees = 3000, newdata = test)
ModelMetrics::mse(exp(gbm.predictions),test$SalePrice)



plot(gbm.fit)
summary(gbm.fit)
summary(gbm.model)
