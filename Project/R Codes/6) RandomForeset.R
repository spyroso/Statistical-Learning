install.packages("inTrees")
library(inTrees)

#Cross validation is used by caret package to find us the optimal number of mtry parameter
rfControl = trainControl(method = "repeatedcv",
                             number = 10,
                             repeats = 1)
summary(train)
#Choosing mtry between 15 and 75 for analysis
rfGrid <-  expand.grid( 
  mtry  = c(15:75)
)
cl <- makeCluster(detectCores()-1)
registerDoParallel(cl)

rf.fit = caret::train(SalePrice ~ ., data = train, 
                          method = 'rf', 
                          trControl = rfControl,
                          tuneGrid = rfGrid)
stopCluster(cl)
registerDoSEQ()

rf.fit
load("C:/Users/Tony/Dropbox/Stat 497/Official Project/Model grids/Legit/rf_fit.rda")

rf.model = randomForest(SalePrice ~ ., data = train, mtry = 45, importance = TRUE)
mseTest(rf.model)
mseTestLog(lm.model.full)
rf.predictions = predict(rf.model,newdata = test)



summary(rf.fit)
plot(rf.fit)
plot(rf.model)
summary(rf.model)
rf.model.list = RF2List(rf.model)
rf.model.list.1 = rf.model.list$list



