#Algorithm used for pruning tree

rpart.fit = rpart(SalePrice ~., data = train)
plot(rpart.fit, uniform=TRUE, 
     main="Regression Tree for SalePrice")
text(rpart.fit, use.n=TRUE, all=TRUE, cex=.5)

#Find optimal CP for pruning of tree.
optimalCP = rpart.fit$cptable[which.min(rpart.fit$cptable[,"xerror"]),"CP"]
rpart.fit = rpart(SalePrice ~., data = train, cp = optimalCP)
plot(rpart.fit, uniform=TRUE, 
     main="Regression Tree for SalePrice")
text(rpart.fit, use.n=TRUE, all=TRUE, cex=.5)

# rpart.fit = rpart(SalePrice ~., data = train, cp = optimalCP)
# plot(rpart.fit, uniform=TRUE, 
#      main="Regression Tree for SalePrice")
# text(rpart.fit, use.n=TRUE, all=TRUE, cex=.5)


mseTest(rpart.fit)
plotcp(rpart.fit)



