# Chapter 8 Lab: Decision Trees

# Fitting Classification Trees

library(tree)
library(ISLR)
attach(Carseats)
hist(Sales)

# turn Sales to binary variable
High=ifelse(Sales<=8,"No","Yes")
Carseats=data.frame(Carseats,High)

# fit tree model; exclude Sales since we
# now have High variable
tree.carseats=tree(High ~ . -Sales, Carseats)
summary(tree.carseats)

# tree diagram
plot(tree.carseats)
text(tree.carseats,pretty=0)

# text form of tree
tree.carseats

# create training / test sets
set.seed(1011)
train=sample(1:nrow(Carseats), 200)
Carseats.test=Carseats[-train,]
High.test=High[-train]

# fit model on training set
tree.carseats=tree(High~.-Sales,Carseats,subset=train)
plot(tree.carseats); text(tree.carseats,pretty=0)

# make predictions on test set using mode
tree.pred=predict(tree.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+57)/200

# use CV to prune tree optimally to reduce variance
set.seed(3)
cv.carseats=cv.tree(tree.carseats,FUN=prune.misclass)
names(cv.carseats)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size,cv.carseats$dev,type="b")
plot(cv.carseats$k,cv.carseats$dev,type="b")
prune.carseats=prune.misclass(tree.carseats,best=9)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(94+60)/200
prune.carseats=prune.misclass(tree.carseats,best=15)
plot(prune.carseats)
text(prune.carseats,pretty=0)
tree.pred=predict(prune.carseats,Carseats.test,type="class")
table(tree.pred,High.test)
(86+62)/200

# Fitting Regression Trees

library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston=tree(medv~.,Boston,subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston,pretty=0)
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size,cv.boston$dev,type='b')
prune.boston=prune.tree(tree.boston,best=5)
plot(prune.boston)
text(prune.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,])
boston.test=Boston[-train,"medv"]
plot(yhat,boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# Bagging and Random Forests

library(randomForest)
library(MASS)
set.seed(101)
dim(Boston)
train = sample(1:nrow(Boston),300)

rf.boston = randomForest(medv ~ ., data = Boston, subset = train)
rf.boston

oob.err = double(13)
test.err = double(13)
for(mtry in 1:13) {
  fit = randomForest(medv ~ ., data = Boston, subset = train,
                     mtry = mtry, ntree = 400)
  oob.err[mtry] = fit$mse[400]
  pred = predict(fit, Boston[-train,])
  test.err[mtry] = with(Boston[-train,],mean((medv-pred)^2))
  cat(mtry," ")
}
matplot(1:mtry,cbind(test.err, oob.err), pch=19,
        col=c("red","blue"), type="b",
        ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c("red","blue"))


bag.boston=randomForest(medv ~ ., data=Boston,
                        subset=train,
                        mtry=13,
                        importance=TRUE)
bag.boston
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag-boston.test)^2)
bag.boston=randomForest(medv~.,data=Boston,subset=train,mtry=13,ntree=25)
yhat.bag = predict(bag.boston,newdata=Boston[-train,])
mean((yhat.bag-boston.test)^2)
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf = predict(rf.boston,newdata=Boston[-train,])
mean((yhat.rf-boston.test)^2)
importance(rf.boston)
varImpPlot(rf.boston)

# Boosting

library(gbm)
set.seed(1)
boost.boston=gbm(medv ~ .,
                 data=Boston[train,],
                 distribution="gaussian",
                 n.trees=10000,
                 shrinkage = 0.01,
                 interaction.depth=4)
summary(boost.boston)
par(mfrow=c(1,2))
plot(boost.boston,i="rm")
plot(boost.boston,i="lstat")

n.trees = seq(from=100, to=10000, by=100)
predmat = predict(boost.boston, newdata=Boston[-train,], n.trees = n.trees)
dim(predmat)
berr = with(Boston[-train,],apply( (predmat - medv)^2, 2, mean))
berr
par(mfrow=c(1,1))
plot(n.trees, berr, pch=19, ylab="Mean Squared Error",
     xlab = "# Trees", main="Boosting Test Error")
abline(h = min(test.err), col="red")


dyhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4,shrinkage=0.2,verbose=F)
yhat.boost=predict(boost.boston,newdata=Boston[-train,],n.trees=5000)
mean((yhat.boost-boston.test)^2)
