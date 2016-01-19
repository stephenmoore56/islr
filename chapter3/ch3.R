library(MASS)
library(ISLR)

### Simple linear regression
names(Boston)
View(Boston)
?Boston
attach(Boston)
cor(medv,lstat)
pairs(Boston)
plot(medv~lstat,Boston)
fit1=lm(medv~lstat,data=Boston)
fit1
abline(fit1,col="red")
summary(fit1)
names(fit1)
confint(fit1)
predict(fit1,data.frame(lstat=c(5,10,15)),interval="confidence")

### Multiple linear regression
fit2=lm(medv~lstat+age,data=Boston)
summary(fit2)
fit3=lm(medv~.,Boston) # twiddle dot means all predictors
# note that age is no longer significant in the presence of all the other
# predictors
summary(fit3)
par(mfrow=c(2,2))
plot(fit3)
fit4=update(fit3,~.-age-indus)
summary(fit4)
anova(fit4)
### Nonlinear terms and Interactions
fit5=lm(medv~lstat*age,Boston)
summary(fit5)
fit5b=lm(medv~I(lstat*age),Boston)
summary(fit5b)
fit6 = lm(medv ~ lstat + I(lstat^2), Boston); summary(fit6)
summary(fit6)
plot(fit6)

attach(Boston)
par(mfrow=c(1,1))
plot(medv~lstat)
points(lstat,fitted(fit6),col="red",pch=20) # pch = Plotting CHarachter
fit7=lm(medv~poly(lstat,4))
points(lstat,fitted(fit7),col="blue",pch=20)
fit8=lm(medv~poly(lstat,3))
plot(medv~lstat)
points(lstat,fitted(fit8),col="purple",pch=20)
par(mfrow=c(2,2))
plot(fit8)

# show all the special plotting characters available 
plot(1:20,1:20,pch=1:20,cex=2)

###Qualitative predictors
View(Carseats)
names(Carseats)
summary(Carseats)
# all predictors and 2 interactions
fit1=lm(Sales~.+Income:Advertising+Age:Price,Carseats)
summary(fit1)
# show how R dummy coded a categorical variable
contrasts(Carseats$ShelveLoc)

### Writing R functions
regplot=function(x,y){
  fit=lm(y~x)
  plot(x,y)
  abline(fit,col="red")
}
attach(Carseats)
regplot(Price,Sales)
regplot=function(x,y,...){
  fit=lm(y~x)
  plot(x,y,...)
  abline(fit,col="red")
}
regplot(Price,Sales,xlab="Price",ylab="Sales",col="blue",pch=20)




