# Chapter 3 Lab: Linear Regression

library(MASS)
library(ISLR)

# Simple Linear Regression

View(Boston)
names(Boston)
?Boston
plot(medv~lstat,Boston)
attach(Boston)
lm.fit=lm(medv~lstat)
lm.fit=lm(medv~lstat,data=Boston)
lm.fit
summary(lm.fit)
abline(lm.fit,col="red",lwd=3)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))), interval="prediction")
plot(lstat,medv)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:25,1:25,pch=1:25,cex=2)

# plot residuals, normal q-q plot, etc.
par(mfrow=c(2,2))
plot(lm.fit)
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

# Multiple Linear Regression

lm.fit=lm(medv~lstat+age,data=Boston)
summary(lm.fit)
fit3=lm(medv~.,data=Boston) # all the other variables are predictors
summary(fit3)
fit4 = update(fit3,~.-age-indus)
summary(fit4)

library(car)
vif(lm.fit)

# Interaction Terms
# main effects (lstat and age alone), are included
# because of heirarchy principle
summary(lm(medv~lstat*age,data=Boston))

# Non-linear Transformations of the Predictors
# protect quadratic term with I()
lm.fit2=lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
# plot points and fitted curve
par(mfrow=c(1,1))
plot(medv~lstat)
abline(lm.fit,col="darkgreen",lwd=3)
points(lstat,fitted(lm.fit2),col="red",pch=20)

# polynomial
lm.fit5=lm(medv~poly(lstat,5)) # 5th degree polynomial / overfitting
summary(lm.fit5)
points(lstat,fitted(lm.fit5),col="blue",pch=20)

summary(lm(medv~log(rm),data=Boston))

# Qualitative Predictors
View(Carseats)
names(Carseats)
summary(Carseats)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age,data=Carseats)
summary(lm.fit)
attach(Carseats)
contrasts(ShelveLoc) # how R will dummy code variable
pairs(Carseats)

# Writing Functions
LoadLibraries
LoadLibraries()
LoadLibraries=function(){
 library(ISLR)
 library(MASS)
 print("The libraries have been loaded.")
 }
LoadLibraries
LoadLibraries()

regplot=function(x,y,...) {
  fit=lm(y~x)
  plot(y~x,...)
  abline(fit,col="red",lwd=2)
}
regplot(Carseats$Price,Carseats$Sales)
regplot(Carseats$Price,Carseats$Sales,xlab="Price",ylab="Sales")
