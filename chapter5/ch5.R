require(ISLR)
require(boot)
?cv.glm
plot(mpg~horsepower,data=Auto)

## LOOCV
# glm fits linear model if you don't specify family
glm.fit=glm(mpg~horsepower, data=Auto)
abline(glm.fit, col="blue")
cv.glm(Auto,glm.fit)$delta #pretty slow (doesnt use formula (5.2) on page 180)
# delta is LOOCV error and then a bias corrected version of it
cv.glm(Auto,glm.fit)

##Lets write a simple function to use formula (5.2)
# takes advantage of the hat matrix produced by 
# a linear regression; 1/n < h < 1; this inflates or
# deflates residual according to the influence of the data point
loocv=function(fit){
  h=lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}

## Now we try it out; quick
loocv(glm.fit)

# there is some curvature in the data, so let's try
# a polynomial model
# let's do LOOCV for each degree
cv.error=rep(0,5)
degree=1:5
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error[d]=loocv(glm.fit)
}
plot(degree,cv.error,type="b")

## 10-fold CV; same range of degrees
cv.error10=rep(0,5)
for(d in degree){
  glm.fit=glm(mpg~poly(horsepower,d), data=Auto)
  cv.error10[d]=cv.glm(Auto,glm.fit,K=10)$delta[1]
}
lines(degree,cv.error10,type="b",col="red")


## Bootstrap
## Minimum risk investment - Section 5.2

alpha=function(x,y){
  vx=var(x)
  vy=var(y)
  cxy=cov(x,y)
  # return last line
  (vy-cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X,Portfolio$Y)
# [1] 0.5758321

## What is the standard error of alpha?

alpha.fn=function(data, index){
  with(data[index,],alpha(X,Y))
}

# test with the original dataset; should get same value
# for alpha that we got from call to alpha() function above
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn (Portfolio,sample(1:100,100,replace=TRUE))

boot.out=boot(Portfolio,alpha.fn,R=1000)
boot.out
plot(boot.out)

# quiz questions
load("5.R.RData")
str(Xy)
attach(Xy)
hist(X1, breaks=30)
hist(X2, breaks=30)
pairs(Xy)
Xy.fit = glm(y~.,data=Xy)
Xy.fit
summary(Xy.fit)
matplot(Xy,type="l")
coef(summary(Xy.fit))[2,2] # std error of Beta1
boot(Xy,coef(summary(Xy.fit))[2,2],R=1000)

betfun = function(data,b,formula){  
  # b is the random indexes for the bootstrap sample
  d = data[b,] 
  return(glm(d$y~., data = d)$coef[2])  
  # thats for the beta coefficient
}
# now you can bootstrap:
bootbet = boot(data=Xy, statistic=betfun, R=5000) 
bootbet
?tsboot
tsboot(tseries=Xy, statistic=betfun, R=5000, sim="fixed", l=100, n.sim=1000)
      