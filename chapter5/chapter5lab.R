### Simple Bootstrapping Example ###
library(psych)

# original sample
x = rnorm(1000)

# boostrap procedure to obtain sample means
sample_means = NULL
for(i in 1:1000) {
  bs_samp = sample(x,length(x),replace=T)
  sample_means = append(sample_means,mean(bs_samp))
}

# new samples from population
new_sample_means = NULL
for(i in 1:1000) {
  new_samp = rnorm(1000)
  new_sample_means = append(new_sample_means,mean(new_samp))
}

# describe x and sample mean distribution
par(mfrow=c(3,1), mar=c(2,2,2,2))
hist(x,breaks=30)
hist(sample_means,breaks=30)
hist(new_sample_means,breaks=30)
describe(x)
describe(sample_means)
describe(new_sample_means)
par(mfrow=c(1,1), mar=c(1,1,1,1))

library(ISLR)
library(boot)

?cv.glm
plot(mpg ~ horsepower, data=Auto)

## LOOCV leave one out CV
glm.fit = glm(mpg ~ horsepower, data=Auto)
summary(glm.fit)
plot(mpg ~ horsepower, data=Auto)
abline(glm.fit,col="blue")

# when K is not specified, LOOCV is used
cv_result = cv.glm(Auto, glm.fit)
cv_result$K
nrow(Auto)
cv_result$delta # CI for mean

# 10-fold CV
cv_result = cv.glm(Auto, glm.fit, K=10)
cv_result$K
cv_result$delta # CI for mean

# short cut function that works on linear regression
loocv = function(fit) {
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)

# now fit polynomial models of various degrees and
# do LOOCV
cv.error = rep(0,5)
degree=1:5
for(d in degree) {
  glm.fit = glm(mpg ~ poly(horsepower,d),data=Auto)
  cv.error[d] = loocv(glm.fit)
}
plot(degree, cv.error, type="b")

# now fit polynomial models of various degrees and
# do K-fold CV with K=10
cv.error10 = rep(0,5)
for(d in degree) {
  glm.fit = glm(mpg ~ poly(horsepower,d),data=Auto)
  cv.error10[d] = cv.glm(Auto,glm.fit,K=10)$delta
}
# add lines to previous plot
lines(degree, cv.error10, type="b",col="red")
