# bootstrap
# minimum risk investment section 5.2

alpha = function(x,y) {
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy - cxy)/(vx + vy - 2*cxy)
}

alpha(Portfolio$X, Portfolio$Y)

alpha.fn = function(data, index) {
  with(data[index,],alpha(X,Y))
}
alpha.fn(Portfolio,1:100)


# run a bootstrap
set.seed(1)
alpha.fn(Portfolio,sample(1:100,100,replace=TRUE))

boot.out = boot(Portfolio,alpha.fn,R=1000)
plot(boot.out)
boot.out
