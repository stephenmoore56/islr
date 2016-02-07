### vectors, data, matrices, subsetting
x=c(2,7,5)
x
y=seq(from=4,length=3,by=3)
?seq
y
x+y
x/y
x^y
x[2]
x[2:3]
# remove second element; minus element 2
x[-2]
x[-c(1,2)]
# matrix contents generated, then shaped
z=matrix(seq(1,12),4,3)
z
z[3:4,2:3]
z[,2:3]
# drop matrix status, convert to vector
z[,1]
# don't drop matrix
z[,1,drop=FALSE]
dim(z)
# list variables in environment
ls()
rm(y)
ls()

### Generating random data, graphics
x=runif(50)
y=rnorm(50)
hist(x)
hist(y)
plot(x,y)
plot(x, y, 
     xlab="Random Uniform",
     ylab="Random Normal",
     pch="*",
     col="blue")

# multiple plots
par(mfrow=c(2,1))
plot(x,y)
hist(y)
par(mfrow=c(1,1))

### Reading in data
Auto=read.csv("Auto.csv")
names(Auto)
dim(Auto)
class(Auto)
fix(Auto)
summary(Auto)
plot(Auto$cylinders,Auto$mpg)
# partial match on variable name!
plot(Auto$cyl,Auto$mpg)
attach(Auto)
search()
plot(cylinders,mpg)
# after conversion to factor, plot() draws
# a boxplot rather than scatterplot
cylinders=as.factor(cylinders)
plot(cylinders, mpg,
     xlab="Cylinders",
     ylab="Mpg",col="red")
pdf(file="../mpg.pdf")
plot(cylinders,mpg,xlab="Cylinders",ylab="Mpg",col="red")
dev.off()
View(Auto)
pairs(Auto,col="brown")
pairs(mpg~cylinders+acceleration+weight,Auto)
q()