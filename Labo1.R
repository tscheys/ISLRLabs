packages <- c("car", "ISLR", "MASS", "data.table", "abind")

lapply(packages, function(x) {
  if(!require(x)) {
    install.packages(x)
    }
  })

x <- c(1,4,5,6)
y <- c(3,4,5,4)

ls()

rm(list = ls())

ls()

x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2)

x <- matrix(data=c(1,2,3,4), nrow=2, ncol=2, byrow = T)

sqrt(x)

x^2

set.seed(1303)
x <- rnorm(5)
y <- x + rnorm(5,mean=50, sd=.1)
cor(x,y)

x <- rnorm(50)
mean(x)
var(x)
sqrt(var(x))
sd(x)

#plotting
x <- rnorm(50)
y <- rnorm(50)
plot(x,y)
plot(x,y, ylab="sales", xlab = "advertising")
plot(x,y, ylab="sales", xlab = "advertising", main="Sales vs advertising")
?plot
plot(x,y, ylab="sales", xlab = "advertising", main="Sales vs advertising", type="l")
pdf(figure.pdf)

# error, you need parens around file name 
Error in gsub("%%", "", s) : object 'figure.pdf' not found


pdf("figure.pdf")
plot(x,y, ylab="sales", xlab = "advertising", main="Sales vs advertising", type="l")
dev.off()

# plotting: contour, image, persp

x <- 1:10
y <- x
f <- outer(x,y, function(x,y) cos(y)/(1 + x^2))

contour(x,y,f)
contour(x,y,f, levels=45, add=T)

pdf("countour.pdf")
plot.new()
contour(x,y,f, nlevels=45, add=T)
dev.off()

fa <- (f-t(f))/2
contour(x,y,fa, nlevels=15)

image(x,y,fa, nlevels=15)
image(x,y,fa)
persp(x,y,fa)

# Loading data 

Auto <- read.table("Auto.data")

for(i in ncol(Auto)) {
  if(!is.factor(Auto[,i])) {
    print(names(Auto)[i])
    print(range(Auto[,i]))
  }
}

#APPLIED EXERCISES

#9 

#b get range 
attach(Auto)
ranges <- sapply(Auto[,1:8], range)
sapply(Auto[,1:8], mean)
sapply(Auto[,1:8], sd)
#variable 9 is een factor 

# remove observations 
Auto_removed_obs <- Auto[-(10:85),]

LowestHousePrices <- Boston[which.min(Boston$medv),]
predictorRanges <- sapply(Boston, range)
# to easily compare values of the suburb with lowest house prices 
# and the overall ranges of the predictors: do rbind 
rbind(predictorRanges, LowestHousePrices)
