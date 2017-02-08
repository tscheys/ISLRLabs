#LabCrossValidationBootstrap

#set.seed can be handy even in a business env. so we can reproduce 
#the results at a later time 

install.packages("boot")
require(boot)

# VALIDATION SET APPROACH 
set.seed(1)
#sample 196 obs from 392 obs
train = sample(392, 196)

lm.fit = lm(mpg ~ horsepower, data = Auto, subset = train) 
attach(Auto)

#MSE: vector mpg - predictions to get error 
# [-train] om enkel validation set te selecteren 
# ^2 to get squared error 
mean((mpg - predict(lm.fit, Auto))[-train]^2)

#Let us check if poly^2 or poly^3 lead to better results 

lm.fit2 = lm(mpg ~ poly(horsepower, 2), data = Auto, subset = train)
mean((mpg - predict(lm.fit2, Auto))[-train]^2)

lm.fit3 = lm(mpg ~ poly(horsepower, 3), data = Auto, subset = train)
mean((mpg - predict(lm.fit3, Auto))[-train]^2)

# LOOCV

glm.fit = glm(mpg ~ horsepower, data = Auto)
lm.fit = lm(mpg ~ horsepower, data = Auto)

#LOOCV 
cv.error = cv.glm(Auto, glm.fit)

#test error CV: 24.23 
cv.error$delta

# automation, we want to loop over increasing polynomials 
cv.error = rep(0,5)
for(i in 1:5) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.error[i] = cv.glm(Auto, glm.fit)$delta[1]
}

cv.error = seq(1,5)
#sapply geeft beter resultaat dan lapply, waarom?
sapply(cv.error, function(i) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  cv.glm(Auto, glm.fit)$delta[1]
})

# improvement by using quadratic fit, but little evidence that higher order 
#polynomials decrease MSE
cv.error


# k fold cross validation 
val.poly = seq(1,5)
sapply(val.poly, function(i) {
  glm.fit = glm(mpg ~ poly(horsepower, i), data = Auto)
  
  #analoog aan LOOCV, maar met K=10 parameter 
  cv.glm(Auto, glm.fit, K=10)$delta[1]
})

# k fold CV should be slower than LOOCV due to formula 5.2 
# k fold is faster in this case bc glm() does not use this formula
# in $delta, the first number is standard CV estimate, 
# the second number is bias corrected version 

# BOOTSTRAP 

alpha.fn = function(data, index) {
  # data is data.frame 
  # index is collection of indexes on which to apply the function 
  # vorm van index hier is c(1,4,6,7,8,53,53,....)
  X = data$X[index]
  Y = data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X) + var(Y) - 2*cov(X,Y)))
}

#standard calculation on all n 
alpha.fn(Portfolio, 1:100)


set.seed(1)
# bootstrap executes below command R times
alpha.fn(Portfolio, sample(100, 100, replace = T))

# simple bootstrap 
boot(Portfolio, alpha.fn, R = 1000)

#using bootstrap to estimate accuracy of lm 
boot.fn = function(data, index) {
  return(coef(lm(mpg ~ horsepower, data, subset = index)))
}

boot(Auto, boot.fn, R=1000)
#SE(hat B0) 0.87
#SE(hat B1) 0.007537

# APPLIED EXERCISES 

#5

#a fit a logistic model 
set.seed(100)

log.fit = glm(default ~ income + balance, data = Default, family = binomial)


errorRates <- rep(0,3)
for(i in 1:3) {
  train <- sample(nrow(Default), round(nrow(Default)/2))
  
  log.fit = glm(default ~ income + balance, data = Default, family = binomial, subset = train)
  #log.fit = glm(default ~ income + balance + student, data = Default, family = binomial, subset = train)
  #TEST
  #get probabilities and code them with correct class 
  predictions = ifelse(predict(log.fit, Default, type = "response")[-train] > 0.5, 0, 1)
  hits = abs(predictions - as.integer(Default$default[-train]))
  errorRates[i] = (sum(hits)/length(predictions))*100
}

#6
summary(log.fit)
#SE B1: 7.5*10^-6
#SE B2: 3.33*10^-4

#B
boot.fn = function(data, index) {
  return(coef(glm(default ~ income + balance, data = data, family = binomial, subset = index)))
}

boot(Default, boot.fn, R=1000)
#SE B1: 4.84*10^-6
#SE B2: 2.24*10^-4
#Bootstrap schat de SE's gevoelig kleiner 

#7
attach(Weekly)
error = rep(0, nrow(Weekly))
for(i in 1:nrow(Weekly)) {
  train = sample(nrow(Weekly))[-i]
  glm.fit = glm(Direction ~ Lag1+Lag2, family = binomial, data = Weekly, subset = train)
  
  predictions = ifelse(predict.glm(glm.fit, Weekly, type = "response")[i] > 0.5, 0, 1) + 1
  error[i] = abs(predictions - as.integer(Weekly$Direction[i]))
  #error[i] = (predict.glm(glm.fit, Weekly, type="response")[i] > 0.5) != Weekly$Direction[i]
}
sum(error)/length(error)
