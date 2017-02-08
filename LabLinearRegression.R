#Lab: Linear regression 
packages <- c("car", "ISLR", "MASS", "data.table", "abind")

lapply(packages, function(x) {
  if(!require(x)) {
    install.packages(x)
    #require(x, character.only = T)
  }
  require(x, character.only = T)
})



loadLibraries <- function() {
  library(ISLR)
  library(MASS)
  library(car)
  print("Libraries have been loaded.")
}

#Explore Boston data set 
?Boston
names(Boston)

#attach() Boston dataset to make selecting predictors easier
attach(Boston)

#fit a linear reg with lm()
lm.fit <- lm(medv ~ lstat)

#if you do not attach Boston, line below will also work 
lm.fit <- lm(medv ~ lstat, data=Boston)

#lm.fit only gives basic info 
#summary(lm.fit) is more detailed 
summary(lm.fit)

#to get a full list of informations stored in lm.fit, do names(lm.fit)
names(lm.fit)

#handigheid: Percentage error uitrekenen opbv RSE/mean(y)
# niet direct gevonden hoe ik RSE eruithaal dus:
res <- lm.fit$residuals

#RSE = square root of RSS/n-2
RSE <- sqrt(sum((res)^2)/(length(res)-2))

# perc error = RSE/mean(y)
percError <- RSE/mean(medv)
# seems to be about 27 percent deviance on average from estimated function

#obtain confidence intervals for coeff 
confint(lm.fit)

#predict values of medv for given valeues of lstat (w pred intervals)
predict(lm.fit, data.frame(lstat = c (5,10,15)),
        interval = "confidence")

predict(lm.fit, data.frame(lstat = c (5,10,15)),
        interval = "prediction")
# the prediction intervals are subst wider than the confidence intervals
# the higher the pecentage of lstat, the lower the med

#plot our linear regression 
plot(lstat, medv)
abline(lm.fit)
#abline(a,b) draws slope w intercept a and slope b
abline(lm.fit, lwd=3, col="red")
# extra parameters: pch: symbool, col: kleur en lwd: lijndikte

#allows for 4 plots in one screen 
par(mfrow=c(2,2))

#residual plot 
# y-as: residus 
# x-as: gefitte waarden
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

#leverage statistics 
plot(hatvalues(lm.fit))
#welke waarde heeft grootste leverage
which.max(hatvalues(lm.fit))

#MLR 

lm.fit <- lm(medv ~ lstat + age)
lm.fit
summary(lm.fit)

# do a regression on all vars 
#data par is necessary here because of . 
lm.fit <- lm(medv ~ ., data = Boston)
summary(lm.fit)

#R2 is given by 
summary(lm.fit)$r.sq
#RSE is given by 
summary(lm.fit)$sigma

#vif(), part of car package 
#made a mistake in chas variabe, better to omit for now
#, since vif() does not work
lm.fit <- lm(medv ~ .-chas, data = Boston)
#".-var" includes all features except var 

vif(lm.fit)

#omit age from fit 
lm.fit <- update(lm.fit, ~.-age)
# of nog, om zowel age als chas direct te omitten 
lm.fit <- lm(medv ~ .- age -chas, data=Boston)

# Interaction terms 

#below code includes lstat, age and lstat*age 
summary(lm(medv ~ age * lstat, data = Boston))

#onderstaande variatie werkt ook 
lm(formula = medv ~ . + lstat * age, data = Boston)

# Polynomial regression
poly.fit <- lm(medv ~ lstat + I(lstat^2))
lm.fit <- lm(medv ~ lstat)
#anova verschil 
anova(poly.fit, lm.fit)
#sign p waarde van f test in anova wijst op superioriteit polynomiale model
#zie lagere RSS voor model 1
# dit bevestigt onze vermoedens die we zagen in onderstaande plot 
# een niet lineaire relatie 
plot(lstat, medv)

# om nu te kijken hoe ver we kunnen verbeteren door hogere orde toe
# te voegen komt de poly() function van pas 
poly5.fit <- lm(medv ~ poly(lstat, 5))
# blijkbaar is tot de 2e macht nog significant, best eens dat model plotten
poly2.fit <- lm(medv ~ poly(lstat, 2))

# vergelijken met eerste macht poly via anova 
anova(poly.fit, poly2.fit)

# log ook mogelijk 
log.fit <- lm(medv ~ log(lstat))

#carseats dataset 
names(Carseats)
attach(Carseats)
lm.fit <- lm(Sales ~.+Income*Advertising+Price*Age, data=Carseats)
#automatic creation of dummy vars by r when confronted with categorical var 
contrasts(ShelveLoc)

#APPLIED EXERCISES

#1

lm.fit <- lm(mpg ~ horsepower, data=Auto)
summary(lm.fit)

#is there a relationship between the predictor and the response?
# on 5% sn we can reject the null H that every B = 0, F test p-value: 2.2e16

#how strong is the relationship between predictor and response
#R2 is 60,6% so we are able to explain 60 percent of the variance in y by regressing on x

#Relation between the respone pos or neg? 
# relation is negative given the negative B associated with horsepower
# thus a one unit increase in horsepower is associated with a 0.15 decrease in mpg

predict(lm.fit, data.frame(horsepower = c (98)),
        interval = "confidence")

predict(lm.fit, data.frame(horsepower = c (98)),
        interval = "prediction")

plot(horsepower,mpg)
abline(lm.fit, col=3, lwd=3)

# we can see a clear u shape in the residual plot. This could mean 
# we have non constant variance in our error terms.

plot(fitted.values(lm.fit),residuals(lm.fit))

#9

pairs(Auto)
#all correlations expect name categorical variable, which is the last variable 
cor(Auto[,-(length(Auto))])

lm.fit <- lm(mpg ~ .-name, data= Auto)
summary(lm.fit)

plot(fitted.values(lm.fit), residuals(lm.fit))
# heteroscedasticiteit 
# leverage 
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))
