#Lab 3 (chapter 4)

#Lab 
library(ISLR)
library(MASS)
library(class)
names(Smarket)
summary(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)
plot(y=Volume, x=Year)
boxplot(formula = Volume ~ Year, main="Volume per year")

# logistic regression 
glm.fit = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = "binomial")

summary(glm.fit)
summary(glm.fit)$coef

glm.prods = predict(glm.fit, type = "response")
contrasts(Direction)

glm.pred = rep("Down", nrow(Smarket))
glm.pred[glm.prods > 0.5] = "Up"

# cross tab for confusion matrix
conf = table(glm.pred, Direction)
precision = conf[1] / (sum(conf[1,]))
recall = conf[1] / (sum(conf[,1]))

# the above is not very indicative: 
# we predict the training observations 
train = Year < 2005
Smarket.2005 = Smarket[!train,]
Direction.2005 <- Direction[!train]
glm.fit = glm(formula = Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
              data = Smarket, family = "binomial", subset = train)

glm.probs = predict(glm.fit, Smarket.2005, type = "response")
glm.pred = rep("Down", 252)
glm.pred[glm.probs > 0.5] = "Up"
# confusion
table(glm.pred, Direction.2005)
# test error rate 
mean(glm.pred != Direction.2005)


#LDA 
lda.fit = lda(Direction ~ Lag1 + Lag2, data = Smarket, subset = train)
lda.pred = predict(lda.fit, Smarket.2005)
# lda automatically makes the classes 
lda.class = lda.pred$class
#qda is perfect analoog

#KNN 
train = Year < 2005
train.X = cbind(Lag1, Lag2)[train,]
test.X = cbind(Lag1, Lag2)[!train,]
train.Direction = Direction[train]

knn.pred = knn(train = train.X, test = test.X, cl = train.Direction, k = 3)

confusion = table(knn.pred, Direction.2005)
Precision = confusion[1,1] / sum(confusion[1,])

#Caravan

attach(Caravan)
standardized.X = scale(Caravan[,-86])

var(standardized.X[,1])

test = 1:1000
train.X = standardized.X[-test,] 
test.X = standardized.X[test,]
train.Y = Caravan[-test,86]
test.Y = Caravan[test,86]

set.seed(1)
knn.pred = knn(train.X, test.X, train.Y, k = 7)

knn.pred
confusion = table(knn.pred, test.Y)
mean(knn.pred != test.Y)

Precision = confusion[2,2] / sum(confusion[2,])


#writing functions 
Power <- function() {
  print(2^3);
}

Power3 <- function(x,a) {
  return(x^a);
}

PlotPower <- function(x, a) {
  x <- x
  y <- mapply(Power3, x, a)
  plot(x=x,y=y, xlab = "X", ylab = a, main="Exponential function X^2")
}