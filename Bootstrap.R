# Lab 5

# Load the data:
library(ISLR)

fix(Auto)
attach(Auto)

# Plot a scatter plot to see that relationship is non-linear:
plot(horsepower, mpg)

# Validation set approach:
set.seed(1)
train=sample(392,196)


lm.fit = lm(mpg ~ horsepower,data=Auto, subset=train)

mean((mpg - predict(lm.fit,Auto))[-train]^2)

lm.fit2 = lm(mpg~poly(horsepower,2),data=Auto,subset=train)
mean((mpg - predict(lm.fit2,Auto))[-train]^2)

lm.fit3 = lm(mpg~poly(horsepower,3),data=Auto,subset=train)
mean((mpg - predict(lm.fit3,Auto))[-train]^2)

set.seed(2) # . . . 


# LOOCV

glm.fit = glm(mpg ~ horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg ~ horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit = glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
  }

plot.ts(cv.error,lty=1,lwd=2)
plot(cv.error, type = "l")
which.min(cv.error)

# K-fold
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
  cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
cv.error.10

# write a loocv function: test poly degrees 1:5
n = length(horsepower)
LOOCV = rep(0,5)
sample = 1:n

for(p in 1:5){
  
  MSE = c()
  
  for(i in 1:n){
    glm.fit = glm(mpg ~ poly(horsepower,p), data=Auto, subset=sample[-i])
    pred.mpg = predict(glm.fit, newdata = Auto[i,])
    MSE[i] = (mpg[i] - pred.mpg)^2
  }
  LOOCV[p] = mean(MSE)
}
choice = which.min(LOOCV)
cat('The polynomial degree of the best model is ', choice)
plot.ts(LOOCV)

glm.fit=glm(mpg~poly(horsepower,choice),data=Auto)


# bootstrap
library(boot)

boot.fn = function(data,index){
  return(coef(lm(mpg ~ horsepower,data=data,subset=index)))
}

boot.fn(Auto,1:392)

set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))

boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower,data=Auto))$coef

boot.fn=function (data ,index){
   coefficients(lm(mpg ~ horsepower+I(horsepower^2),data=data, subset=index))
}
set.seed(1)
boot(Auto ,boot.fn,1000)

# DIY Bootstrap
B = 1000
coef.boot = matrix(ncol=3,nrow=B)

for(i in 1:B){
  sample = sample(392,392,replace=T)
  lm.fit = lm(mpg ~ horsepower+I(horsepower^2),data=Auto, subset=sample)
  coef.boot[i,] = coef(lm.fit)  
}
mean.boot = colMeans(coef.boot)
se.boot = rep(0,3)

for(j in 1:3){
  se.boot[j] = sqrt(1/(B-1)*sum((coef.boot[,j]-mean.boot[j])^2))
}
