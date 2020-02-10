# Model selection
# 1. Best subset selection
library(ISLR)
fix(Hitters)
attach(Hitters)

# Check for missing values
sum(is.na(Hitters))
Hitters = na.omit(Hitters)

names(Hitters)
install.packages('leaps')
library('leaps')

regfit.full = regsubsets(Salary ~., data=Hitters)
summary(regfit.full)
regfit.full = regsubsets(Salary ~., data=Hitters, nvmax=19)
summary(regfit.full)
reg.summary = summary(regfit.full)
names(reg.summary)

reg.summary$rsq
# Plotting the selection criteria
par(mfrow = c(2,2))
plot(reg.summary$rss, 
     xlab = 'Number of Variables',
     ylab = 'RSS',
     type = 'l')
rss.min = which.min(reg.summary$rss)
points(rss.min, reg.summary$rss[rss.min],
       col='red', cex=2, pch=20)

plot(reg.summary$adjr2, 
     xlab = 'Number of Variables',
     ylab = 'Adjusted RSq',
     type = 'l')
ar2.max = which.max(reg.summary$adjr2)
points(ar2.max, reg.summary$adjr2[ar2.max],
       col='red', cex=2, pch=20)

par(mfrow = c(2,2))
plot(reg.summary$rss, 
     xlab = 'Number of Variables',
     ylab = 'RSS',
     type = 'l')
rss.min = which.min(reg.summary$rss)
points(rss.min, reg.summary$rss[rss.min],
       col='red', cex=2, pch=20)

plot(reg.summary$cp, 
     xlab = 'Number of Variables',
     ylab = 'Cp',
     type = 'l')
cp.min = which.min(reg.summary$cp)
points(cp.min, reg.summary$cp[cp.min],
       col='red', cex=2, pch=20)

plot(reg.summary$bic, 
     xlab = 'Number of Variables',
     ylab = 'bic',
     type = 'l')
bic.min = which.min(reg.summary$bic)
points(bic.min, reg.summary$bic[bic.min],
       col='red', cex=2, pch=20)

#2. Ridge and Lasso
install.packages('glmnet')
library('glmnet')

#glmnet function requires the data to be 
#in matrix form
#Regressor matrix with dummy variables
x = model.matrix(Salary ~., Hitters)[,-1]
# Response variable
y = Hitters$Salary

#Ridge regression with alpha = 0
grid = 10^seq(10,-2, length=100)
ridge.mod = glmnet(x,y, alpha=0, lambda = grid)
# 100 models with 20 coefficients were fit
# depends in lambda
dim(coef(ridge.mod))
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
#Corresponding norm
sqrt(sum(coef(ridge.mod)[-1,50]^2))

# Plotting the coefficients in different 
# colors as functions of lambda
index = dim(coef(ridge.mod))
coef.ridge = matrix(nrow=20, ncol=100)
row.names(coef.ridge) = 
  row.names(coef(ridge.mod))

# Creating a matrix of estimated coefficients
# depending on lambda
for(i in 1:index[1]){
  for(lambda in 1:index[2]){
    coef.ridge[i, lambda] = 
      coef(ridge.mod)[,lambda][i]
  }
}

# Plotting the coefficients in different
# colors
plot(1:100, coef.ridge[2,], col=1, lwd = 1,
     ylim = c(min(coef.ridge[-1,]), 
              max(coef.ridge[-1,])),
     'l', ylab = 'Coefficients',
     xlab = 'Lambda:decreasing'
     )
for(i in 3:20){
  lines(coef.ridge[i,], col=i+10, lwd=1)
}
#Predictions
predict(ridge.mod, s=50, type = 'coefficients')

# Training vs. test set
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]

ridge.mod = glmnet(x[train,], y[train],
                   alpha=0, lambda=grid,
                   thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, 
                     newx = x[test,])
#Test MSE
mean((ridge.pred-y.test)^2)

# If you just fit the model with intercept
mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod, s=1e10,
                   newx=x[test,])
mean((ridge.pred-y.test)^2)

# No shrinkage (ols)
ridge.pred = predict(ridge.mod, s=0,
                     newx=x[test,])
mean((ridge.pred-y.test)^2) # test MSE

# Cross-validation
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], 
                   alpha=0)
plot(cv.out)
# Best value of lambda for the training set
bestlam = cv.out$lambda.min
bestlam

ridge.pred = predict(ridge.mod, 
                     s = bestlam,
                     newx = x[test, ])
mean((ridge.pred-y.test)^2) # test MSE

out = glmnet(x,y, alpha=0)
predict(out, type='coefficients', 
        s=bestlam)

# LASSO: same algorithm as for ridge, alpha=1

lasso.mod = glmnet(x[train,], y[train],
                   alpha = 1, lambda=grid)
plot(lasso.mod)

# Cross-validation
set.seed(1)
cv.out = cv.glmnet(x[train, ], y[train],
                   alpha = 1, lambda=grid)
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
lasso.pred = predict(lasso.mod, s=bestlam,
                     newx = x[test,])
mean((lasso.pred-y.test)^2)
out = glmnet(x,y, alpha=1, lambda=grid)
lasso.coef = predict(out, type='coefficients',
                     s=bestlam)[1:20,]
lasso.coef[lasso.coef!=0]
