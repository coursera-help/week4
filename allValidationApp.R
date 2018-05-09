######################### Ridge Lasso ############################
library(glmnet)
library(MASS)
library(ISLR)
Hitters
View(Hitters)
dim(Hitters)
Hitters<-na.omit(Hitters)
x=model.matrix(Salary ~., Hitters)[, -1]
y=Hitters$Salary

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=-train
grid=10^seq(10,-2,length=100)

############## Ridge regression ##################
ridge.mod<-cv.glmnet(x[train,], y[train], alpha=0, lambda = grid)
coef(ridge.mod)
sqrt(sum(coef(ridge.mod)^2))

bestlambda=ridge.mod$lambda.min
ridge.pred<-predict(ridge.mod, newx = x[test,], s=bestlambda, type="response")
mean((ridge.pred-y[test])^2)
ridge.coeff<-predict(ridge.mod, newx = x[test,], s=bestlambda, type="coefficients")[1:20,]
ridge.coeff

################## Lasso regression ######################
lasso.mod=cv.glmnet(x[train,], y[train], alpha=1, lambda=grid)
bestlambda_lasso=lasso.mod$lambda.min
bestlambda_lasso

lasso.pred=predict(lasso.mod, newx=x[test,], s=bestlambda_lasso, type="response")
mean((lasso.pred-y[test])^2)

lasso.coeff=predict(lasso.mod, newx=x[test,], s=bestlambda_lasso, type="coefficients")[1:20,]
lasso.coeff



####################### Logistics ########################
library(MASS)
library(ISLR)
Smarket
market<-Smarket
head(market)
#market$Direction<-ifelse(market$Direction=="Up", 1, 0)

glm.fit<-glm(Direction~( Lag1+Lag2+Lag3+Lag4+Lag5+Volume), data=market, family="binomial")
glm.fit
summary(glm.fit)
coef(glm.fit)
contrasts(market$Direction)
summary(glm.fit)$coef

glm.pred<-predict(glm.fit, newdata = market, type="response")
table(glm.pred>0.5, market$Direction)

glm.p<-rep("Down", 1250)
glm.p[glm.pred>0.5]="Up"
conf<-table(glm.p, market$Direction)
accuracy=sum(diag(conf))/sum(conf)
accuracy


attach(smarket)
train_market=(market$Year<2005)
dim(market[-train,])
glm.fit_market1<-glm(Direction~(Lag1+Lag2+Lag3+Lag4+Lag5+Volume), data=market[train,], family="binomial")
glm.pred_market1<-predict(glm.fit_market1, newdata = market[-train,], type="response")
glm.prob_market=rep("Down", 623)
glm.prob_market[glm.pred_market1>0.5]="Up"
conf<-table(glm.prob_market, market[-train,]$Direction)
conf
accuracy=sum(diag(conf))/sum(conf)
accuracy


############## ROCR Curve #######
library(ROCR)
ROCRpred_market<-prediction(glm.pred_market1, market[-train,]$Direction)
ROCRPref_market<-performance(ROCRpred_market, "tpr", "fpr")
plot(ROCRPref_market, colorize=TRUE)
auc.tmp_market<-performance(ROCRpred_market, "auc")
auc_market<-as.numeric(auc.tmp_market@y.values)
auc_market

library(caret)
train_control=trainControl(method="cv", number=10)
model.market<-train(Direction~(Lag1+Lag2+Lag3+Lag4+Lag5+Volume), data=market, trControl=train_control, method="glm", family="binomial")
model.market

pred.market<-predict(model.market, newdata = market[-train,])

conf<-table(pred.market, market[-train,]$Direction)
accuracy.market<-sum(diag(conf))/sum(conf)
accuracy.market


########################## Subset Selection #####################
Hitters
library(leaps)
regsubset_full<-regsubsets(Salary~., Hitters)
summary(regsubset_full)
regsubset_full1<-regsubsets(Salary~., Hitters, nvmax=19)
reg_summary<-summary(regsubset_full1)
coef(regsubset_full1, which.min(summary(regsubset_full1)$cp))

####### Lowest RSS
plot(reg_summary$rss, xlab="Number of variables", ylab="RSS", type="l")

###### Max adjR2
which.max(reg_summary$adjr2)
plot(reg_summary$adjr2, xlab="Number of variables", ylab="adjR2", type="l")
points(11, reg_summary$adjr2[11], col="red", cex=2, pch=20)

####### Min Cp
which.min(reg_summary$cp)
plot(reg_summary$cp, xlab="Number of variables", ylab="Cp", type='l')
points(10, reg_summary$cp[10], col="red", cex=2, pch=20)

####### Min BIC
which.min(reg_summary$bic)
plot(reg_summary$bic, xlab="Number of variables", ylab="BIC", type="l")
points(6, reg_summary$bic[6], col="red", cex=2, pch=20)

names(regsubset_full1)
plot(regsubset_full1, scale="bic")
plot(regsubset_full1, scale="Cp")
plot(regsubset_full1, scale="adjr2")
plot(regsubset_full1, scale="r2")
coef(regsubset_full1, 6)


############ Forward Selection ##################
regsubset_forward<-regsubsets(Salary~., Hitters, nvmax=19, method="forward")
regfwd_summary<-summary(regsubset_forward)
which.min(regfwd_summary$cp)
plot(regfwd_summary$cp, xlab="Number of variables", ylab="Cp", type="l")
points(10, regfwd_summary$cp[10], col="red", cex=2, pch=20)
coef(regsubset_forward, which.min(regfwd_summary$cp))

################# Backward Selection #################
regsubset_backward<-regsubsets(Salary~., Hitters, nvmax=19, method="backward")
regbckwd_summary<-summary(regsubset_backward)
which.min(regbckwd_summary$bic)
plot(regbckwd_summary$bic, xlab="Number of variables", ylab="bic", type="l")
points(8, regbckwd_summary$bic[8], col="red", cex=2, pch=20)
coef(regsubset_backward, which.min(regbckwd_summary$bic))


################## Cross Validation ##################
### LOOCV
cat("\014")
View(Auto)
autodata<-Auto[1:8]
dim(autodata)
head(autodata)

library(boot)
reg.bwd_auto<-regsubsets(mpg~., autodata, nvmax=8, method="backward")
which.min(summary(reg.bwd_auto)$cp)
plot(summary(reg.bwd_auto)$cp, xlab="Number of variables", ylab="Cp", type="l")
points(6, summary(reg.bwd_auto)$cp[6], col="red", cex=2, pch=20)
coef(reg.bwd_auto, which.min(summary(reg.bwd_auto)$cp))

model.fit<-glm(mpg~(cylinders+displacement++weight+year+origin), data=autodata)
summary(model.fit)

mse_loocv<-cv.glm(autodata, model.fit)
mse_loocv$delta[1]

mse_loocv_delta=rep(NA, 10)
for(i in 1:10){
  model<-glm(mpg~poly(horsepower, i), data=Auto)
  mse_loocv_delta[i]<-cv.glm(Auto, model)$delta[1]
}
mse_loocv_delta


########## K-fold
library(caret)
train_control=trainControl(method="cv", number=10)
model<-train(mpg~horsepower, data=Auto, trControl=train_control, method="glm")
model
summary(model)
preds<-predict(model, Auto)
