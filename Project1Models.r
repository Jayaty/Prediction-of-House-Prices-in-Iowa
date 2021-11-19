
# probably need to set your working directory as follows 
# setwd("C:\\Users\\Kai\\Dropbox\\NCSU\\ST516\\Projects\\P1\\ST_516_Project")


# Intialisation scripts for splitting dataset

#fullDataset = data.frame(read.csv("housingData.csv"))

#set.seed(0x5f3759df)

#fullDataset$LotShape = as.factor(fullDataset$LotShape)
#fullDataset$LotConfig = as.factor(fullDataset$LotConfig)
#fullDataset$Neighborhood = as.factor(fullDataset$Neighborhood)
#fullDataset$BldgType = as.factor(fullDataset$BldgType)
#fullDataset$HouseStyle = as.factor(fullDataset$HouseStyle)
#fullDataset$Exterior1st = as.factor(fullDataset$Exterior1st)
#fullDataset$Foundation = as.factor(fullDataset$Foundation)
#fullDataset$BsmtFinType1 = as.factor(fullDataset$BsmtFinType1)
#fullDataset$KitchenQual = as.factor(fullDataset$KitchenQual)
#fullDataset$GarageType = as.factor(fullDataset$GarageType)


#numRowsTotal = nrow(fullDataset)

#finalTestSize = as.integer(.2 * numRowsTotal)

#test=sample(1:numRowsTotal,finalTestSize,replace=F)

#trainData = fullDataset[-test,]

#testData = fullDataset[test,]

#saveRDS(trainData, "trainData.rds")
#saveRDS(testData, "testData.rds")

# Load in split datasets
trainData = readRDS("trainData.rds")
testData = readRDS("testData.rds")

str(trainData)
str(testData)


## Put your code for whatever models you're working on here ##
## Make sure to check out your individual branch first :)   ##

##################################
### Exploratory Data analysis ####
##################################

library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(pcr)
install.packages("ggplot2")
library(ggplot2)
library(gplots)
data_trial <- read.csv("~/Documents/ST_516_Stats/Project/ST_516_Project/trainData.csv")

describe(data_trial)

#### Plotting Nominal Variables
ggplot(data_trial, aes(x=BldgType, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Buliding Type", x="Building Type", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=LotShape, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Lot Shape", x="Lot Shape", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=LotConfig, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Lot Configuration", x="Lot configuration", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=Neighborhood, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Neighborhood", x="Neighborhood", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=HouseStyle, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs House Style", x="House Style", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=Exterior1st, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Exterior1st", x="Exterior1st", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=Foundation, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Foundation", x="Foundation", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=BsmtFinType1, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Basement Finishing Type", x="Basement Finishing Type", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=KitchenQual, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Kitchen Quality", x="Kitchen Quality", y="Sales Price in k") +
  geom_boxplot()

ggplot(data_trial, aes(x=GarageType, y= SalePrice/1000))+
  labs(title="Sale Price (in k) vs Garage Type", x="Garage Type", y="Sales Price in k") +
  geom_boxplot()

##### Plotting Discrete Variables

plotmeans( data_trial$SalePrice/1000 ~ data_trial$OverallQual, xlab = "Overall Quality", ylab = "Sales Price in k", main = "Sale Price (in k) vs Overall_quality")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$OverallCond, xlab = "Overall Condition", ylab = "Sales Price in k", main = "Sale Price (in k) vs OverallCond")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$YearBuilt,xlab = "YearBuilt", ylab = "Sales Price in k", main = "Sale Price (in k) vs YearBuilt")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$YearRemodAdd, xlab = "YearRemodAdd", ylab = "Sales Price in k", main = "Sale Price (in k) vs YearRemodAdd")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$FullBath,  xlab = "FullBath", ylab = "Sales Price in k", main = "Sale Price (in k) vs FullBath")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$HalfBath,  xlab = "HalfBath", ylab = "Sales Price in k", main = "Sale Price (in k) vs HalfBath")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$BedroomAbvGr,  xlab = "BedroomAbvGr", ylab = "Sales Price in k", main = "Sale Price (in k) vs BedroomAbvGr")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$TotRmsAbvGrd, xlab = "TotRmsAbvGrd", ylab = "Sales Price in k", main = "Sale Price (in k) vs TotRmsAbvGrd")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$Fireplaces,  xlab = "Fireplaces", ylab = "Sales Price in k", main = "Sale Price (in k) vs Fireplaces")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$GarageCars,  xlab = "GarageCars", ylab = "Sales Price in k", main = "Sale Price (in k) vs GarageCars")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$YrSold,  xlab = "YrSold", ylab = "Sales Price in k", main = "Sale Price (in k) vs YrSold")

###### Plotting Continuous Variables 

plotmeans( data_trial$SalePrice/1000 ~ data_trial$LotFrontage,  xlab = "LotFrontage", ylab = "Sales Price in k", main = "Sale Price (in k) vs Lot Frontage")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$LotArea,  xlab = "LotArea", ylab = "Sales Price in k", main = "Sale Price (in k) vs YrSold")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$TotalBsmtSF,  xlab = "TotalBsmtSF", ylab = "Sales Price in k", main = "Sale Price (in k) vs TotalBsmtSF")

plotmeans( data_trial$SalePrice/1000 ~ data_trial$GrLivArea,  xlab = "GrLivArea", ylab = "Sales Price in k", main = "Sale Price (in k) vs GrLivArea")



##################################
### Lasso Regression Models ######
##################################

rm(list = ls())

trainData = readRDS("trainData.rds")
testData = readRDS("testData.rds")

# create predictor matrix and vector for response
lasso.train <- readRDS("trainData.rds")
lasso.train <- na.roughfix(lasso.train) #eliminate NA observations

x <- model.matrix(SalePrice~.,lasso.train)
# remove the intercept from the matrix of variables for consideration
x <- x[,-1]
y <- lasso.train$SalePrice

# Use cross validation to estimate optimal lambda
# Create Grid for Lambda, fit model using all lambdas
grid <- 10^seq(10,-10,length=1000) # lambda ranges from 0.1 to 0.00001
lasso.mod <- glmnet(x,y,alpha = 1,lambda = grid)

# Plot coefficients as we change lambda
plot(lasso.mod, xlab="L2 Norm") #x-axis is in terms of sqrt(sum(beta^2))
abline(h=0,lty=3)

# estimate optimal lambda using cross-validation
cv.lasso <- cv.glmnet(x, y, alpha=1, lambda = grid)
plot(cv.lasso)
cv.lasso

# extract optimal and "1se" lambdas from model object
bestlam.l <- cv.lasso$lambda.min
mse.l <- min(cv.lasso$cvm)
paste("optimal lambda = ",bestlam.l)
paste("optimal mse = ",mse.l)
lam1se.l <- cv.lasso$lambda.1se
mse1se.l <- cv.lasso$cvm[which(cv.lasso$lambda==lam1se.l)]
paste("1se lambda = ",lam1se.l)
paste("1se mse = ",mse1se.l)

# get coefficients for best model and compare to OLS
# summary(lm.fit) - still need to complete comparison with team OLS model?
lasso.coeff <- predict(lasso.mod, type = "coefficients",s=bestlam.l)
lasso.coeff

# plot fitted values for OLS and Lasso, compare with actual - training
# once we have a favorite OLS model we will need to compare these
#fit.lasso <- predict(lasso.mod, s=bestlam.l,x)
#plot(lm.fit$fitted.values, lasso.train$SalePrice, pch=19, col="blue", main = "Training Lasso Model")
#points(fit.lasso,lasso.train$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Lasso"), pch=c(19,1), col=c("blue","red"),bty = "n")

# "Zoom In" on the large cluster - training
#plot(lm.fit$fitted.values, lasso.train$SalePrice, pch=19, col="blue",xlim = c(-100,15000),ylim=c(-100,15000), main = "Training Lasso Model - 'Zoomed In'")
#points(fit.lasso,lasso.train$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Lasso"), pch=c(19,1), col=c("blue","red"),bty = "n")

# Compare R2 - training - lasso vs OLS
#R2.lasso <- cor(fit.lasso,lasso.train$SalePrice)^2
#R2.lm <- cor(lm.fit$fitted.values,lasso.train$SalePrice)^2
#paste("R2 for OLS model = ",R2.lm)
#paste("R2 for Lasso model = ",R2.lasso)

# get coefficients for 1se model and compare to OLS
lasso.coef <- predict(lasso.mod,type="coefficients",s=lam1se.l)
lasso.coef
#summary(lm.fit)

# plot fitted values for OLS and Lasso, compare with actual
#fit.lasso.se <- predict(lasso.mod,s=lam1se.l,x)
#plot(lm.fit$fitted.values,lasso.train$SalePrice,pch=19,col="blue", main = "Training Lasso Model", sub = "1se")
#points(fit.lasso.se,lasso.train$SalePrice,col="red",lwd=2)
#abline(a=0,b=1)

# compare R2 for each fit
#R2.lasso.se <- cor(fit.lasso.se,lasso.train$SalePrice)^2
#R2.lasso.se
#R2.lm


# Determine Test Error for Lasso Model
lasso.test <- readRDS("testData.rds")
lasso.test <- na.roughfix(lasso.test) #eliminate NA observations

# plot fitted values for OLS and Ridge, compare with actual - test
x.val <- model.matrix(SalePrice~.,lasso.test)
# remove the intercept from the matrix of variables for consideration
x.val <- x.val[,-1]
#validation for 'best' lambda
fit.lasso.val.best <- predict(lasso.mod, s=bestlam.l,x.val)
# need favorite OLs for comparison
#plot(ols.pred, lasso.test$SalePrice, pch=19, col="blue", main = "Testing Lasso Model")
#points(fit.lasso.val,lasso.test$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Lasso"), pch=c(19,1), col=c("blue","red"),bty = "n")

#find the test error (MSE) of the Lasso model
# this uses the 'best' lambda
mse.l.best <- mean((lasso.test$SalePrice-fit.lasso.val.best)^2)
mse.l.best
rmse.l.best <- sqrt(mean((lasso.test$SalePrice-fit.lasso.val.best)^2))
rmse.l.best

#validation for '1se' lambda
fit.lasso.val.1se <- predict(lasso.mod, s=lam1se.l,x.val)
# need favorite OLs for comparison
#plot(ols.pred, lasso.test$SalePrice, pch=19, col="blue", main = "Testing Lasso Model")
#points(fit.lasso.val,lasso.test$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Lasso"), pch=c(19,1), col=c("blue","red"),bty = "n")

#find the test error (MSE) of the Lasso model
# this uses the 'best' lambda
mse.l.1se <- mean((lasso.test$SalePrice-fit.lasso.val.1se)^2)
mse.l.1se
rmse.l.1se <- sqrt(mean((lasso.test$SalePrice-fit.lasso.val.1se)^2))
rmse.l.1se


##################################
### Ridge Regression Models ######
##################################

rm(list = ls())

trainData = readRDS("trainData.rds")
testData = readRDS("testData.rds")

ridge.train <- readRDS("trainData.rds")
ridge.train <- na.roughfix(ridge.train) #eliminate NA observations

x <- model.matrix(SalePrice~.,ridge.train)
# remove the intercept from the matrix of variables for consideration
x <- x[,-1]
y <- ridge.train$SalePrice

# Use cross validation to estimate optimal lambda
# Create Grid for Lambda, fit model using all lambdas
grid <- 10^seq(10,-10,length=1000) # lambda ranges from 0.1 to 0.00001
ridge.mod <- glmnet(x,y,alpha = 0,lambda = grid)

# Plot coefficients as we change lambda
plot(ridge.mod, xlab="L2 Norm") #x-axis is in terms of sqrt(sum(beta^2))
abline(h=0,lty=3)

# estimate optimal lambda using cross-validation
cv.ridge <- cv.glmnet(x, y, alpha=0, lambda = grid)
plot(cv.ridge)
cv.ridge

# extract optimal and "1se" lambdas from model object
bestlam.r <- cv.ridge$lambda.min
mse.r <- min(cv.ridge$cvm)
paste("optimal lambda = ",bestlam.r)
paste("optimal mse = ",mse.r)
lam1se.r <- cv.ridge$lambda.1se
mse1se.r <- cv.ridge$cvm[which(cv.ridge$lambda==lam1se.r)]
paste("1se lambda = ",lam1se.r)
paste("1se mse = ",mse1se.r)

# get coefficients for best model and compare to OLS
# summary(lm.fit) - still need to complete comparison with team OLS model?
ridge.coeff <- predict(ridge.mod, type = "coefficients",s=bestlam.r)
ridge.coeff

# plot fitted values for OLS and Ridge, compare with actual - training
# once we have a favorite OLS model we will need to compare these
#fit.ridge <- predict(ridge.mod, s=bestlam.r,x)
#plot(lm.fit$fitted.values, college.train$Apps, pch=19, col="blue", main = "Training Ridge Model")
#points(fit.ridge,college.train$Apps,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Ridge"), pch=c(19,1), col=c("blue","red"),bty = "n")

# "Zoom In" on the large cluster - training
#plot(lm.fit$fitted.values, college.train$Apps, pch=19, col="blue",xlim = c(-100,15000),ylim=c(-100,15000), main = "Training Ridge Model - 'Zoomed In'")
#points(fit.ridge,college.train$Apps,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Ridge"), pch=c(19,1), col=c("blue","red"),bty = "n")

# Compare R2 - training - ridge vs OLS
#R2.ridge <- cor(fit.ridge,college.train$Apps)^2
#R2.lm <- cor(lm.fit$fitted.values,college.train$Apps)^2
#paste("R2 for OLS model = ",R2.lm)
#paste("R2 for Ridge model = ",R2.ridge)

# get coefficients for 1se model and compare to OLS
ridge.coef <- predict(ridge.mod,type="coefficients",s=lam1se.r)
ridge.coef
#summary(lm.fit)

# plot fitted values for OLS and Ridge, compare with actual
#fit.ridge.se <- predict(ridge.mod,s=lam1se.r,x)
#plot(lm.fit$fitted.values,college.train$Apps,pch=19,col="blue", main = "Training Ridge Model", sub = "1se")
#points(fit.ridge.se,college.train$Apps,col="red",lwd=2)
#abline(a=0,b=1)

# compare R2 for each fit
#R2.ridge.se <- cor(fit.ridge.se,college.train$Apps)^2
#R2.ridge.se
#R2.lm


# Determine Test Error for Ridge Model
ridge.test <- readRDS("testData.rds")
ridge.test <- na.roughfix(ridge.test) #eliminate NA observations

# plot fitted values for OLS and Ridge, compare with actual - test
x.val <- model.matrix(SalePrice~.,ridge.test)
# remove the intercept from the matrix of variables for consideration
x.val <- x.val[,-1]
#validation for 'best' lambda
fit.ridge.val.best <- predict(ridge.mod, s=bestlam.r,x.val)
# need favorite OLs for comparison
#plot(ols.pred, ridge.test$SalePrice, pch=19, col="blue", main = "Testing Ridge Model")
#points(fit.ridge.val,ridge.test$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Ridge"), pch=c(19,1), col=c("blue","red"),bty = "n")

#find the test error (MSE) of the Ridge model
# this uses the 'best' lambda
mse.r.best <- mean((ridge.test$SalePrice-fit.ridge.val.best)^2)
mse.r.best
rmse.r.best <- sqrt(mean((ridge.test$SalePrice-fit.ridge.val.best)^2))
rmse.r.best

#validation for '1se' lambda
fit.ridge.val.1se <- predict(ridge.mod, s=lam1se.r,x.val)
# need favorite OLs for comparison
#plot(ols.pred, ridge.test$SalePrice, pch=19, col="blue", main = "Testing Ridge Model")
#points(fit.ridge.val,ridge.test$SalePrice,col="red",lwd=2)
#abline(a=0, b=1)
#legend("topleft", legend = c("OLS", "Ridge"), pch=c(19,1), col=c("blue","red"),bty = "n")

#find the test error (MSE) of the Ridge model
# this uses the 'best' lambda
mse.r.1se <- mean((ridge.test$SalePrice-fit.ridge.val.1se)^2)
mse.r.1se
rmse.r.1se <- sqrt(mean((ridge.test$SalePrice-fit.ridge.val.1se)^2))
rmse.r.1se


##########################################
### Interaction/Higher Order Models ######
##########################################

rm(list = ls())
trainData = readRDS("trainData.rds")
testData = readRDS("testData.rds")

library(randomForest) # needed for na.roughfix()
library(leaps)
#### first order, simple multiple regression ####
data1 <- na.roughfix(trainData)
data1test <- na.roughfix(testData)
data1test <- data1test[data1test$Exterior1st!="AsphShn",]
num = length(data1) - 1  # modify me to change how many predictors we use, currently set to all 25
first_order_mods <- regsubsets(SalePrice ~ ., data = data1, nvmax = num, method = "forward")
# can use criterion-based methods to select best model
mods1 <- summary(first_order_mods)
names(mods1)
p = 1:(num + 1)
aic <- mods1$bic + 2 * num - log(dim(data1)[1]) * num
# plot to determine best model
plot.new()
par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(p, aic, pch = 19, type = "b", main = "AIC")
points(which.min(aic), aic[which.min(aic)], cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods1$bic, pch = 19, type = "b", main = "BIC")
points(which.min(mods1$bic), mods1$bic[which.min(mods1$bic)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods1$adjr2, pch = 19, type = "b", main = "adj-R2")
points(which.max(mods1$adjr2), mods1$adjr2[which.max(mods1$adjr2)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

fit <- lm(SalePrice~.,data=data1)
sm <- summary(fit)
mse <- mean(sm$residuals^2) # mean square error
mse
mspe <- mean((data1test$SalePrice - predict.lm(fit,data1test))^2) # mean square prediction error
mspe

## backward selection

first_order_mods2 <- regsubsets(SalePrice ~ ., data = data1, nvmax = num, method = "backward")
# can use criterion-based methods to select best model
mods2 <- summary(first_order_mods2)
names(mods2)
p = 1:(num + 1)
aic <- mods2$bic + 2 * num - log(dim(data1)[1]) * num
# plot to determine best model
plot.new()
par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(p, aic, pch = 19, type = "b", main = "AIC")
points(which.min(aic), aic[which.min(aic)], cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods2$bic, pch = 19, type = "b", main = "BIC")
points(which.min(mods2$bic), mods2$bic[which.min(mods2$bic)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods2$adjr2, pch = 19, type = "b", main = "adj-R2")
points(which.max(mods2$adjr2), mods2$adjr2[which.max(mods2$adjr2)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

## exhaustive selection not working

#### quadratics included on all relevant predictors (only on ratio data) ####
# all variables are also centered
data2 <- na.roughfix(trainData)
data2test <- na.roughfix(testData)
data2test <- data1test[data1test$Exterior1st!="AsphShn",]
mu <- mean(data2$LotFrontage)
data2$LotFrontage_sq <- (data2$LotFrontage-mu) * (data2$LotFrontage-mu)
mu <- mean(data2test$LotFrontage)
data2test$LotFrontage_sq <- (data2test$LotFrontage-mu) * (data2test$LotFrontage-mu)

# needs double precision
mu <- mean(data2$LotArea)
data2$LotArea_sq <- (as.double(data2$LotArea)-mu) * (as.double(data2$LotArea)-mu)
mu <- mean(data2test$LotArea)
data2test$LotArea_sq <- (as.double(data2test$LotArea)-mu) * (as.double(data2test$LotArea)-mu)

mu <- mean(data2$OverallQual)
data2$OverallQual_sq <- (data2$OverallQual-mu) * (data2$OverallQual-mu)
mu <- mean(data2test$OverallQual)
data2test$OverallQual_sq <- (data2test$OverallQual-mu) * (data2test$OverallQual-mu)

mu <- mean(data2$OverallCond)
data2$OverallCond_sq <- (data2$OverallCond-mu) * (data2$OverallCond-mu)
mu <- mean(data2test$OverallCond)
data2test$OverallCond_sq <- (data2test$OverallCond-mu) * (data2test$OverallCond-mu)

mu <- mean(data2$TotalBsmtSF)
data2$TotalBsmtSF_sq <- (data2$TotalBsmtSF-mu) * (data2$TotalBsmtSF-mu)
mu <- mean(data2test$TotalBsmtSF)
data2test$TotalBsmtSF_sq <- (data2test$TotalBsmtSF-mu) * (data2test$TotalBsmtSF-mu)

mu <- mean(data2$GrLivArea)
data2$GrLivArea_sq <- (data2$GrLivArea-mu) * (data2$GrLivArea-mu)
mu <- mean(data2test$GrLivArea)
data2test$GrLivArea_sq <- (data2test$GrLivArea-mu) * (data2test$GrLivArea-mu)

mu <- mean(data2$FullBath)
data2$FullBath_sq <- (data2$FullBath-mu) * (data2$FullBath-mu)
mu <- mean(data2test$FullBath)
data2test$FullBath_sq <- (data2test$FullBath-mu) * (data2test$FullBath-mu)

mu <- mean(data2$HalfBath)
data2$HalfBath_sq <- (data2$HalfBath-mu) * (data2$HalfBath-mu)
mu <- mean(data2test$HalfBath)
data2test$HalfBath_sq <- (data2test$HalfBath-mu) * (data2test$HalfBath-mu)

mu <- mean(data2$BedroomAbvGr)
data2$BedroomAbvGr_sq <- (data2$BedroomAbvGr-mu) * (data2$BedroomAbvGr-mu)
mu <- mean(data2test$BedroomAbvGr)
data2test$BedroomAbvGr_sq <- (data2test$BedroomAbvGr-mu) * (data2test$BedroomAbvGr-mu)

mu <- mean(data2$TotRmsAbvGrd)
data2$TotRmsAbvGrd_sq <- (data2$TotRmsAbvGrd-mu) * (data2$TotRmsAbvGrd-mu)
mu <- mean(data2test$TotRmsAbvGrd)
data2test$TotRmsAbvGrd_sq <- (data2test$TotRmsAbvGrd-mu) * (data2test$TotRmsAbvGrd-mu)

mu <- mean(data2$Fireplaces)
data2$Fireplaces_sq <- (data2$Fireplaces-mu) * (data2$Fireplaces-mu)
mu <- mean(data2test$Fireplaces)
data2test$Fireplaces_sq <- (data2test$Fireplaces-mu) * (data2test$Fireplaces-mu)

mu <- mean(data2$GarageCars)
data2$GarageCars_sq <- (data2$GarageCars-mu) * (data2$GarageCars-mu)
mu <- mean(data2test$GarageCars)
data2test$GarageCars_sq <- (data2test$GarageCars-mu) * (data2test$GarageCars-mu)

# repeat steps from first order model, but now for data2
num = length(data2) - 1  # modify me to change how many predictors we use, currently set to all 34
second_order_mods <- regsubsets(SalePrice ~ ., data = data2, nvmax = num, method = "forward")
# can use criterion-based methods to select best model
mods2 <- summary(second_order_mods)
p = 1:(num + 1)
aic2 <- mods2$bic + 2 * num - log(dim(data2)[1]) * num
# plot to determine best model
plot.new()
par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(p, aic2, pch = 19, type = "b", main = "AIC")
points(which.min(aic2), aic2[which.min(aic2)], cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods2$bic, pch = 19, type = "b", main = "BIC")
points(which.min(mods2$bic), mods2$bic[which.min(mods2$bic)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

plot(p, mods2$adjr2, pch = 19, type = "b", main = "adj-R2")
points(which.max(mods2$adjr2), mods2$adjr2[which.max(mods2$adjr2)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:num), lty = 3)

fit <- lm(SalePrice~.,data=data2)
sm <- summary(fit)
mse <- mean(sm$residuals^2) # mean square error
mse
mspe <- mean((data2test$SalePrice - predict.lm(fit,data2test))^2) # mean square prediction error
mspe

## Possible sources for interactions (not doing cubic because that would be a lot IMO)

# cross-validation
# set up for cross validation

pred.sbs <- function(obj,new,id){
  form <- as.formula(obj$call[[2]])
  mat <- model.matrix(form,new)
  coefi <- coef(obj,id=id)
  xvars <- names(coefi)
  return(mat[,xvars]%*%coefi)
}

k <- 10  # set number of folds
p <- 25 # number of predictor variables
RNGkind(sample.kind = "Rounding")
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds <- sample(1:k,nrow(data1),replace=T) # not sure about the dataset
# create dummy matrix to store CV error estimates
cv.err <- matrix(NA,k,p,dimnames=list(NULL,paste(1:p)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-9 predictors fit without kth fold
  best.mods <- regsubsets(SalePrice~.,data=data1[folds!=j,],
                          nvmax=p,method="forward") # getting error with "exhaustive", going with forward
  # estimate test error for all nine models by predicting kth fold 
  for (i in 1:p){
    pred <- pred.sbs(best.mods,data1[folds==j,],id=i)
    cv.err[j,i] <- mean((data1$SalePrice[folds==j]-pred)^2)  # save error est
  }
}
mse.cv <- apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min <- which.min(mse.cv)  # find minimum mean MSE
oneSE.cv <- apply(cv.err,2,sd) # compute standard error for each number of predictors
min1se <- mse.cv[min]+oneSE.cv[min]

# find 1se number of predictors
for(i in 1:p){
  if(mse.cv[i]>min1se){
    min.1se <- i+1
  }
}

# plot and put a red circle around lowest MSE, blue circle around 1se MSE
par(mfrow=c(1,1))
plot(1:25,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE",pch=19)
points(min,mse.cv[min],cex=2,col="red",lwd=2)
points(min.1se,mse.cv[min.1se],cex=2,col="blue",lwd=2)
abline(h=min1se,lty=2,col="blue") # plot 1se line

# cross-validation for second order model
# set up for cross validation

k <- 10  # set number of folds
p <- 34 # number of predictor variables
RNGkind(sample.kind = "Rounding")
set.seed(123)
# create an index with id 1-5 to assign observations to folds
folds <- sample(1:k,nrow(data2),replace=T) # not sure about the dataset
# create dummy matrix to store CV error estimates
cv.err <- matrix(NA,k,p,dimnames=list(NULL,paste(1:p)))

# perform CV
for (j in 1:k){
  # pick models with lowest RSS with 1-9 predictors fit without kth fold
  best.mods <- regsubsets(SalePrice~.,data=data2[folds!=j,],
                          nvmax=p,method="forward") # getting error with "exhaustive", going with forward
  # estimate test error for all nine models by predicting kth fold 
  for (i in 1:p){
    pred <- pred.sbs(best.mods,data2[folds==j,],id=i)
    cv.err[j,i] <- mean((data2$SalePrice[folds==j]-pred)^2)  # save error est
  }
}
mse.cv <- apply(cv.err,2,mean) # compute mean MSE for each number of predictors
min <- which.min(mse.cv)  # find minimum mean MSE
oneSE.cv <- apply(cv.err,2,sd) # compute standard error for each number of predictors
min1se <- mse.cv[min]+oneSE.cv[min]

# find 1se number of predictors
for(i in 1:p){
  if(mse.cv[i]>min1se){
    min.1se <- i+1
  }
}

# plot and put a red circle around lowest MSE, blue circle around 1se MSE
par(mfrow=c(1,1))
plot(1:34,mse.cv,type="b",xlab="no. of predictors)",ylab="est. test MSE",pch=19)
points(min,mse.cv[min],cex=2,col="red",lwd=2)
points(min.1se,mse.cv[min.1se],cex=2,col="blue",lwd=2)
abline(h=min1se,lty=2,col="blue") # plot 1se line

# since 18 predictors seems to be functionally equivalent to 34 (within 1se), use 18 (a)
a = 18
xvarm <- names(coef(best.mods, id = a))[2:(a+1)]
# needs to be manually edited, as some of the categorical dummy variables mess things up
# down to 15 predictors instead, see below
xvarm  <- c("LotArea",             "Neighborhood",        "BldgType",      
            "OverallQual",         "OverallCond",         "YearBuilt",
            "BsmtFinType1",        "GrLivArea",           "FullBath",
            "KitchenQual",         "GarageType",          "YrSold",
            "OverallQual_sq",      "OverallCond_sq",      "FullBath_sq")
data2.best <- data2[,c("SalePrice", xvarm)]
quadratic.best <- lm(SalePrice ~ ., data = data2.best)

# scatterplot of fit versus actual, for report
par(mfrow=c(1,2))
quadratic.best.test <- predict(quadratic.best, data2test)
plot(quadratic.best.test, data2test$SalePrice, pch = 1, 
     xlab="Predicted Value", ylab="Actual Value")
abline(a=0, b=1, lwd=1, col="blue")

# log transformation

data1$SalePrice <- log(data1$SalePrice)
data1test$SalePrice <- log(data1test$SalePrice)
num = 25  # modify me to change how many predictors we use, currently set to all 25
first_order_mods <- regsubsets(SalePrice ~ ., data = data1, nvmax = num, method = "forward")
# can use criterion-based methods to select best model
mods1 <- summary(first_order_mods)
names(mods1)
p = 1:(num + 1)
aic <- mods1$bic + 2 * num - log(dim(data1)[1]) * num
# plot to determine best model
plot.new()
par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(p, aic, pch = 19, type = "b", main = "AIC")
points(which.min(aic), aic[which.min(aic)], cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:9), lty = 3)

plot(p, mods1$bic, pch = 19, type = "b", main = "BIC")
points(which.min(mods1$bic), mods1$bic[which.min(mods1$bic)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:9), lty = 3)

plot(p, mods1$adjr2, pch = 19, type = "b", main = "adj-R2")
points(which.max(mods1$adjr2), mods1$adjr2[which.max(mods1$adjr2)],
       cex = 1.5, col = "red", lwd = 2)
abline(v = c(1:9), lty = 3)

fit <- lm(SalePrice~.,data=data1)
sm <- summary(fit)
mse <- mean(sm$residuals^2) # mean square error
mse
mspe <- mean((data1test$SalePrice - predict.lm(fit,data1test))^2) # mean square prediction error
mspe


##########################################
### Tree-based Models ####################
##########################################

{

  rm(list = ls())
  
  trainData = readRDS("trainData.rds")
  testData = readRDS("testData.rds")
  
  library(rpart)
  library(randomForest)
  library(caret)
  
  # credit to https://stackoverflow.com/a/56936983 for how to handle NAs: create factors (done above) then roughfix will use means, etc to handle it semi intelligently
  bagModel = randomForest(SalePrice~.,data=trainData, mtry=25, importance=T, na.action = na.roughfix, ntree=2000)
  
  
  plot(bagModel)
  # after about 300 trees things settle down pretty nicely
  
  varImpPlot(bagModel, type=1, pch=19)
  
  bagPrediction = data.frame(predict(bagModel, newdata=testData))
  
  colnames(bagPrediction) = "predictedValue"
  
  bagError = testData$SalePrice - bagPrediction$predictedValue
  
  bagErrorNoNA = bagError[!is.na(bagError)]
  
  bagMSE = mean(bagErrorNoNA^2) # with interpolated NA values: 602300604
  
  bagRMSE = sqrt(bagMSE) # with interpolated NA values: 24541
  
  
  
  
  # Random Forests
  control = trainControl(method="cv", number=5, search="grid")
  
  
  tunegrid = expand.grid(mtry=c(1:25))
  preproc = preProcess(trainData, method = "medianImpute")
  trainProc = predict(preproc, trainData)
  
  # the median impute got most of the NAs filled in, but not everything :thinking:
  trainProcReal = na.omit(trainProc)
  
  rfGridsearch = train(SalePrice~.,data=trainProcReal, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=control, preProcess="medianImpute")
  
  
  print(rfGridsearch)
  
  plot(rfGridsearch)
  
  # neither shows an issue with overfitting, model gets pretty much steadily better as we proceed
  # finally ends up at its best at 24
  
  rfModel = randomForest(SalePrice~.,data=trainProcReal, mtry=24, importance=T, na.action = na.roughfix, ntree=2000)
  
  ## Try again, this time with LOOCV
  LOOCVcontrol = trainControl(method="LOOCV", search="grid")
  LOOCVrfGridsearch = train(SalePrice~.,data=trainProcReal, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=LOOCVcontrol, preProcess="medianImpute")
  
  print(LOOCVrfGridsearch)
  plot(LOOCVrfGridsearch)
  
  # LOOCV says that the best thing to do is 25 features, 
  
  
  
  ### Skip _all_ NA values, try bagging & LOOCV-RFs again ###
  
  
  ## Bagging, no NA values in dataset ##
  trainDataNoNA = na.omit(trainData)
  
  bagModelNoNA = randomForest(SalePrice~.,data=trainData, mtry=25, importance=T, na.action = na.omit, ntree=2000)
  
  plot(bagModelNoNA)
  # after about 500 trees things settle down pretty nicely
  
  varImpPlot(bagModelNoNA, type=1, pch=19)
  
  bagPredictionNoNA = data.frame(predict(bagModelNoNA, newdata=testData))
  
  colnames(bagPredictionNoNA) = "predictedValue"
  
  bagErrorNoNA = testData$SalePrice - bagPredictionNoNA$predictedValue
  
  bagErrorNoNAFiltered = bagErrorNoNA[!is.na(bagErrorNoNA)]
  
  bagMSENoNA = mean(bagErrorNoNAFiltered^2) # 619520857
  
  bagRMSE = sqrt(bagMSENoNA) # 24890
  
  
  ## RFs, no NA values in dataset ##
  noNAControl = trainControl(method="cv", search="grid")
  tunegrid = expand.grid(mtry=c(1:25))
  LOOCVrfGridsearch = train(SalePrice~.,data=trainDataNoNA, method="rf", metric="RMSE", tuneGrid=tunegrid, trControl=noNAControl)
  
  print(LOOCVrfGridsearch)
  plot(LOOCVrfGridsearch)
  
  # works _slightly_ better with 24 features than 25
  # RMSE_24: 24463.42
  # RMSE_25: 24567.30
  
  # Build the model with 24 features to use for prediction:
  bagModelNoNA24 = randomForest(SalePrice~.,data=trainData, mtry=25, importance=T, na.action = na.roughfix, ntree=2000)
  bagPredictionNoNA24 = data.frame(predict(bagModelNoNA24, newdata=testData))
  
  colnames(bagPredictionNoNA24) = "predictedValue"
  
  bagErrorNoNA24 = testData$SalePrice - bagPredictionNoNA24$predictedValue
  
  bagErrorNoNAFiltered24 = bagErrorNoNA24[!is.na(bagErrorNoNA24)]
  
  bagMSENoNA24 = mean(bagErrorNoNAFiltered24^2) # 611835711
  
  bagRMSE24 = sqrt(bagMSENoNA24) # 24735
  
  
  ## boosting ##
  
  control = trainControl(method="cv", number=5, search="grid")
  tunegrid = expand.grid(n.trees=c(1000, 3000, 5000, 7000, 10000), interaction.depth=c(1,3,5, 7),shrinkage=c(0.005,0.01, .03, .05),n.minobsinnode=c(1,3,5))
  gb_gridsearch = train(SalePrice~.,data=trainDataNoNA, method="gbm", metric="RMSE",tuneGrid=tunegrid, trControl=control)
  print(gb_gridsearch)
  plot(gb_gridsearch)
  
  # tried originally with (100, 200, 300) trees, (1,3,5) depth, and (.001, .005, .01) shrinkage.  output said that optimal results were with 300, 5, .01, so expanded all
  
  # next tried with (100, 200, 300, 1000, 3000), (1,3,5, 7, 9, 13, 15), and (0.001,0.005,0.01, .03, .05, .09, .13) and got 3000, 3, .01.  
  
  # final run: 5000 trees, 3 interaction depth, .01 shrinkage, 3 minobsinnode
  
  require(gbm)
  
  boostModel = gbm(SalePrice~., data=trainDataNoNA, distribution="gaussian", n.trees = 5000, interaction.depth = 3, shrinkage = .01, n.minobsinnode = 3)
  
  boostPredictionNoNA = data.frame(predict(boostModel, newdata=testData))
  
  colnames(boostPredictionNoNA) = "predictedValue"
  
  boostErrorNoNA = testData$SalePrice - boostPredictionNoNA$predictedValue
  
  boostErrorFiltered = boostErrorNoNA[!is.na(boostErrorNoNA)]
  
  boostMSENoNA = mean(boostErrorFiltered^2) # 402713725
  
  boostRMSE = sqrt(boostMSENoNA) # 20068
  
  boostRMSE
  
  plot(boostPredictionNoNA$predictedValue,testData$SalePrice, xlab="", ylab="")
  abline(a=0, b=1, col="blue")
  
  mtext(side=1,line=2,"Predicted Value")
  mtext(side=2,line=3,"Actual Value")
  
}